use regex::Regex;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, PartialEq, Eq)]
pub struct ListType(pub String);

#[derive(Debug, PartialEq, Eq)]
pub struct ListIndent(pub String);

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'input> {
    Comment(&'input str),
    Footnote(String, String),
    ListItem(ListIndent, ListType, String),
    Literal(&'input str),
    Paragraph(String),
    Subject(String),
    Scissored(&'input str),
    Trailer(&'input str),
    VerticalSpace,
}

pub fn parse(input: &str, comment_char: char) -> Vec<Token> {
    let mut toks = Vec::new();

    let mut has_subject = false;
    let mut has_scissors = false;
    let lines = input.lines();
    let blank_or_empty = Regex::new(r"^\s*$").unwrap();
    let footnote = Regex::new(r"^\[[^]]+\]:? .+$").unwrap();
    let trailer = Regex::new(r"^\p{Alphabetic}[-\w]+: .+$").unwrap();
    let indented = Regex::new(r"^(?:\t| {4,})").unwrap();
    let list_item = Regex::new(
        r"(?x)
    ^(?P<indent>
    # Lists may be indented a little; too much and they become literals.
    \s{0,2}
    )
    (?P<li>
        (:?
            # We recognize unnumbered list markers...
            [-*]
            |
            (:?
                # ... and numbered list markers...
                \d+
                # ... followed by some delimiter observed in the wild...
                [.)\]:]
                |
                # ... or, alternatively, wrapped in parentheses...
                \(\d+\)
            )
        )
    # ... when the list item marker ends with at least one space.
    \s+)",
    )
    .unwrap();
    let mut px = false;
    for line in lines {
        if has_scissors {
            toks.push(Token::Scissored(line));
        } else if line.starts_with(comment_char) {
            let t = if &line[1..] == " ------------------------ >8 ------------------------" {
                has_scissors = true;
                Token::Scissored(line)
            } else {
                Token::Comment(line)
            };
            toks.push(t);
        } else if blank_or_empty.is_match(line) {
            if toks.last() != Some(&Token::VerticalSpace) {
                toks.push(Token::VerticalSpace);
            }
            px = true;
        } else if !has_subject {
            parse_subject(line, &mut toks);
            has_subject = true;
            px = false;
        } else if px && footnote.is_match(line) {
            debug_assert!(footnote.as_str().contains(' '));
            let mut splitter = line.splitn(2, ' ');
            let key = splitter.next().unwrap().to_owned();
            let rest = splitter.next().unwrap().trim().to_owned();
            toks.push(Token::Footnote(key, rest));
        } else if trailer.is_match(line) {
            toks.push(Token::Trailer(line));
        } else if let Some(y) = match toks.last_mut() {
            Some(&mut Token::Footnote(_, ref mut b)) => {
                b.push(' ');
                b.push_str(line.trim());
                None
            }
            Some(&mut Token::Paragraph(ref mut b)) => {
                b.push(' ');
                b.push_str(line.trim());
                None
            }
            Some(&mut Token::ListItem(_, _, ref mut b)) => {
                if list_item.is_match(line) {
                    Some(list_item_from_line(&list_item, line))
                } else {
                    b.push(' ');
                    b.push_str(line.trim());
                    None
                }
            }
            _ => {
                if list_item.is_match(line) {
                    Some(list_item_from_line(&list_item, line))
                } else if indented.is_match(line) {
                    Some(Token::Literal(line))
                } else {
                    px = false;
                    Some(Token::Paragraph(line.trim().to_owned()))
                }
            }
        } {
            toks.push(y);
        }
    }

    if toks.last() == Some(&Token::VerticalSpace) {
        let len = toks.len() - 1;
        toks.truncate(len);
    }

    toks
}

const SUBJECT_CHAR_LIMIT: usize = 90;

fn parse_subject(line: &str, toks: &mut Vec<Token>) {
    let line = line.trim_start();
    // If the subject has an autosquash pattern, return immediately. The
    // referenced commit may predate commitmsgfmt's rules so don't clean up the
    // end either.
    // Otherwise, if it fits, proceed.
    // If it's too long, break it at the first period.
    // If there is no period within the limit, just break at the limit.
    // We don't bother with spaces because at this point we can't break nicely.
    let is_autosquash = line.starts_with("fixup! ") || line.starts_with("squash! ");
    let (subject, rest) = if is_autosquash {
        (line, None)
    } else {
        let one_past_end = line.grapheme_indices(true).nth(SUBJECT_CHAR_LIMIT);
        let (subject, rest) = match one_past_end {
            None => (line, None),
            Some((limit, _)) => match line.find('.') {
                Some(period) if period < limit => (&line[..period], Some(&line[period + 1..])),
                Some(_) | None => {
                    let (s, r) = line.split_at(limit);
                    (s, Some(r))
                }
            },
        };

        let subject = subject.trim_end_matches(|c| c == '.' || char::is_whitespace(c));
        (subject, rest)
    };

    toks.push(Token::Subject(subject.to_owned()));
    toks.push(Token::VerticalSpace);

    if let Some(rest) = rest {
        let rest = rest.trim_start_matches('.');
        if !rest.is_empty() {
            toks.push(Token::Paragraph(rest.trim().to_owned()));
        }
    }
}

fn list_item_from_line<'a>(pat: &Regex, line: &str) -> Token<'a> {
    let captures = pat.captures(line).unwrap();
    let indent = captures.name("indent").unwrap();
    let li = captures.name("li").unwrap();
    Token::ListItem(
        ListIndent(indent.as_str().to_owned()),
        ListType(li.as_str().to_owned()),
        line[li.end()..].to_owned(),
    )
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    use pretty_assertions::assert_eq;

    fn parse(s: &str) -> Vec<Token> {
        super::parse(s, '#')
    }

    #[test]
    fn parses_empty_str() {
        assert!(parse("").is_empty());
    }

    #[test]
    fn parses_one_empty_line() {
        assert_eq!(parse("\n"), []);
    }

    #[test]
    fn parses_multiple_empty_lines() {
        assert_eq!(parse("\n\n"), []);
    }

    #[test]
    fn parses_one_blank_line() {
        assert_eq!(parse("   "), []);
    }

    #[test]
    fn parses_multiple_blank_lines() {
        assert_eq!(parse("   \n\t   \t"), []);
    }

    #[test]
    fn parses_default_comment() {
        assert_eq!(super::parse("# foo", '#'), [Comment("# foo")]);
    }

    #[test]
    fn parses_custom_comment() {
        assert_eq!(super::parse("@ foo", '@'), [Comment("@ foo")]);
        assert_eq!(super::parse("# foo", '@'), [Subject("# foo".to_owned())]);
    }

    #[test]
    fn parses_mixed_comment_and_content() {
        assert_eq!(
            parse("# foo\n\n # bar"),
            [Comment("# foo"), VerticalSpace, Subject("# bar".to_owned()),],
        );
    }

    #[test]
    fn parses_subject() {
        assert_eq!(parse("Hello, world"), [Subject("Hello, world".to_owned())]);
    }

    #[test]
    fn parses_subject_trimming_start() {
        assert_eq!(parse(" # foo"), [Subject("# foo".to_owned())]);
    }

    #[test]
    fn parses_subject_with_area() {
        let expected_subject = "foo: bar";

        assert_eq!(
            parse(expected_subject),
            [Subject(expected_subject.to_owned())]
        );
    }

    #[test]
    fn parses_fitting_subject() {
        let s = "f".repeat(SUBJECT_CHAR_LIMIT);
        assert_eq!(parse(&s), [Subject(s.to_owned())]);
    }

    #[test]
    fn parses_too_long_subject_as_subject_and_paragraph() {
        let s = "f".repeat(SUBJECT_CHAR_LIMIT) + "bar";
        assert_eq!(
            parse(&s),
            [
                Subject("f".repeat(SUBJECT_CHAR_LIMIT)),
                VerticalSpace,
                Paragraph("bar".to_owned()),
            ],
        );
    }

    #[test]
    fn parses_autosquash_subject_as_is() {
        let original_subject = "f".repeat(SUBJECT_CHAR_LIMIT);
        for autosquash_prefix in ["fixup", "squash"].iter() {
            // Include punctuation test case, in case punctuation slipped into original_subject.
            let autosquash_subject = format!(
                "{prefix}! {subject}..",
                prefix = autosquash_prefix,
                subject = original_subject
            );
            assert_eq!(
                parse(&autosquash_subject),
                [Subject(autosquash_subject.to_owned()),],
            );
        }
    }

    #[test]
    fn subject_strips_trailing_periods() {
        let expected_subject = "f".repeat(SUBJECT_CHAR_LIMIT - 1);

        let s = expected_subject.clone() + "....";

        assert_eq!(parse(&s), [Subject(expected_subject)]);
    }

    #[test]
    fn bug_subject_comprised_of_periods_becomes_empty() {
        let periods = "....";

        assert_eq!(parse(&periods), [Subject("".to_owned())]);
    }

    #[test]
    fn too_long_subject_breaks_at_first_period_within_subject_limit() {
        let expected_subject = "f".repeat(SUBJECT_CHAR_LIMIT - 10);

        let s = expected_subject.clone() + " . abc . def";

        assert_eq!(
            parse(&s),
            [
                Subject(expected_subject),
                VerticalSpace,
                Paragraph("abc . def".to_owned()),
            ],
        );
    }

    #[test]
    fn too_long_subject_breaks_at_limit_if_first_period_after_limit() {
        let expected_subject = "f".repeat(SUBJECT_CHAR_LIMIT);

        let s = expected_subject.clone() + ".abc.def";

        assert_eq!(
            parse(&s),
            [
                Subject(expected_subject),
                VerticalSpace,
                Paragraph("abc.def".to_owned()),
            ],
        );
    }

    #[test]
    fn extends_existing_paragraph_with_lines_instead_of_making_new() {
        assert_eq!(
            parse(
                "foo

this is
one paragraph

this is
# two
paragraphs
"
            ),
            [
                Subject("foo".to_owned()),
                VerticalSpace,
                Paragraph("this is one paragraph".to_owned()),
                VerticalSpace,
                Paragraph("this is".to_owned()),
                Comment("# two"),
                Paragraph("paragraphs".to_owned()),
            ]
        );
    }

    #[test]
    fn strips_paragraph_trailing_whitespace() {
        assert_eq!(
            parse(
                "foo

this paragraph has\t\t\t\t
trailing whitespace
"
            ),
            [
                Subject("foo".to_owned()),
                VerticalSpace,
                Paragraph("this paragraph has trailing whitespace".to_owned()),
            ]
        );
    }

    #[test]
    fn parses_literal_starting_with_4_spaces() {
        assert_eq!(
            parse(
                "
some subject

some paragraph

    some 4-space literal
      continuation

some other paragraph
    no literal without vertical space
"
            ),
            [
                VerticalSpace,
                Subject("some subject".to_owned()),
                VerticalSpace,
                Paragraph("some paragraph".to_owned()),
                VerticalSpace,
                Literal("    some 4-space literal"),
                Literal("      continuation"),
                VerticalSpace,
                Paragraph("some other paragraph no literal without vertical space".to_owned()),
            ],
        );
    }

    #[test]
    fn parses_literal_starting_with_1_tab() {
        assert_eq!(
            parse(
                "
some subject

some paragraph

\tsome 4-space literal
\t  continuation
\t\tcontinuation

some other paragraph
\tno literal without vertical space
"
            ),
            [
                VerticalSpace,
                Subject("some subject".to_owned()),
                VerticalSpace,
                Paragraph("some paragraph".to_owned()),
                VerticalSpace,
                Literal("\tsome 4-space literal"),
                Literal("\t  continuation"),
                Literal("\t\tcontinuation"),
                VerticalSpace,
                Paragraph("some other paragraph no literal without vertical space".to_owned()),
            ],
        );
    }

    #[test]
    fn parses_multiline_literal_as_separate_literals() {
        // A clean implementation would recognize this as a single logical block
        // and ensure consistent indentation. That's very difficult to do
        // because subsequent lines could be continuations needing further
        // indentation, and then we need to resolve indentation levels. We could
        // trivially achieve that by making continuation indentation be some
        // multiple of the literal-defining indentation but at that point we're
        // making so many assumptions we might just be better off relying on the
        // raw input.
        assert_eq!(
            parse(
                "
some subject

some paragraph

    some 4-space literal
    some 4-space literal
    some 4-space literal

some other paragraph
"
            ),
            [
                VerticalSpace,
                Subject("some subject".to_owned()),
                VerticalSpace,
                Paragraph("some paragraph".to_owned()),
                VerticalSpace,
                Literal("    some 4-space literal"),
                Literal("    some 4-space literal"),
                Literal("    some 4-space literal"),
                VerticalSpace,
                Paragraph("some other paragraph".to_owned()),
            ],
        );
    }

    #[test]
    fn parses_trailers() {
        // Trailers look like HTTP or email headers but are not formally
        // specified. They usually appear at the end of the message but can
        // appear other places. git-interpret-trailers(1) tries to identify
        // trailers in a file.
        //
        // Until somebody complains we assume the default colon separator and
        // make no restrictions on placement or duplication.
        assert_eq!(
            parse(
                "
subject

Fixes: All the things
Cc: John Doe <john@doe.com>
Reviewed-by: NSA
Signed-off-by: Jane Doe <jane@doe.com>
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Trailer("Fixes: All the things"),
                Trailer("Cc: John Doe <john@doe.com>"),
                Trailer("Reviewed-by: NSA"),
                Trailer("Signed-off-by: Jane Doe <jane@doe.com>"),
            ],
        );
    }

    #[test]
    fn footnotes_are_left_bracket_ident_right_bracket_space_text_distanced_from_paragraph() {
        assert_eq!(
            parse(
                "
subject

[1] footnote
[fo-ot] note
[ä] multi-code-point footnote key

[@]:    footnote

[] not a footnote

[1]not a footnote

[1] 
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Footnote("[1]".to_owned(), "footnote".to_owned()),
                Footnote("[fo-ot]".to_owned(), "note".to_owned()),
                Footnote("[ä]".to_owned(), "multi-code-point footnote key".to_owned()),
                VerticalSpace,
                Footnote("[@]:".to_owned(), "footnote".to_owned()),
                VerticalSpace,
                Paragraph("[] not a footnote".to_owned()),
                VerticalSpace,
                Paragraph("[1]not a footnote".to_owned()),
                VerticalSpace,
                Paragraph("[1]".to_owned()),
            ],
        );
    }

    #[test]
    fn footnote_cannot_follow_paragraph_immediately() {
        assert_eq!(
            parse(
                "
subject

paragraph 1
[1] not footnote 1
[2] not footnote 2

paragraph 2

[1] footnote 1
[2] footnote 2
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Paragraph("paragraph 1 [1] not footnote 1 [2] not footnote 2".to_owned()),
                VerticalSpace,
                Paragraph("paragraph 2".to_owned()),
                VerticalSpace,
                Footnote("[1]".to_owned(), "footnote 1".to_owned()),
                Footnote("[2]".to_owned(), "footnote 2".to_owned()),
            ],
        );
    }

    #[test]
    fn footnote_order_is_unchanged() {
        // Naive solution is technically trivial but may not match semantics.
        assert_eq!(
            parse(
                "
subject

[2] bar
[b] a
[a] b
[1] foo
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Footnote("[2]".to_owned(), "bar".to_owned()),
                Footnote("[b]".to_owned(), "a".to_owned()),
                Footnote("[a]".to_owned(), "b".to_owned()),
                Footnote("[1]".to_owned(), "foo".to_owned()),
            ],
        );
    }

    #[test]
    fn footnotes_may_span_multiple_lines() {
        assert_eq!(
            parse(
                "
subject

[1] foo
bar
[2] foo
    bar
[3] foo
        bar
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Footnote("[1]".to_owned(), "foo bar".to_owned()),
                Footnote("[2]".to_owned(), "foo bar".to_owned()),
                Footnote("[3]".to_owned(), "foo bar".to_owned()),
            ],
        );
    }

    #[test]
    fn bug_footnote_idents_are_not_disambiguated() {
        // Nice-to-have but not really our job. Requires tracking all footnotes.
        assert_eq!(
            parse(
                "
subject

[1] foo
[1] bar
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Footnote("[1]".to_owned(), "foo".to_owned()),
                Footnote("[1]".to_owned(), "bar".to_owned()),
            ],
        );
    }

    #[test]
    fn parses_lists() {
        assert_eq!(
            parse(
                "
foo

- list item
- wrapped list
  item
- over-indented
        continuation
- under-indented
continuation

paragraph

 - indented list
 item

  - indented list
  item

   - paragraph

- spaced list item
"
            ),
            [
                VerticalSpace,
                Subject("foo".to_owned()),
                VerticalSpace,
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("- ".to_owned()),
                    "list item".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("- ".to_owned()),
                    "wrapped list item".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("- ".to_owned()),
                    "over-indented continuation".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("- ".to_owned()),
                    "under-indented continuation".to_owned()
                ),
                VerticalSpace,
                Paragraph("paragraph".to_owned()),
                VerticalSpace,
                ListItem(
                    ListIndent(" ".to_owned()),
                    ListType("- ".to_owned()),
                    "indented list item".to_owned()
                ),
                VerticalSpace,
                ListItem(
                    ListIndent("  ".to_owned()),
                    ListType("- ".to_owned()),
                    "indented list item".to_owned()
                ),
                VerticalSpace,
                Paragraph("- paragraph".to_owned()),
                VerticalSpace,
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("- ".to_owned()),
                    "spaced list item".to_owned()
                ),
            ],
        );
    }

    #[test]
    fn parses_different_list_types() {
        // - Numbered list items require a delimiter, else we generate too many
        //   false-positives.
        // - All these variants have been observed across the Git and Linux
        //   repositories
        // - Also a few others, such as " [1] ", but those are rare and cause
        //   ambiguities not worth dealing with.
        assert_eq!(
            parse(
                "
foo

- dash
* bullet
1. numbered
0. unordered numbered
2) numbered
3] numbered
4: numbered
50) multi-digit numbered
(1) parenthesised list item
 (10) parenthesised list item
  (100) parenthesised list item

6\tnot a list item

7 not a list item

-) not a list item
"
            ),
            [
                VerticalSpace,
                Subject("foo".to_owned()),
                VerticalSpace,
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("- ".to_owned()),
                    "dash".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("* ".to_owned()),
                    "bullet".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("1. ".to_owned()),
                    "numbered".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("0. ".to_owned()),
                    "unordered numbered".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("2) ".to_owned()),
                    "numbered".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("3] ".to_owned()),
                    "numbered".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("4: ".to_owned()),
                    "numbered".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("50) ".to_owned()),
                    "multi-digit numbered".to_owned()
                ),
                ListItem(
                    ListIndent("".to_owned()),
                    ListType("(1) ".to_owned()),
                    "parenthesised list item".to_owned()
                ),
                ListItem(
                    ListIndent(" ".to_owned()),
                    ListType("(10) ".to_owned()),
                    "parenthesised list item".to_owned()
                ),
                ListItem(
                    ListIndent("  ".to_owned()),
                    ListType("(100) ".to_owned()),
                    "parenthesised list item".to_owned()
                ),
                VerticalSpace,
                Paragraph("6\tnot a list item".to_owned()),
                VerticalSpace,
                Paragraph("7 not a list item".to_owned()),
                VerticalSpace,
                Paragraph("-) not a list item".to_owned()),
            ],
        );
    }

    #[test]
    fn parses_scissored_content() {
        assert_eq!(
            parse(
                "
subject

format
this

# ------------------------ >8 ------------------------
do
 not
  format
 this
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Paragraph("format this".to_owned()),
                VerticalSpace,
                Scissored("# ------------------------ >8 ------------------------"),
                Scissored("do"),
                Scissored(" not"),
                Scissored("  format"),
                Scissored(" this"),
            ],
        );
    }

    #[test]
    fn parses_scissored_content_with_custom_comment_char() {
        assert_eq!(
            super::parse(
                "
subject

# ------------------------ >8 ------------------------
above is not a comment;
do the needful

$ ------------------------ >8 ------------------------
do
 not
  format
", '$'
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Paragraph("# ------------------------ >8 ------------------------ above is not a comment; do the needful".to_owned()),
                VerticalSpace,
                Scissored("$ ------------------------ >8 ------------------------"),
                Scissored("do"),
                Scissored(" not"),
                Scissored("  format"),
            ],
        );
    }
}
