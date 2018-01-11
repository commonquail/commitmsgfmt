use regex::Regex;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, PartialEq)]
pub struct ListType(pub String);

#[derive(Debug, PartialEq)]
pub enum Token {
    Comment(String),
    Reference(String),
    ListItem(ListType, String),
    Literal(String),
    Paragraph(String),
    Subject(String),
    Trailer(String),
    VerticalSpace,
}

pub fn parse(input: &str) -> Vec<Token> {
    let mut toks = Vec::new();

    let mut has_subject = false;
    let lines = input.lines();
    let blank_or_empty = Regex::new(r"^\s*$").unwrap();
    let reference = Regex::new(r"^\[[^]]+\]:? .+$").unwrap();
    let trailer = Regex::new(r"^\p{Alphabetic}[-\w]+: .+$").unwrap();
    let indented = Regex::new(r"^(?:\t| {4,})").unwrap();
    let list_item = Regex::new(
        r"(?x)
    ^(
    # Lists may be indented a little; too much and they become literals.
    \s{0,2}
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
    ).unwrap();
    for line in lines {
        if line.starts_with('#') {
            toks.push(Token::Comment(line.to_owned()));
        } else if blank_or_empty.is_match(line) {
            if toks.last() != Some(&Token::VerticalSpace) {
                toks.push(Token::VerticalSpace);
            }
        } else if !has_subject {
            parse_subject(line, &mut toks);
            has_subject = true;
        } else if reference.is_match(line) {
            toks.push(Token::Reference(line.to_owned()));
        } else if trailer.is_match(line) {
            toks.push(Token::Trailer(line.to_owned()));
        } else {
            match toks.last_mut() {
                Some(&mut Token::Paragraph(ref mut b)) => {
                    b.push(' ');
                    b.push_str(line.trim());
                    None
                }
                Some(&mut Token::ListItem(_, ref mut b)) => {
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
                        let mut raw = line.to_owned();
                        raw.push('\n'); // Recover linefeed lost from iterator.
                        Some(Token::Literal(raw))
                    } else {
                        Some(Token::Paragraph(line.trim().to_owned()))
                    }
                }
            }.map(|y| toks.push(y));
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
    let line = line.trim_left();
    // If the subject fits, return immediately.
    // If it's too long, break it at the first period.
    // If there is no period within the limit, just break at the limit.
    // We don't bother with spaces because at this point we can't break nicely.
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

    let subject = subject.trim_right_matches(|c| c == '.' || char::is_whitespace(c));
    toks.push(Token::Subject(subject.to_owned()));
    toks.push(Token::VerticalSpace);

    if let Some(rest) = rest {
        let rest = rest.trim_left_matches('.');
        if !rest.is_empty() {
            toks.push(Token::Paragraph(rest.trim().to_owned()));
        }
    }
}

fn list_item_from_line(pat: &Regex, line: &str) -> Token {
    let t = pat.captures(line).unwrap().get(1).unwrap().as_str();
    Token::ListItem(ListType(t.to_owned()), line[t.len()..].to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Token::*;

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
    fn parses_comment() {
        assert_eq!(parse("# foo"), [Comment("# foo".to_owned())]);
    }

    #[test]
    fn parses_mixed_comment_and_content() {
        assert_eq!(
            parse("# foo\n\n # bar"),
            [
                Comment("# foo".to_owned()),
                VerticalSpace,
                Subject("# bar".to_owned()),
            ],
        );
    }

    #[test]
    fn parses_subject() {
        assert_eq!(parse("Hello, world"), [Subject("Hello, world".to_owned())]);
    }

    #[test]
    fn parses_subject_trimming_left() {
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
        assert_eq!(parse(&s), [Subject(s)]);
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
                Comment("# two".to_owned()),
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
                Literal("    some 4-space literal\n".to_owned()),
                Literal("      continuation\n".to_owned()),
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
                Literal("\tsome 4-space literal\n".to_owned()),
                Literal("\t  continuation\n".to_owned()),
                Literal("\t\tcontinuation\n".to_owned()),
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
                Literal("    some 4-space literal\n".to_owned()),
                Literal("    some 4-space literal\n".to_owned()),
                Literal("    some 4-space literal\n".to_owned()),
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
                Trailer("Fixes: All the things".to_owned()),
                Trailer("Cc: John Doe <john@doe.com>".to_owned()),
                Trailer("Reviewed-by: NSA".to_owned()),
                Trailer("Signed-off-by: Jane Doe <jane@doe.com>".to_owned()),
            ],
        );
    }

    #[test]
    fn references_are_left_bracket_ident_right_bracket_space_text() {
        assert_eq!(
            parse(
                "
subject

[1] reference
[re-fe] rence

[@]:    reference

[] not a reference

[1]not a reference

[1] 
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Reference("[1] reference".to_owned()),
                Reference("[re-fe] rence".to_owned()),
                VerticalSpace,
                Reference("[@]:    reference".to_owned()),
                VerticalSpace,
                Paragraph("[] not a reference".to_owned()),
                VerticalSpace,
                Paragraph("[1]not a reference".to_owned()),
                VerticalSpace,
                Paragraph("[1]".to_owned()),
            ],
        );
    }

    #[test]
    fn reference_order_is_unchanged() {
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
                Reference("[2] bar".to_owned()),
                Reference("[b] a".to_owned()),
                Reference("[a] b".to_owned()),
                Reference("[1] foo".to_owned()),
            ],
        );
    }

    #[test]
    fn bug_references_are_single_line_only() {
        // Some references are prose and should be treated accordingly.
        // Regrettably this clashes with references that should not wrap. Maybe
        // fixable by identifying unwrappable words instead of unwrappable
        // lines.
        assert_eq!(
            parse(
                "
subject

[1] foo
bar
"
            ),
            [
                VerticalSpace,
                Subject("subject".to_owned()),
                VerticalSpace,
                Reference("[1] foo".to_owned()),
                Paragraph("bar".to_owned()),
            ],
        );
    }

    #[test]
    fn bug_reference_idents_are_not_disambiguated() {
        // Nice-to-have but not really our job. Requires tracking all references
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
                Reference("[1] foo".to_owned()),
                Reference("[1] bar".to_owned()),
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
                ListItem(ListType("- ".to_owned()), "list item".to_owned()),
                ListItem(ListType("- ".to_owned()), "wrapped list item".to_owned()),
                ListItem(
                    ListType("- ".to_owned()),
                    "over-indented continuation".to_owned()
                ),
                ListItem(
                    ListType("- ".to_owned()),
                    "under-indented continuation".to_owned()
                ),
                VerticalSpace,
                Paragraph("paragraph".to_owned()),
                VerticalSpace,
                ListItem(ListType(" - ".to_owned()), "indented list item".to_owned()),
                VerticalSpace,
                ListItem(ListType("  - ".to_owned()), "indented list item".to_owned()),
                VerticalSpace,
                Paragraph("- paragraph".to_owned()),
                VerticalSpace,
                ListItem(ListType("- ".to_owned()), "spaced list item".to_owned()),
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
                ListItem(ListType("- ".to_owned()), "dash".to_owned()),
                ListItem(ListType("* ".to_owned()), "bullet".to_owned()),
                ListItem(ListType("1. ".to_owned()), "numbered".to_owned()),
                ListItem(ListType("0. ".to_owned()), "unordered numbered".to_owned()),
                ListItem(ListType("2) ".to_owned()), "numbered".to_owned()),
                ListItem(ListType("3] ".to_owned()), "numbered".to_owned()),
                ListItem(ListType("4: ".to_owned()), "numbered".to_owned()),
                ListItem(
                    ListType("50) ".to_owned()),
                    "multi-digit numbered".to_owned()
                ),
                ListItem(
                    ListType("(1) ".to_owned()),
                    "parenthesised list item".to_owned()
                ),
                ListItem(
                    ListType(" (10) ".to_owned()),
                    "parenthesised list item".to_owned()
                ),
                ListItem(
                    ListType("  (100) ".to_owned()),
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
}
