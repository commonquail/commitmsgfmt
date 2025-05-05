use std::borrow::Cow;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, PartialEq, Eq)]
pub struct ListType<'input>(pub &'input str);

#[derive(Debug, PartialEq, Eq)]
pub struct ListIndent<'input>(pub &'input str);

type ReflowableStrBuf<'input> = Cow<'input, str>;

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'input> {
    Comment(&'input str),
    FencedCodeBlock(&'input str),
    Footnote(&'input str, ReflowableStrBuf<'input>),
    ListItem(
        ListIndent<'input>,
        ListType<'input>,
        ReflowableStrBuf<'input>,
    ),
    Literal(&'input str),
    Paragraph(ReflowableStrBuf<'input>),
    Subject(&'input str),
    Scissored(&'input str),
    Trailer(&'input str),
    BlockQuote(&'input str),
    VerticalSpace,
}

// Token::FencedCodeBlock is the first block construct that can interrupt another block construct;
// that cannot be identified purely from either the current line or the previous token; and that
// may not be wrapped. That means there are valid situations we cannot represent without more
// sophisticated state management. We also don't want to needlessly extend a String. We'll just
// track the opening code fence, match it against closing fences, and treat the entire block as a
// glorified Token::Literal sequence. We accept the loss of composability.
struct CodeFence<'input>(&'input str);

impl CodeFence<'_> {
    fn is_closed_by(&self, line: &str) -> bool {
        // "until a closing code fence of the same type as the code block began with (backticks or
        // tildes), and with at least as many backticks or tildes as the opening code fence"
        line_as_code_fence(line)
            .map(|fence| fence.0.starts_with(self.0))
            .unwrap_or(false)
    }
}

pub fn parse<'a>(input: &'a str, comment_string: &str) -> Vec<Token<'a>> {
    let mut toks = Vec::new();

    let mut has_subject = false;
    let mut has_scissors = false;
    let lines = input.lines();
    let mut px = false;
    let mut in_code_fence: Option<CodeFence> = None;
    for line in lines {
        if has_scissors {
            toks.push(Token::Scissored(line));
        } else if let Some(ref fence) = in_code_fence {
            toks.push(Token::FencedCodeBlock(line));
            if fence.is_closed_by(line) {
                in_code_fence = None;
            }
        } else if let Some(fence) = line_as_code_fence(line) {
            toks.push(Token::FencedCodeBlock(line));
            in_code_fence = Some(fence);
        } else if line.starts_with(comment_string) {
            let index = comment_string.len();
            let t = if &line[index..] == " ------------------------ >8 ------------------------" {
                has_scissors = true;
                Token::Scissored(line)
            } else {
                Token::Comment(line)
            };
            toks.push(t);
        } else if is_line_blank_or_whitespace(line) {
            if toks.last() != Some(&Token::VerticalSpace) {
                toks.push(Token::VerticalSpace);
            }
            px = true;
        } else if !has_subject {
            parse_subject(line, &mut toks);
            has_subject = true;
            px = false;
        } else if px && is_line_footnote(line) {
            debug_assert!(line.contains(' '));
            let mut splitter = line.splitn(2, ' ');
            let key = splitter.next().unwrap();
            let rest = splitter.next().unwrap().trim().into();
            toks.push(Token::Footnote(key, rest));
        } else if is_line_trailer(line) {
            toks.push(Token::Trailer(line));
        } else if let Some(y) = match toks.last_mut() {
            Some(&mut Token::Footnote(_, ref mut b)) => extend_prose_buffer_with_line(b, line),
            Some(&mut Token::Paragraph(ref mut b)) => {
                line_as_line_block_quote(line).or_else(|| extend_prose_buffer_with_line(b, line))
            }
            Some(&mut Token::ListItem(_, _, ref mut b)) => {
                line_as_list_item(line).or_else(|| extend_prose_buffer_with_line(b, line))
            }
            _ => {
                if let Some(tok) = line_as_list_item(line) {
                    Some(tok)
                } else if is_line_indented(line) {
                    Some(Token::Literal(line))
                } else if let Some(tok) = line_as_line_block_quote(line) {
                    Some(tok)
                } else {
                    px = false;
                    Some(Token::Paragraph(line.trim().into()))
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

fn parse_subject<'input>(line: &'input str, toks: &mut Vec<Token<'input>>) {
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

    toks.push(Token::Subject(subject));
    toks.push(Token::VerticalSpace);

    if let Some(rest) = rest {
        let rest = rest.trim_start_matches('.');
        if !rest.is_empty() {
            toks.push(Token::Paragraph(rest.trim().into()));
        }
    }
}

fn extend_prose_buffer_with_line<'input>(
    buf: &mut ReflowableStrBuf<'input>,
    line: &'input str,
) -> Option<Token<'static>> {
    let buf = buf.to_mut();
    buf.push(' ');
    buf.push_str(line.trim());
    None
}

fn is_line_blank_or_whitespace(line: &str) -> bool {
    line.chars().all(char::is_whitespace)
}

fn is_line_footnote(line: &str) -> bool {
    if let Some(rest) = line.strip_prefix('[') {
        let mut chars = rest.chars();
        if chars.next() == Some(']') {
            // Reject "[]"
            return false;
        }
        let mut chars = chars.skip_while(|c| c != &']');
        let mut c = chars.next();
        if c != Some(']') {
            // Require "[.+]"
            return false;
        }
        c = chars.next();
        if c == Some(':') {
            // Allow "[.+]:?"
            c = chars.next();
        }
        if c != Some(' ') {
            // Require "[.+]:? "
            return false;
        }
        // Require "[.+]:? .+"
        return chars.next().is_some();
    }
    false
}

fn is_line_indented(line: &str) -> bool {
    line.starts_with("    ") || line.starts_with('\t')
}

fn is_line_trailer(line: &str) -> bool {
    // As of Git 2.42:
    //
    // 1) A trailer is comprised of a trailer token, a ":" separator, and a value.
    //
    // 2) A trailer token's bytes either satisfy C89's isalnum() or equal ASCII "-".
    //
    // 3) A trailer token contains at least one byte not counting the separator.
    //
    // 4) Only the last trailer-like block is recognized, and if that trailer-block violates the
    //    trailer token requirement then no trailer block is recognized.
    //
    // 5) A single trailer may have an empty value. If there are multiple trailers and one has an
    //    empty value, that trailer will be merged with the previous or next trailer's value.
    //
    // Requirements 1) and 2) are easy and necessary. Requirement 3) has a tricky interaction with
    // other functionality but we can probably disregard it, a trailer token of /^[a-z-]$/ seems
    // like a terrible trailer.
    //
    // We don't care about requirements 4) or 5). That's more of a post-submission presentation
    // matter, and implementing it would be needlessly complex, error prone, and most importantly
    // very unergonomic during writing. This means we can recognize something as a trailer that Git
    // would not recognize as a trailer but that's the sensible trade-off.

    // We look for the space, not the colon. A space will come in almost every line so we'll find
    // that after a typical word length's steps. Colons never occur so they'd take O(len(line))
    // time to figure out nothing. When we do find a space it's constant time effort to locate a
    // colon, which must come immediately before.
    let trailer_token_with_colon = match line.find(' ') {
        Some(ix_first_space) => &line[..ix_first_space],
        None => return false,
    };

    if trailer_token_with_colon.len() < 3 {
        return false;
    }

    let is_token_char = |c: char| c.is_ascii_alphanumeric() || c == '-';
    let mut chars = trailer_token_with_colon.chars().rev();
    match chars.next() {
        Some(':') => chars.all(is_token_char),
        _ => false,
    }
}

fn line_as_list_item(line: &str) -> Option<Token> {
    enum LiState {
        New,
        IndentSp1,
        IndentSp2,
        Ul,
        Ol,
        ParenWrappedOlOpen,
        ParenWrappedOl,
        OlClosed,
        TrailingWs,
        StopParse,
    }

    let mut li_state = LiState::New;
    let mut ix_li_type_start = 0;
    let mut ix_li_content_start: Option<usize> = None;
    for (ix, c) in line.char_indices() {
        match li_state {
            LiState::New | LiState::IndentSp1 | LiState::IndentSp2 => {
                ix_li_type_start = ix;
                li_state = match c {
                    ' ' => match li_state {
                        LiState::New => LiState::IndentSp1,
                        LiState::IndentSp1 => LiState::IndentSp2,
                        _ => LiState::StopParse,
                    },
                    '-' | '*' => LiState::Ul,
                    '(' => LiState::ParenWrappedOlOpen,
                    _ if c.is_ascii_digit() => LiState::Ol,
                    _ => LiState::StopParse,
                };
            }
            LiState::Ul => {
                li_state = if c.is_ascii_whitespace() {
                    LiState::TrailingWs
                } else {
                    LiState::StopParse
                }
            }
            LiState::ParenWrappedOlOpen => {
                li_state = if c.is_ascii_digit() {
                    LiState::ParenWrappedOl
                } else {
                    LiState::StopParse
                }
            }
            LiState::ParenWrappedOl => {
                if c == ')' {
                    li_state = LiState::OlClosed;
                } else if !c.is_ascii_digit() {
                    li_state = LiState::StopParse;
                }
            }
            LiState::Ol => match c {
                ')' | '.' | ':' | ']' => {
                    li_state = LiState::OlClosed;
                }
                _ => {
                    if !c.is_ascii_digit() {
                        li_state = LiState::StopParse;
                    }
                }
            },
            LiState::OlClosed => {
                li_state = if c.is_ascii_whitespace() {
                    LiState::TrailingWs
                } else {
                    LiState::StopParse
                }
            }
            LiState::TrailingWs => {
                if !c.is_ascii_whitespace() {
                    ix_li_content_start = Some(ix);
                    li_state = LiState::StopParse;
                }
            }
            LiState::StopParse => break,
        }
    }

    ix_li_content_start.map(|ix_li_content_start| {
        let li_indent = &line[..ix_li_type_start];
        let li_type = &line[ix_li_type_start..ix_li_content_start];
        let li_content = line[ix_li_content_start..].into();
        Token::ListItem(ListIndent(li_indent), ListType(li_type), li_content)
    })
}

fn line_as_code_fence(line: &'_ str) -> Option<CodeFence> {
    enum FenceState {
        New,
        IndentSp1,
        IndentSp2,
        IndentSp3,
        Backtick,
    }

    let mut fence_state = FenceState::New;
    let mut ix_fence_start = 0;
    let mut fence_length = 0;
    let mut tally = || fence_length += 1;
    // https://spec.commonmark.org/0.30/#fenced-code-blocks
    // Backtick fenced code blocks appear relatively safe to support. Tilde fenced code blocks, on
    // the other hand, are unsafe: tildes are often used for emphasizing compilation error output
    // or underlining headers.
    for (ix, c) in line.char_indices() {
        match fence_state {
            FenceState::New
            | FenceState::IndentSp1
            | FenceState::IndentSp2
            | FenceState::IndentSp3 => {
                ix_fence_start = ix;
                // "preceded by up to three spaces of indentation"
                fence_state = match c {
                    ' ' => match fence_state {
                        FenceState::New => FenceState::IndentSp1,
                        FenceState::IndentSp1 => FenceState::IndentSp2,
                        FenceState::IndentSp2 => FenceState::IndentSp3,
                        _ => break,
                    },
                    '`' => {
                        tally();
                        FenceState::Backtick
                    }
                    _ => break,
                };
            }
            // "Tildes and backticks cannot be mixed."
            FenceState::Backtick => match c {
                '`' => tally(),
                _ => break,
            },
        }
    }

    // "at least three consecutive backtick characters (`) or tildes (~)"
    if fence_length >= 3 {
        let ix_end = ix_fence_start + fence_length;
        debug_assert!(ix_end <= line.len());
        let fence = &line[ix_fence_start..ix_end];
        Some(CodeFence(fence))
    } else {
        None
    }
}

fn line_as_line_block_quote(line: &str) -> Option<Token> {
    if line.starts_with('>') {
        Some(Token::BlockQuote(line))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    use pretty_assertions::assert_eq;

    fn parse(s: &str) -> Vec<Token> {
        super::parse(s, "#")
    }

    #[test]
    fn llvmcov_impl_debug() {
        let actual = ListItem(ListIndent("a"), ListType("b"), "c".into());
        assert_eq!(
            r#"ListItem(ListIndent("a"), ListType("b"), "c")"#,
            format!("{:?}", actual)
        );
    }

    #[test]
    fn llvmcov_line_as_list_item() {
        let matrix = [
            ("1.  ", None),
            ("11. a", Some(("", "11. ", "a"))),
            ("1. a", Some(("", "1. ", "a"))),
            (" 1. a", Some((" ", "1. ", "a"))),
            ("  1. a", Some(("  ", "1. ", "a"))),
            ("   1. a", None),
            ("(1) a", Some(("", "(1) ", "a"))),
            ("(1)a", None),
            ("(1a", None),
            ("(1", None),
            ("(a) b", None),
            ("1) d", Some(("", "1) ", "d"))),
            ("1: a", Some(("", "1: ", "a"))),
            ("1] a", Some(("", "1] ", "a"))),
            ("* b", Some(("", "* ", "b"))),
            ("- c", Some(("", "- ", "c"))),
        ];
        for (input, expected) in matrix.iter() {
            let expected = expected.map(|e| ListItem(ListIndent(e.0), ListType(e.1), e.2.into()));
            let actual = line_as_list_item(input);
            assert_eq!(expected, actual, "'{}'=>{:?}", input, expected);
        }
    }

    #[test]
    fn llvmcov_is_line_footnote() {
        let matrix = [
            ("[", false),
            ("]", false),
            ("[]", false),
            ("[a", false),
            ("[a]", false),
            ("[a] ", false),
            ("[a] b", true),
            ("[a]: b", true),
        ];
        for (input, expected) in matrix.iter() {
            let actual = is_line_footnote(input);
            assert_eq!(expected, &actual, "'{}'=>{}", input, expected);
        }
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
        assert_eq!(super::parse("# foo", "#"), [Comment("# foo")]);
    }

    #[test]
    fn parses_custom_comment() {
        assert_eq!(super::parse("@ foo", "@"), [Comment("@ foo")]);
        assert_eq!(super::parse("# foo", "@"), [Subject("# foo")]);
    }

    #[test]
    fn parses_mixed_comment_and_content() {
        assert_eq!(
            parse("# foo\n\n # bar"),
            [Comment("# foo"), VerticalSpace, Subject("# bar"),],
        );
    }

    #[test]
    fn parses_subject() {
        assert_eq!(parse("Hello, world"), [Subject("Hello, world")]);
    }

    #[test]
    fn parses_subject_trimming_start() {
        assert_eq!(parse(" # foo"), [Subject("# foo")]);
    }

    #[test]
    fn parses_subject_with_area() {
        let expected_subject = "foo: bar";

        assert_eq!(parse(expected_subject), [Subject(expected_subject)]);
    }

    #[test]
    fn parses_fitting_subject() {
        let s = "f".repeat(SUBJECT_CHAR_LIMIT);
        assert_eq!(parse(&s), [Subject(&s)]);
    }

    #[test]
    fn parses_too_long_subject_as_subject_and_paragraph() {
        let s = "f".repeat(SUBJECT_CHAR_LIMIT) + "bar";
        assert_eq!(
            parse(&s),
            [
                Subject(&"f".repeat(SUBJECT_CHAR_LIMIT)),
                VerticalSpace,
                Paragraph("bar".into()),
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
            assert_eq!(parse(&autosquash_subject), [Subject(&autosquash_subject),],);
        }
    }

    #[test]
    fn subject_strips_trailing_periods() {
        let expected_subject = "f".repeat(SUBJECT_CHAR_LIMIT - 1);

        let s = expected_subject.clone() + "....";

        assert_eq!(parse(&s), [Subject(&expected_subject)]);
    }

    #[test]
    fn bug_subject_comprised_of_periods_becomes_empty() {
        let periods = "....";

        assert_eq!(parse(periods), [Subject("")]);
    }

    #[test]
    fn too_long_subject_breaks_at_first_period_within_subject_limit() {
        let expected_subject = "f".repeat(SUBJECT_CHAR_LIMIT - 10);

        let s = expected_subject.clone() + " . abc . def";

        assert_eq!(
            parse(&s),
            [
                Subject(&expected_subject),
                VerticalSpace,
                Paragraph("abc . def".into()),
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
                Subject(&expected_subject),
                VerticalSpace,
                Paragraph("abc.def".into()),
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
                Subject("foo"),
                VerticalSpace,
                Paragraph("this is one paragraph".into()),
                VerticalSpace,
                Paragraph("this is".into()),
                Comment("# two"),
                Paragraph("paragraphs".into()),
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
                Subject("foo"),
                VerticalSpace,
                Paragraph("this paragraph has trailing whitespace".into()),
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
                Subject("some subject"),
                VerticalSpace,
                Paragraph("some paragraph".into()),
                VerticalSpace,
                Literal("    some 4-space literal"),
                Literal("      continuation"),
                VerticalSpace,
                Paragraph("some other paragraph no literal without vertical space".into()),
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
                Subject("some subject"),
                VerticalSpace,
                Paragraph("some paragraph".into()),
                VerticalSpace,
                Literal("\tsome 4-space literal"),
                Literal("\t  continuation"),
                Literal("\t\tcontinuation"),
                VerticalSpace,
                Paragraph("some other paragraph no literal without vertical space".into()),
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
                Subject("some subject"),
                VerticalSpace,
                Paragraph("some paragraph".into()),
                VerticalSpace,
                Literal("    some 4-space literal"),
                Literal("    some 4-space literal"),
                Literal("    some 4-space literal"),
                VerticalSpace,
                Paragraph("some other paragraph".into()),
            ],
        );
    }

    #[test]
    fn parses_codefence_backtick_verbatim() {
        let input = "
subject

```
backtick
```

 ```
 backtick
 ```

  ```
  backtick
  ```

   ```
   backtick
   ```
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            FencedCodeBlock("```"),
            FencedCodeBlock("backtick"),
            FencedCodeBlock("```"),
            VerticalSpace,
            FencedCodeBlock(" ```"),
            FencedCodeBlock(" backtick"),
            FencedCodeBlock(" ```"),
            VerticalSpace,
            FencedCodeBlock("  ```"),
            FencedCodeBlock("  backtick"),
            FencedCodeBlock("  ```"),
            VerticalSpace,
            FencedCodeBlock("   ```"),
            FencedCodeBlock("   backtick"),
            FencedCodeBlock("   ```"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_tilde_not_fenced_code_block() {
        let input = "
subject

~~~
tilde
~~~
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            Paragraph("~~~ tilde ~~~".into()),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_backtick_indented_aligned_4sp_not_fenced_code_block() {
        let input = "
subject

    ```
    backtick
    ```
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            Literal("    ```"),
            Literal("    backtick"),
            Literal("    ```"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_backtick_indented_unaligned() {
        let input = "
subject

 ```
backtick 1 0
```
  ```
backtick 2 0
```
   ```
backtick 3 0
```
```
backtick 0 1
 ```
```
backtick 0 2
  ```
```
backtick 0 3
   ```
  ```
backtick 2 1
 ```
   ```
backtick 3 2
  ```
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            FencedCodeBlock(" ```"),
            FencedCodeBlock("backtick 1 0"),
            FencedCodeBlock("```"),
            FencedCodeBlock("  ```"),
            FencedCodeBlock("backtick 2 0"),
            FencedCodeBlock("```"),
            FencedCodeBlock("   ```"),
            FencedCodeBlock("backtick 3 0"),
            FencedCodeBlock("```"),
            FencedCodeBlock("```"),
            FencedCodeBlock("backtick 0 1"),
            FencedCodeBlock(" ```"),
            FencedCodeBlock("```"),
            FencedCodeBlock("backtick 0 2"),
            FencedCodeBlock("  ```"),
            FencedCodeBlock("```"),
            FencedCodeBlock("backtick 0 3"),
            FencedCodeBlock("   ```"),
            FencedCodeBlock("  ```"),
            FencedCodeBlock("backtick 2 1"),
            FencedCodeBlock(" ```"),
            FencedCodeBlock("   ```"),
            FencedCodeBlock("backtick 3 2"),
            FencedCodeBlock("  ```"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_backtick_fence_extra_long() {
        let input = "
subject

```
backtick 3 4
````
````
backtick 4 5
`````
`````
backtick 5 6
``````
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            FencedCodeBlock("```"),
            FencedCodeBlock("backtick 3 4"),
            FencedCodeBlock("````"),
            FencedCodeBlock("````"),
            FencedCodeBlock("backtick 4 5"),
            FencedCodeBlock("`````"),
            FencedCodeBlock("`````"),
            FencedCodeBlock("backtick 5 6"),
            FencedCodeBlock("``````"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_backtick_fence_too_short_not_fenced_code_block() {
        let input = "
subject

``
backtick
``
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            Paragraph("`` backtick ``".into()),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_backtick_with_infostring() {
        let input = "
subject

```info
backtick info no leading ws
```

``` info
backtick info leading sp
```

```	info
backtick info leading tab
```

```info`
backtick info accept illegal info with backtick
```

```info~
backtick info accept legal info with tilde
```
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            FencedCodeBlock("```info"),
            FencedCodeBlock("backtick info no leading ws"),
            FencedCodeBlock("```"),
            VerticalSpace,
            FencedCodeBlock("``` info"),
            FencedCodeBlock("backtick info leading sp"),
            FencedCodeBlock("```"),
            VerticalSpace,
            FencedCodeBlock("```\tinfo"),
            FencedCodeBlock("backtick info leading tab"),
            FencedCodeBlock("```"),
            VerticalSpace,
            FencedCodeBlock("```info`"),
            FencedCodeBlock("backtick info accept illegal info with backtick"),
            FencedCodeBlock("```"),
            VerticalSpace,
            FencedCodeBlock("```info~"),
            FencedCodeBlock("backtick info accept legal info with tilde"),
            FencedCodeBlock("```"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_backtick_can_interrupt_paragraph() {
        let input = "
subject

a
```
backtick
```
b
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            Paragraph("a".into()),
            FencedCodeBlock("```"),
            FencedCodeBlock("backtick"),
            FencedCodeBlock("```"),
            Paragraph("b".into()),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_backtick_fence_unmatched_length() {
        let input = "
subject

````
backtick
```

backtick

``
backtick
````
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            FencedCodeBlock("````"),
            FencedCodeBlock("backtick"),
            FencedCodeBlock("```"),
            FencedCodeBlock(""),
            FencedCodeBlock("backtick"),
            FencedCodeBlock(""),
            FencedCodeBlock("``"),
            FencedCodeBlock("backtick"),
            FencedCodeBlock("````"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_codefence_backtick_unterminated() {
        let input = "
subject

```
backtick
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            FencedCodeBlock("```"),
            FencedCodeBlock("backtick"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn bug_parses_codefence_backtick_enclosed_in_block_quote() {
        let input = "
subject

> a
> ```
> backtick
> ```
> b

> c
> ```
> backtick
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            BlockQuote("> a"),
            BlockQuote("> ```"),
            BlockQuote("> backtick"),
            BlockQuote("> ```"),
            BlockQuote("> b"),
            VerticalSpace,
            BlockQuote("> c"),
            BlockQuote("> ```"),
            BlockQuote("> backtick"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn bug_parses_codefence_backtick_enclosed_in_list_item() {
        let input = "
subject

- a
  ```
  backtick
  ```
  b

- c
  ```
  backtick
";

        let expected = vec![
            VerticalSpace,
            Subject("subject"),
            VerticalSpace,
            ListItem(ListIndent(""), ListType("- "), "a".into()),
            FencedCodeBlock("  ```"),
            FencedCodeBlock("  backtick"),
            FencedCodeBlock("  ```"),
            Paragraph("b".into()),
            VerticalSpace,
            ListItem(ListIndent(""), ListType("- "), "c".into()),
            FencedCodeBlock("  ```"),
            FencedCodeBlock("  backtick"),
        ];

        let actual = parse(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn parses_block_quote_verbatim() {
        assert_eq!(
            parse(
                "
some subject

some paragraph

> some block quote

some other paragraph
"
            ),
            [
                VerticalSpace,
                Subject("some subject"),
                VerticalSpace,
                Paragraph("some paragraph".into()),
                VerticalSpace,
                BlockQuote("> some block quote"),
                VerticalSpace,
                Paragraph("some other paragraph".into()),
            ],
        );
    }

    #[test]
    fn parses_nested_block_quotes_verbatim() {
        assert_eq!(
            parse(
                "
some subject

some paragraph

> > some block quote

some other paragraph
"
            ),
            [
                VerticalSpace,
                Subject("some subject"),
                VerticalSpace,
                Paragraph("some paragraph".into()),
                VerticalSpace,
                BlockQuote("> > some block quote"),
                VerticalSpace,
                Paragraph("some other paragraph".into()),
            ],
        );
    }

    #[test]
    fn parses_nested_block_quotes_ignoring_quote_marker_spacing_and_quote_levels() {
        assert_eq!(
            parse(
                "
some subject

some paragraph

>>>> >>> >> some block quote

some other paragraph
"
            ),
            [
                VerticalSpace,
                Subject("some subject"),
                VerticalSpace,
                Paragraph("some paragraph".into()),
                VerticalSpace,
                BlockQuote(">>>> >>> >> some block quote"),
                VerticalSpace,
                Paragraph("some other paragraph".into()),
            ],
        );
    }

    #[test]
    fn parses_block_quote_with_immediately_preceding_paragraph_as_attribution_leaving_no_vertical_space(
    ) {
        assert_eq!(
            parse(
                "
some subject

some attribution paragraph
> some block quote

some other paragraph
"
            ),
            [
                VerticalSpace,
                Subject("some subject"),
                VerticalSpace,
                Paragraph("some attribution paragraph".into()),
                BlockQuote("> some block quote"),
                VerticalSpace,
                Paragraph("some other paragraph".into()),
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
                Subject("subject"),
                VerticalSpace,
                Trailer("Fixes: All the things"),
                Trailer("Cc: John Doe <john@doe.com>"),
                Trailer("Reviewed-by: NSA"),
                Trailer("Signed-off-by: Jane Doe <jane@doe.com>"),
            ],
        );
    }

    #[test]
    fn trailer_token_satisfies_isalnum_ish() {
        assert_eq!(
            parse(
                "
subject

abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789: æøå
æ: multi-byte token char
"
            ),
            [
                VerticalSpace,
                Subject("subject"),
                VerticalSpace,
                Trailer("abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789: æøå"),
                Paragraph("æ: multi-byte token char".into()),
            ],
        );
    }

    #[test]
    fn trailer_token_is_at_least_2_bytes() {
        assert_eq!(
            parse(
                "
subject

ab: c
a: b
"
            ),
            [
                VerticalSpace,
                Subject("subject"),
                VerticalSpace,
                Trailer("ab: c"),
                Paragraph("a: b".into()),
            ],
        );
    }

    #[test]
    fn trailer_must_have_value() {
        assert_eq!(
            parse(
                "
subject

ab: c
ab:
"
            ),
            [
                VerticalSpace,
                Subject("subject"),
                VerticalSpace,
                Trailer("ab: c"),
                Paragraph("ab:".into()),
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
                Subject("subject"),
                VerticalSpace,
                Footnote("[1]", "footnote".into()),
                Footnote("[fo-ot]", "note".into()),
                Footnote("[ä]", "multi-code-point footnote key".into()),
                VerticalSpace,
                Footnote("[@]:", "footnote".into()),
                VerticalSpace,
                Paragraph("[] not a footnote".into()),
                VerticalSpace,
                Paragraph("[1]not a footnote".into()),
                VerticalSpace,
                Paragraph("[1]".into()),
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
                Subject("subject"),
                VerticalSpace,
                Paragraph("paragraph 1 [1] not footnote 1 [2] not footnote 2".into()),
                VerticalSpace,
                Paragraph("paragraph 2".into()),
                VerticalSpace,
                Footnote("[1]", "footnote 1".into()),
                Footnote("[2]", "footnote 2".into()),
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
                Subject("subject"),
                VerticalSpace,
                Footnote("[2]", "bar".into()),
                Footnote("[b]", "a".into()),
                Footnote("[a]", "b".into()),
                Footnote("[1]", "foo".into()),
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
                Subject("subject"),
                VerticalSpace,
                Footnote("[1]", "foo bar".into()),
                Footnote("[2]", "foo bar".into()),
                Footnote("[3]", "foo bar".into()),
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
                Subject("subject"),
                VerticalSpace,
                Footnote("[1]", "foo".into()),
                Footnote("[1]", "bar".into()),
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

\t- tab indent is literal, not list item
"
            ),
            [
                VerticalSpace,
                Subject("foo"),
                VerticalSpace,
                ListItem(ListIndent(""), ListType("- "), "list item".into()),
                ListItem(ListIndent(""), ListType("- "), "wrapped list item".into()),
                ListItem(
                    ListIndent(""),
                    ListType("- "),
                    "over-indented continuation".into()
                ),
                ListItem(
                    ListIndent(""),
                    ListType("- "),
                    "under-indented continuation".into()
                ),
                VerticalSpace,
                Paragraph("paragraph".into()),
                VerticalSpace,
                ListItem(ListIndent(" "), ListType("- "), "indented list item".into()),
                VerticalSpace,
                ListItem(
                    ListIndent("  "),
                    ListType("- "),
                    "indented list item".into()
                ),
                VerticalSpace,
                Paragraph("- paragraph".into()),
                VerticalSpace,
                ListItem(ListIndent(""), ListType("- "), "spaced list item".into()),
                VerticalSpace,
                Literal("\t- tab indent is literal, not list item"),
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
                Subject("foo"),
                VerticalSpace,
                ListItem(ListIndent(""), ListType("- "), "dash".into()),
                ListItem(ListIndent(""), ListType("* "), "bullet".into()),
                ListItem(ListIndent(""), ListType("1. "), "numbered".into()),
                ListItem(ListIndent(""), ListType("0. "), "unordered numbered".into()),
                ListItem(ListIndent(""), ListType("2) "), "numbered".into()),
                ListItem(ListIndent(""), ListType("3] "), "numbered".into()),
                ListItem(ListIndent(""), ListType("4: "), "numbered".into()),
                ListItem(
                    ListIndent(""),
                    ListType("50) "),
                    "multi-digit numbered".into()
                ),
                ListItem(
                    ListIndent(""),
                    ListType("(1) "),
                    "parenthesised list item".into()
                ),
                ListItem(
                    ListIndent(" "),
                    ListType("(10) "),
                    "parenthesised list item".into()
                ),
                ListItem(
                    ListIndent("  "),
                    ListType("(100) "),
                    "parenthesised list item".into()
                ),
                VerticalSpace,
                Paragraph("6\tnot a list item".into()),
                VerticalSpace,
                Paragraph("7 not a list item".into()),
                VerticalSpace,
                Paragraph("-) not a list item".into()),
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
                Subject("subject"),
                VerticalSpace,
                Paragraph("format this".into()),
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
    fn parses_scissored_content_with_custom_comment_string() {
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
", "$"
            ),
            [
                VerticalSpace,
                Subject("subject"),
                VerticalSpace,
                Paragraph("# ------------------------ >8 ------------------------ above is not a comment; do the needful".into()),
                VerticalSpace,
                Scissored("$ ------------------------ >8 ------------------------"),
                Scissored("do"),
                Scissored(" not"),
                Scissored("  format"),
            ],
        );
    }
    #[test]
    fn parses_scissored_content_with_custom_multichar_comment_string() {
        assert_eq!(
            super::parse(
                "
subject

# ------------------------ >8 ------------------------
above is not a comment;
do the needful

## ------------------------ >8 ------------------------
do
 not
  format
", "##"
            ),
            [
                VerticalSpace,
                Subject("subject"),
                VerticalSpace,
                Paragraph("# ------------------------ >8 ------------------------ above is not a comment; do the needful".into()),
                VerticalSpace,
                Scissored("## ------------------------ >8 ------------------------"),
                Scissored("do"),
                Scissored(" not"),
                Scissored("  format"),
            ],
        );
    }
}
