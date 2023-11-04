use crate::parser::parse;
use crate::parser::Token;
use crate::parser::Token::*;
use crate::worditer::WordIter;
use unicode_segmentation::UnicodeSegmentation;

pub struct CommitMsgFmt {
    /// Max width of the message body; not used for the subject line.
    width: usize,
    /// The character that identifies a comment when used in column 0 of a line.
    comment_char: char,
}

impl CommitMsgFmt {
    pub fn new(width: usize, comment_char: char) -> CommitMsgFmt {
        CommitMsgFmt {
            width,
            comment_char,
        }
    }

    pub fn filter(&self, input: &str) -> String {
        let msg = parse(input, self.comment_char);
        // The output size can be less than the input size only if the input contains characters
        // that will be trimmed, such as leading whitespace, which is improbable. It is more likely
        // the output size will exceed the input size due to injected linefeeds and continuation
        // indentation. There is no worthwhile way to dynamically estimate the size difference
        // between input and output so we pad by a small constant factor big enough to fit the
        // linefeeds of a 3rd quartile message of 14 lines, as if such a message were written in a
        // single line.
        let probable_capacity_required = input.len() + 20;
        let mut buf = String::with_capacity(probable_capacity_required);
        self.reflow_into(&mut buf, &msg);
        buf
    }

    fn reflow_into(&self, buf: &mut String, msg: &[Token]) {
        for tok in msg {
            match *tok {
                BlockQuote(s) | Comment(s) | FencedCodeBlock(s) | Literal(s) | Scissored(s)
                | Trailer(s) => {
                    buf.push_str(s);
                }
                ListItem(ref indent, ref li, ref s) => {
                    let indent = &indent.0;
                    let li = &li.0;
                    let mut continuation = String::with_capacity(indent.len() + li.len());
                    continuation.push_str(indent);
                    continuation.push_str(&" ".repeat(li.len()));
                    buf.push_str(indent);
                    buf.push_str(li);

                    self.wrap_paragraph_into(buf, s, Some(&continuation));
                }
                Paragraph(ref p) => {
                    self.wrap_paragraph_into(buf, p, None);
                }
                Footnote(key, ref rest) => {
                    buf.push_str(key);
                    buf.push(' ');
                    let continuation = " ".repeat(key.graphemes(true).count() + 1);
                    self.wrap_paragraph_into(buf, rest.trim(), Some(&continuation));
                }
                Subject(s) => {
                    buf.push_str(s);
                }
                VerticalSpace => {}
            }
            buf.push('\n');
        }
    }

    fn wrap_paragraph_into(&self, buf: &mut String, paragraph: &str, continuation: Option<&str>) {
        let limit = match continuation {
            Some(c) => self.width.saturating_sub(c.len()),
            None => self.width,
        };
        let mut cur_line_len = 0;
        for word in WordIter::new(paragraph, self.comment_char) {
            let word_len = word.graphemes(true).count();

            // Not a new line so we need to fiddle with whitespace.
            if cur_line_len != 0 {
                if cur_line_len + word_len > limit {
                    cur_line_len = 0;
                    buf.push('\n');
                    if let Some(cont) = continuation {
                        buf.push_str(cont);
                    }
                } else {
                    buf.push(' ');
                }
            }

            buf.push_str(&word);
            cur_line_len += word_len + 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    fn filter(w: usize, s: &str) -> String {
        CommitMsgFmt::new(w, '#').filter(&s)
    }

    #[test]
    fn formats_empty_string() {
        assert_eq!("", filter(10, ""));
    }

    #[test]
    fn formats_non_empty_string() {
        assert_eq!(filter(10, "foo"), "foo\n");
    }

    #[test]
    fn formats_long_single_line_message() {
        let s = "f".repeat(100);
        assert_eq!(filter(10, &s), "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

ffffffffff
");
    }

    #[test]
    fn preserves_autosquash_subject_line() {
        let original_subject = "f".repeat(100);
        let autosquash_subject = format!("fixup! {}", original_subject);
        let input = format!(
            "
{subject}

format
this
",
            subject = autosquash_subject
        );

        let expected = format!(
            "
{subject}

format this
",
            subject = autosquash_subject
        );
        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn counts_line_length_by_graphemes() {
        let s = "
ääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääë

öööö ü";

        assert_eq!(
            filter(5, &s),
            "
ääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääää

ë

öööö
ü
",
            "break before every changing grapheme"
        );
    }

    #[test]
    fn formats_trailers() {
        let msg = "
foo

bar

Reviewed-by: Hubert Blaine Wolfeschlegelsteinhausenbergerdorff, Sr. <hubert.blaine.wolfe@666.sr>
Signed-off-by: Some Guy <some@email.address>
Cc: Abominable Snowman <yeti@mountain.np>
";

        assert_eq!(filter(10, &msg), msg, "print trailers literally");
    }

    #[test]
    fn formats_literals() {
        let input = "
foo

    literal indented with 4 spaces
      continuation

\tliteral indented with tab
\t  continuation
\t\tcontinuation
";

        let expected = "
foo

    literal indented with 4 spaces
      continuation

\tliteral indented with tab
\t  continuation
\t\tcontinuation
";

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn preserves_fenced_code_block() {
        let input = "
foo

```
backtick
fenced
code
block
```
";

        let expected = "
foo

```
backtick
fenced
code
block
```
";

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn preserves_fenced_code_block_interrupting_paragraph() {
        let input = "
foo

a
```
backtick
```
b
";

        let expected = "
foo

a
```
backtick
```
b
";

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn ignores_fenced_code_block_with_tilde() {
        let input = "
foo

~~~
tilde
fenced
code
block
not
supported
~~~
";

        let expected = "
foo

~~~ tilde fenced code block not supported ~~~
";

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn preserves_block_quote() {
        let input = "
foo

> block quote
paragraph
";

        let expected = input;

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn preserves_block_quote_with_attribution() {
        let input = "
foo

author wrote:
> block quote
paragraph
";

        let expected = input;

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn preserves_multiline_block_quote() {
        let input = "
xx-xxxxxx xxxx xxxxxxx xxxxxxxxxxxxxx

xxxx xxxxxx xxxxxxx xxxxx xx xxx xxx -x xxxxxx xxxx xxxx-xx-xxxxx, xxxxx
xxxxxxxxx xxxx xxxxxxx xxxxxxxxxxxxxx. xxxx xxx xxxxxxx:

> ```
> -x xx --xx-xxxx
>     xxxxxxxx xxxxxxx xxx xxxxxxx xxxxxxxxxxxxxx xxx xxxxxxxxxxxxxxxx
>     xxxxxxx xx xxx xxxxxxxx. xxxx xx
> ```

xxx xxxxxxx xxxxxxxx xx `xxxx` xx xx xxxxxx xxxxxxx xxxxxxxxxxxxxx.
";

        let expected = "
xx-xxxxxx xxxx xxxxxxx xxxxxxxxxxxxxx

xxxx xxxxxx xxxxxxx xxxxx xx
xxx xxx -x xxxxxx xxxx
xxxx-xx-xxxxx, xxxxx xxxxxxxxx
xxxx xxxxxxx xxxxxxxxxxxxxx.
xxxx xxx xxxxxxx:

> ```
> -x xx --xx-xxxx
>     xxxxxxxx xxxxxxx xxx xxxxxxx xxxxxxxxxxxxxx xxx xxxxxxxxxxxxxxxx
>     xxxxxxx xx xxx xxxxxxxx. xxxx xx
> ```

xxx xxxxxxx xxxxxxxx xx `xxxx`
xx xx xxxxxx xxxxxxx
xxxxxxxxxxxxxx.
";

        assert_eq!(filter(30, &input), expected);
    }

    #[test]
    fn formats_footnotes() {
        let msg = "
foo

paragraph

[2] note
[1] note
[3]    foo bar baz qux https://a.really-long-url.example
[4] https://a.really-long-url.example
[footnote] footnote extending
   beyond line-wrapping
            limit
[ä] multi-code-point footnote key
";
        let expected = "
foo

paragraph

[2] note
[1] note
[3] foo bar baz qux
    https://a.really-long-url.example
[4] https://a.really-long-url.example
[footnote] footnote
           extending
           beyond
           line-wrapping
           limit
[ä] multi-code-point
    footnote key
";

        assert_eq!(filter(20, &msg), expected);
    }

    #[test]
    fn formats_footnote_references() {
        let msg = "
foo

x [1] x
[1].
x [1] .

yy [2] y
[2] .

zzz [3]
z
[3] .

www [4] w
www [4] .
";
        let expected = "
foo

x [1]
x [1].
x [1] .

yy [2]
y [2] .

zzz [3]
z [3] .

www [4]
w
www [4] .
";

        assert_eq!(filter(8, &msg), expected);
    }

    #[test]
    fn wraps_long_list_items_aligning_continuation_line_with_content_start() {
        let input = "
foo

- list item that should be wrapped

- foo foo foo foo foo foo foo foo

- foo foo foo bar
- foo foo fooo bar
- foo foo foooo bar
- foo foo fooooo bar
- foo foo baaaaar bar

- ääääääääääääääëëëëëëëëëëëëëëö
";

        let expected = "
foo

- list item that
  should be
  wrapped

- foo foo foo
  foo foo foo
  foo foo

- foo foo foo
  bar
- foo foo fooo
  bar
- foo foo foooo
  bar
- foo foo fooooo
  bar
- foo foo
  baaaaar bar

- ääääääääääääääëëëëëëëëëëëëëëö
";

        assert_eq!(filter(16, &input), expected);
    }

    #[test]
    fn realigns_misaligned_continuation_line() {
        let input = "
foo

- continuation line that should be
      realigned

  - spc-indented continuation line
      that should be realigned
";

        let expected = "
foo

- continuation line that should be
  realigned

  - spc-indented continuation line
    that should be realigned
";

        assert_eq!(filter(34, &input), expected);
    }

    #[test]
    fn tolerates_continuation_longer_than_body_width() {
        // If a continuation is longer than the requested body width it is
        // impossible to wrap a footnote to within said width. Instead just
        // break up the footnote as aggressively as possible.
        let input = "
foo

[1] note note
";
        let expected = "
foo

[1] note
    note
";
        assert_eq!(filter(3, &input), expected);
    }

    #[test]
    fn joins_unnecessarily_wrapped_list_items() {
        let input = "
foo

- list item
  that should not have been wrapped
";

        let expected = "
foo

- list item that should not have been wrapped
";

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn preserves_multiple_list_items() {
        let input = "
foo

- list item
- list
  item
- list item
";

        let expected = "
foo

- list item
- list item
- list item
";

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn recognizes_list_item_indented_one_space() {
        let input = "
foo

 - list
   item

    - literal
";

        let expected = "
foo

 - list item

    - literal
";

        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn recognizes_different_list_types() {
        let msg = "
foo

- dash
* bullet
1. numbered
0. unordered numbered
2) numbered
3] numbered
4: numbered

6\tnumbered

7 numbered
";

        assert_eq!(filter(72, &msg), msg);
    }

    #[test]
    fn preserves_scissored_content() {
        let input = "
foo

format
this

# ------------------------ >8 ------------------------
preserve
 scissored

content
";

        let expected = "
foo

format this

# ------------------------ >8 ------------------------
preserve
 scissored

content
";
        assert_eq!(filter(72, &input), expected);
    }

    #[test]
    fn preserves_comment() {
        let input = "
foo

# comment
";

        let expected = "
foo

# comment
";
        assert_eq!(filter(2, &input), expected);
    }

    #[test]
    fn preserves_scissored_content_with_custom_comment_char() {
        let input = "
foo

# ------------------------ >8 ------------------------
format
this

; ------------------------ >8 ------------------------
preserve
 scissored

content
";

        let expected = "
foo

# ------------------------ >8 ------------------------ format this

; ------------------------ >8 ------------------------
preserve
 scissored

content
";
        let fmt = CommitMsgFmt::new(72, ';');
        assert_eq!(fmt.filter(&input), expected);
    }

    #[test]
    fn bug_wrapping_treats_exotic_whitespace_differently_from_ascii_spaces() {
        // Whether wrapping should treat all whitespace the same is debatable but certainly we only
        // consider the regular ASCII space. This test documents the acknowledgement thereof, not a
        // deliberate decision, and classifies it a bug for that reason.
        //
        // Some largely arbitrary whitespace characters from
        // https://en.wikipedia.org/w/index.php?title=Whitespace_character&oldid=858017200
        let input = "
foo

a b\tc\u{00a0}d\u{2003}e\u{2009}\u{2009}f\u{202f}g

    a b\tc\u{00a0}d\u{2003}e\u{2009}f\u{202f}g
";

        let expected = "
foo

a
b\tc\u{00a0}d\u{2003}e\u{2009}\u{2009}f\u{202f}g

    a b\tc\u{00a0}d\u{2003}e\u{2009}f\u{202f}g
";
        assert_eq!(filter(2, &input), expected);
    }

    #[test]
    fn does_not_break_words() {
        let input = "
foo

foo bar baz
qux areallylongword
and https://a.really-long-url.example

- foo bar baz
- qux https://a.really-long-url.example
- https://a.really-long-url.example

[1] https://a.really-long-url.example
";
        let expected = "
foo

foo bar
baz qux
areallylongword
and
https://a.really-long-url.example

- foo bar
  baz
- qux
  https://a.really-long-url.example
- https://a.really-long-url.example

[1] https://a.really-long-url.example
";
        assert_eq!(filter(10, &input), expected);
    }

    #[test]
    fn does_not_break_non_breaking_words() {
        let input = "
foo

foo #1 bar [1][qux] [2] baz -- wup

[1] note
[qux] note
[2] note
";
        let expected = "
foo

foo #1
bar [1][qux] [2]
baz --
wup

[1] note
[qux] note
[2] note
";
        assert_eq!(filter(2, &input), expected);
    }
}
