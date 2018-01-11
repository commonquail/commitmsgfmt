use parser::Token::*;
use parser::Token;
use parser::parse;
use unicode_segmentation::UnicodeSegmentation;

pub struct CommitMsgFmt {
    width: usize,
}

impl CommitMsgFmt {
    pub fn new(width: usize) -> CommitMsgFmt {
        CommitMsgFmt { width: width }
    }

    pub fn filter(&self, input: &str) -> String {
        let msg = parse(input);
        self.reflow(&msg)
    }

    fn reflow(&self, msg: &[Token]) -> String {
        let mut buf = String::new();
        for tok in msg {
            match *tok {
                Comment(ref c) => {
                    buf.push_str(c.as_str());
                    buf.push('\n');
                }
                ListItem(ref prefix, ref s) => {
                    let prefix = &prefix.0;
                    let mut rest = s.clone();
                    buf.push_str(prefix);

                    let mut rest_len = rest.graphemes(true).count();
                    let indent = &" ".repeat(prefix.len());
                    let limit = self.width - indent.len();

                    while rest_len > limit {
                        let mut end = limit + 1;
                        while !rest.is_char_boundary(end) {
                            end -= 1;
                        }
                        let (end, start) = match rest[..end].rfind(' ') {
                            Some(prev_space) => (prev_space, prev_space + 1),
                            None => match rest.grapheme_indices(true).nth(end) {
                                Some((ll, _)) => (ll, ll),
                                None => (end, end),
                            },
                        };
                        buf.push_str(&rest[..end]);
                        buf.push('\n');
                        buf.push_str(indent);
                        rest = rest.split_off(start);
                        rest_len = rest.len();
                    }
                    buf.push_str(&rest);
                    buf.push('\n');
                }
                Literal(ref l) => {
                    buf.push_str(l.as_str());
                }
                Paragraph(ref p) => {
                    let mut line = String::with_capacity(self.width);
                    for c in p.chars() {
                        let len = line.graphemes(true).count();
                        match c {
                            ' ' => {
                                if len < self.width {
                                    line.push(c);
                                } else {
                                    buf.push_str(&line);
                                    buf.push('\n');
                                    line.clear();
                                }
                            }
                            _ => {
                                if len < self.width {
                                    line.push(c);
                                } else {
                                    let rest = match line.rfind(' ') {
                                        Some(prev_space) => {
                                            let rest = line.split_off(prev_space);
                                            Some(rest)
                                        }
                                        None => None,
                                    };
                                    buf.push_str(&line);
                                    buf.push('\n');

                                    line.clear();

                                    if let Some(rest) = rest {
                                        line.push_str(&rest[1..]);
                                    }
                                    line.push(c);
                                }
                            }
                        }
                    }
                    buf.push_str(&line);
                    buf.push('\n');
                }
                Reference(ref s) | Subject(ref s) | Trailer(ref s) => {
                    buf.push_str(s.as_str());
                    buf.push('\n');
                }
                VerticalSpace => buf.push('\n'),
            }
        }

        buf
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn filter(w: usize, s: &str) -> String {
        CommitMsgFmt::new(w).filter(&s)
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
        let s = "f".repeat(105);
        assert_eq!(filter(10, &s), "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

ffffffffff
fffff
");
    }

    #[test]
    fn counts_line_length_by_graphemes() {
        let s = "
ääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääë

öööööü";

        assert_eq!(filter(5, &s), "
ääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääääää

ë

ööööö
ü
", "break before every changing grapheme");
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
    fn formats_references() {
        // References don't wrap. Many are just URLs, which don't wrap nicely,
        // or short sentences that don't qualify for wrapping, but some
        // references are entire paragraphs of prose and ought to be wrapped.
        // See: git -C ../git/ log --format=%b | grep '^\[[^]]\+\] '
        let msg = "
foo

[2] note
[1] note
[reference] reference extending beyond line-wrapping limit
";

        assert_eq!(filter(10, &msg), msg, "print references literally");
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

- ääääääääääääää
  ëëëëëëëëëëëëëë
  ö
";

        assert_eq!(filter(16, &input), expected);
    }

    #[test]
    fn realigns_misaligned_continuation_line() {
        let input = "
foo

- continuation line that should be
      realigned
";

        let expected = "
foo

- continuation line that should be
  realigned
";

        assert_eq!(filter(34, &input), expected);
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
}
