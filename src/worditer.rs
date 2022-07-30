use regex::Regex;
use std::borrow::Cow;

/// An iterator over "words" in some text. A word is generally a sequence of non-whitespace
/// characters bounded by either whitespace characters or the empty string. However, this is not a
/// definition and the exact details are unspecified. Rather, the iterator promises to produce
/// results that wrap safely and sensibly.
pub(crate) struct WordIter<'text> {
    /// The commit message comment character, to avoid creating a word that starts with the comment
    /// character lest that word degenerates into a comment.
    comment_char: char,
    /// The "next word", if present, may be considered the head of a list whose rest is
    /// [`Self::naive_words`] before the next invocation of [`Self::next()`]. It was the last word
    /// extracted from `naive_words` on the previous invocation of `next()` that was determined to
    /// not be a logical part of the result of that invocation.
    next_word: Option<&'text str>,
    /// An iterator over each whitespace-separated token in the submitted text. `WordIter` may
    /// consume multiple "naive words" to form a single "word" result during [`Self::next()`].
    naive_words: core::str::Split<'text, char>,
}

lazy_static! {
    static ref FOOTNOTE_REFERENCE: Regex = Regex::new(r"^(?:\[[^]]+\])+$").unwrap();
}

impl<'text> WordIter<'text> {
    pub fn new(text: &'text str, comment_char: char) -> Self {
        WordIter {
            comment_char,
            next_word: None,
            naive_words: text.split(' '),
        }
    }

    fn is_non_breaking_word(&self, word: &str) -> bool {
        word == "--" || word.starts_with(self.comment_char) || FOOTNOTE_REFERENCE.is_match(word)
    }
}

impl<'text> Iterator for WordIter<'text> {
    type Item = Cow<'text, str>;

    fn next(&mut self) -> Option<Self::Item> {
        let word_start = self.next_word.or_else(|| self.naive_words.next())?;
        let mut non_breaking_word = Cow::Borrowed(word_start);

        self.next_word = self.naive_words.next();
        while let Some(word) = self.next_word {
            if self.is_non_breaking_word(word) {
                let non_breaking_word = non_breaking_word.to_mut();
                non_breaking_word.push(' ');
                non_breaking_word.push_str(word);
            } else if word.is_empty() {
                // str::split(' ') discards the matched space but produces empty
                // strings instead; this is "surprising but intentional". We
                // don't need those empty strings so skip them.
                //
                // split_whitespace() and split_ascii_whitespace() don't produce
                // empty strings but match more tokens than we're used to; see
                // be0833e (Document Unicode whitespace handling, 2018-09-16).
                debug_assert!("a ".split_ascii_whitespace().collect::<Vec<&str>>() == vec!["a"]);
            } else {
                break;
            }

            self.next_word = self.naive_words.next();
        }

        Some(non_breaking_word)
    }
}

impl std::iter::FusedIterator for WordIter<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rand::seq::SliceRandom;

    type Item<'text> = <WordIter<'text> as Iterator>::Item;

    fn iter(text: &str) -> WordIter {
        WordIter::new(&text, '#')
    }

    fn collect(it: WordIter) -> Vec<Item> {
        it.collect()
    }

    fn iter_collect(text: &str) -> Vec<Item> {
        collect(iter(&text))
    }

    fn some_comment_char() -> char {
        let some_comment_chars = ['#', ';', '!', '%'];
        *some_comment_chars.choose(&mut rand::thread_rng()).unwrap()
    }

    #[test]
    fn smoke() {
        let text = "a b #1 c [1] d";
        let expect = ["a", "b #1", "c [1]", "d"];
        let res = iter_collect(&text);
        assert_eq!(res, expect);
    }

    #[test]
    fn smoke_ridiculous() {
        let text = "a #1 #2 b [1][2] [3] [4]c d";
        let expect = ["a #1 #2", "b [1][2] [3]", "[4]c", "d"];
        let res = iter_collect(&text);
        assert_eq!(res, expect);
    }

    #[test]
    fn empty_is_empty() {
        let empty_string = "";
        let expect = [empty_string];
        let res = iter_collect(&empty_string);
        assert_eq!(res, expect);
    }

    #[test]
    fn no_space_is_input() {
        let no_space_text = "a";
        let expect = ["a"];
        let res = iter_collect(&no_space_text);
        assert_eq!(res, expect);
    }

    #[test]
    fn many_spaces() {
        let text = "a   b";
        let expect = ["a", "b"];
        let res = iter_collect(&text);
        assert_eq!(res, expect);
    }

    #[test]
    fn none_result_repeats() {
        let res = {
            let no_space_text = "a";
            let mut it = iter(&no_space_text);
            let _ = it.next();
            [it.next(), it.next()]
        };
        let expect = [None, None];
        assert_eq!(res, expect);
    }

    #[test]
    fn merges_comment_char() {
        let comment_char = some_comment_char();

        let text = format!("a {}1b d", comment_char);
        let res = {
            let it = WordIter::new(&text, comment_char);
            collect(it)
        };
        let expect = [&format!("a {}1b", comment_char), "d"];

        assert_eq!(res, expect);
    }

    #[test]
    fn merges_en_dash() {
        let text = "a -- b";
        let expect = ["a --", "b"];
        let res = iter_collect(&text);
        assert_eq!(res, expect);
    }

    #[test]
    fn lone_comment_char_binds_left() {
        // This is a limitation of the text analysis heuristic. The test case is
        // a special-case of "merges_comment_char()" made explicit.
        //
        // The first space after a comment character marks the end of a word,
        // that comment character being the last part of the word.
        // This means that "a #1" and "a # 1" behave differently, producing
        // 1 respectively 2 words, the comment character always in the same word
        // as the preceding token:
        // - The comment character must join the preceding token to avoid being
        //   pushed onto its own line and accidentally degrading into a comment.
        // - The token after the comment character cannot join the the preceding
        //   token because we have no way to determine that that token or any of
        //   the subsequent tokens should be individual words or parts of the
        //   first token -- this is the least surprising heuristic we can apply.
        let comment_char = some_comment_char();

        let text = format!("a {} b", comment_char);
        let res = {
            let it = WordIter::new(&text, comment_char);
            collect(it)
        };
        let expect = [&format!("a {}", comment_char), "b"];

        assert_eq!(res, expect);
    }

    #[test]
    fn merges_footnote_number_references() {
        let some_num = rand::random::<u8>();
        let text = format!("a [{0}] [{0}] b", some_num);
        let expect = [&format!("a [{0}] [{0}]", some_num), "b"];
        let res = iter_collect(&text);
        assert_eq!(res, expect);
    }

    #[test]
    fn merges_footnote_text_references() {
        let text = "a [foo] [bar] b";
        let expect = ["a [foo] [bar]", "b"];
        let res = iter_collect(&text);
        assert_eq!(res, expect);
    }
}
