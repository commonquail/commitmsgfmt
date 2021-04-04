/// An iterator over "words" in some text. A word is generally a sequence of non-whitespace
/// characters bounded by either whitespace characters or the empty string. However, this is not a
/// definition and the exact details are unspecified.
pub(crate) struct WordIter<'text> {
    naive_words: core::str::Split<'text, char>,
}

impl<'text> WordIter<'text> {
    pub fn new(text: &'text str) -> Self {
        WordIter {
            naive_words: text.split(' '),
        }
    }
}

impl<'text> Iterator for WordIter<'text> {
    type Item = &'text str;

    fn next(&mut self) -> Option<Self::Item> {
        self.naive_words.next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    type Item<'text> = <WordIter<'text> as Iterator>::Item;

    fn iter(text: &str) -> WordIter {
        WordIter::new(&text)
    }

    fn collect(it: WordIter) -> Vec<Item> {
        it.collect()
    }

    fn iter_collect(text: &str) -> Vec<Item> {
        collect(iter(&text))
    }

    #[test]
    fn smoke() {
        let text = "a b #1 c [1] d";
        let expect = ["a", "b", "#1", "c", "[1]", "d"];
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
        let expect = ["a", "", "", "b"];
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
}
