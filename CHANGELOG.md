# Changelog

`commitmsgfmt` formats commit messages. It reflows and wraps text, with special
understanding of patterns often seen in commit messages.

## 1.1.0 - 2018-08-25

- #3: If the `core.commentChar` setting is set to an explicit character, use
  that for identifying comments. Default to `#` when unset or set to the
  special `auto` value that makes Git choose, among 10 possible candidates, the
  first character to not occur in the current message's first column.

- Advise on Vim integration regarding `'textwidth'` and `'wrap'` settings.

## 1.0.2 - 2018-08-22

- #4: Fix AppVeyor build failure.
- #2: Fix "scissor" formatting for `commit --verbose`.

## 1.0.1 - 2018-08-20

- #1: Fix continuation indentation of tab-indented list items

## 1.0.0 - 2018-05-09

- First public release of `commitmsgfmt`; the "make it work" version.
