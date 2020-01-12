# Changelog

`commitmsgfmt` formats commit messages. It reflows and wraps text, with special
understanding of patterns often seen in commit messages.

## 1.3.0 - 2020-01-12

- If the subject line matches a prefix recognized by `git rebase --autosquash`
  (`fixup! ` or `squash! `), preserve it in its entirety. Format the rest of
  the message as usual; it may be that the user is annotating the "fixup"
  commit for review purposes.

## 1.2.0 - 2018-09-28

- When wrapping a line, only do so at the last space before the line length
  limit; do not break any words to remain within the limit. "Words" that extend
  beyond the limit are not likely to be meaningfully breakable in the first
  place, and breaking is actively detrimental to URLs. This change alone yields
  a 6-8 times speed-up as reported by `time(1)`.

- _References_ may span multiple lines, subsequent lines following the same
  indentation rules as _list items_. The syntactical unit has been renamed to
  _footnote_ to better capture the new functionality.

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
