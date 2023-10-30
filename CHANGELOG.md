# Changelog

`commitmsgfmt` formats commit messages. It reflows and wraps text, with special
understanding of patterns often seen in commit messages.

## Unreleased

- #6: Recognize lines that begin with `>` as _block quotes_ and preserve them
  in their entirety, and allow them to follow a preceding paragraph without the
  empty line that is otherwise usually required.

- #7: Recognize fenced code blocks with backtick code fences (` ``` `) and
  preserve them in their entirety. Do not recognize tilde code fences (`~~~`),
  which are virtually never used in practice and which would interfere with
  many other uses. Per CommonMark 0.3.0 a code fence must be at least three
  characters long and may optionally be indented up to three spaces, and the
  closing code fence must be at least as long as the opening code fence
  ignoring whitespace.

- If `--width` is specified multiple times, ignore all but the last occurrence.

- A large number of straightforward optimizations reduce total third-party
  dependency count; compilation time; execution time; and dynamic memory
  allocations. See below for details.

The optimization impact is measured with `hyperfine` and Valgrind's massif for
three distinct inputs: an empty file, a file that corresponds to the 3rd
quartile of message sizes, and Project Gutenberg's _Don Quixote_ in plain-text
[[quixote]], where

    $ wc corpus-* | grep [.]
         14      82     577 corpus-3rd-qu.txt
      43285  430276 2391727 corpus-don-quixote.txt
          0       0       0 corpus-empty.txt

    $ sha256sum corpus-*
    ff90dc6f56b02faa2f67a9b937ba935ea62f246e0165d7212b2d0e500668f40b  corpus-3rd-qu.txt
    bc899c11c714f764f90aaddfe08e1b50d67812da8e54bc12c814a4544ae0c8ff  corpus-don-quixote.txt
    e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855  corpus-empty.txt

Largely by removing the `clap` and `regex` packages `commitmsgfmt` has become
considerably leaner across the board, and without incurring undue future
maintenance cost:

    VERSION   | PACKAGES | BINARY SIZE | `.text` SECTION | MEAN BUILD TIME
              |          | stripped    |                 | debug    release
    ----------+----------+-------------+-----------------+-----------------
    1.5.0     | 42       | 2103792 b   | 1126.4 KiB      | 3.073 s  4.525 s
    1.6.0     | 29       |  494032 b   |  334.3 KiB      | 0.910 s  1.143 s
    1.6.0 (%) | 70%      | 23%         | 30%             | 30%      25%


    VERSION   | MEAN EXECUTION TIME         | TOTAL ALLOCATIONS
              | empty   3rd qu  don-quixote | empty        3rd          don-quixote
    ----------+-----------------------------+---------------------------------------
    1.5.0     | 7.1 ms  7.6 ms  67.4 ms     | 4,944,024 B  5,531,448 B  29,304,608 B
    1.6.0     | 4.9 ms  4.6 ms  52.6 ms     |    15,736 B     36,760 B  22,015,736 B
    1.6.0 (%) | 69%     61%     78%         | 0%           1%           75%


The `clap` package contributed the most to build time. The `regex` package
contributed the most to execution time and dynamic allocations. Neither
discovery surprised.

[quixote]: https://www.gutenberg.org/cache/epub/996/pg996.txt

## 1.5.0 - 2022-07-30

- Fix an edge case where footnote references followed by punctuation would
  circumvent the prevention of undesirable line breaking added in v1.4.0. This
  meant that `foo [1].` could wrap while `foo [1]` could not; now neither can.

- Extend the range of non-breakable tokens to include any sequence of ASCII
  punctuation. Previously `foo ...` could wrap; now it cannot.

- Restrict the definition of footnotes to require a vertical space after
  a preceding paragraph. This prevents footnote references that have ended up
  at the start of a line from degenerating into footnotes. That is, previously
  `foo\n[1] bar` would be a paragraph followed by a footnote whereas now it is
  a single paragraph containing a footnote reference.

## 1.4.1 - 2021-04-05

- Fix an errant man page header accidentally added in v1.4.0.

## 1.4.0 - 2021-04-05

- #5: Prevent breaking lines between certain types of tokens. This increases
  safety by preventing a line break from accidentally creating a comment line,
  and improves legibility by preserving some typographic constructs' context,
  namely footnote references and _en_-dashes.

- Fix an unlikely subtraction overflow error where a continuation line required
  indentation beyond what the specified message body permitted.

- Build release binaries with GitHub Actions instead of Travis CI and AppVeyor
  and remove the now obsolete Travis CI and AppVeyor integrations. This change
  restores precompiled macOS and Windows binaries. BSD binaries are likely gone
  for good but rarely compiled successfully on Travis CI anyway.

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
