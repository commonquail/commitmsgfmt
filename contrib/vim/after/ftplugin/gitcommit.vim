" THIS FILE IS AN EXAMPLE. It is not intended to be blindly copied or
" symlinked to your user runtime; expect breakage if you do so.

" This is the absolute minimal required setting for commitmsgfmt.
" Either ensure commitmsgfmt is on $PATH or specify absolute path.
setlocal formatprg=commitmsgfmt

" The rest of this file demonstrates additional, strictly optional settings
" that may prove useful.

" On <LocalLeader>gq, reformat the entire buffer and return the cursor to its
" position before reformatting (as best as possible).
"
" This mapping is very helpful for working around inherent limitations in Vim
" and commitmsgfmt:
"
" - gqip et al. do not transmit any context information, only the text to
"   format, so to commitmsgfmt the formatted range appears to be the entire
"   message and it will apply the subject-line special case accordingly. The
"   only way to avoid this is to include the actual subject line in the input
"   to commitmsgfmt. Removing this special case from commitmsgfmt would defeat
"   much of its purpose.
"
" - gggqG is the most reliable way of formatting the entire buffer but it is
"   both highly annoying and error-prone to type and leaves your cursor at the
"   end of the document.
nnoremap <buffer> <silent> <LocalLeader>gq :let b:cursorpos=winsaveview()<CR>gggqG:call winrestview(b:cursorpos)<CR>

" Sibling mapping to the above, to not wreck all muscle memory built up for
" gqip.
nmap <buffer> gqip <LocalLeader>gq

" To override the default message body width, instead use
setlocal formatprg=commitmsgfmt\ --width=42

" If you want to use the value of 'textwidth', instead use
let &l:formatprg='commitmsgfmt --width=' . &l:textwidth

" commitmsgfmt was not written to work with merge commits, which, in the Git
" and Linux kernel projects, often use special, hard-to-detect constructs not
" seen in non-merge commits. If you only ever write merge commit messages like
" non-merge commit messages you will have no problem and can ignore this.
" Otherwise, this guard is useful for conditionally enabling commitmsgfmt.
"
" Mind, Vim doesn't handle those problematic constructs especially well
" either.
if filereadable(fnamemodify(@%, ':p:h') . '/MERGE_HEAD')
  " Merge commit. Consider not enabling commitmsgfmt here.

  " If you ever want to use plain Vim, these settings along with the default
  " 'filetype' runtime settings worked best for me.
  "
  " Let gq format comments, so comments reflow but don't get uncommented; and
  " numbered lists, with the number-list pattern amended to include unordered
  " lists.
  setlocal formatoptions+=q
  setlocal formatoptions+=n
  setlocal formatlistpat=^\\s*[-*0-9]\\+[\]:.)}\\t\ ]\\s*
else
  " Non-merge commit.
  setlocal formatprg=commitmsgfmt
endif
