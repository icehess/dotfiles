" Elixir filetype plugin
" Language: HEEx
" Maintainer:	Mitchell Hanberg <vimNOSPAM@mitchellhanberg.com>
" Last Change: 2022 Sep 21

" if exists("b:did_ftplugin")
"   finish
" endif
" let b:did_ftplugin = 1

setlocal shiftwidth=2 softtabstop=2 expandtab

setlocal comments=:<%!--
setlocal commentstring=<%!--\ %s\ --%>

" HTML: thanks to Johannes Zellner and Benji Fisher.
if exists("loaded_matchit") && !exists("b:match_words")
  let b:match_ignorecase = 1
  let b:match_words = '<!--:-->,' ..
	\	      '<:>,' ..
	\	      '<%:%>,' ..
	\	      '<\.[a-b_]+:>,' ..
	\	      '<\@<=[ou]l\>:<\@<=/[ou]l>,' ..
	\	      '<\@<=dl\>[^>]*\%(>\|$\):<\@<=d[td]\>:<\@<=/dl>,' ..
	\	      '<\@<=\([^/!][^ \t>]*\)[^>]*\%(>\|$\):<\@<=/\1>'
  let b:html_set_match_words = 1
  " let b:undo_ftplugin ..= " | unlet! b:match_ignorecase b:match_words b:html_set_match_words"
endif

" let b:undo_ftplugin = 'set sw< sts< et< com< cms<'
