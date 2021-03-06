" Author: Magnus Ottenklinger - https://github.com/evnu

let g:ale_erlang_erlc_options = get(g:, 'ale_erlang_erlc_options', '')
let g:ale_erlang_erlc_env_vars = get(g:, 'ale_erlang_erlc_env_vars', '')

function! ale_linters#erlang#kazoo_erlc#GetCommand(buffer) abort
    let l:output_file = ale#util#Tempname()
    call ale#command#ManageFile(a:buffer, l:output_file)

    let l:cmd = ale#Var(a:buffer, 'erlang_erlc_env_vars')
    \   . 'erlc -o ' . ale#Escape(l:output_file)
    \   . ' -I' . expand('%:p:h')
    \   . ' -I' . expand('%:p:h') . '/../include'
    \   . ' -I' . expand('%:p:h') . '/../ '
    \   . ale#Var(a:buffer, 'erlang_erlc_options')
    \   . ' %t'

    return l:cmd
endfunction

function! ale_linters#erlang#kazoo_erlc#Handle(buffer, lines) abort
    " Matches patterns like the following:
    "
    " error.erl:4: variable 'B' is unbound
    " error.erl:3: Warning: function main/0 is unused
    " error.erl:4: Warning: variable 'A' is unused
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+): (Warning: )?(.+)$'

    " parse_transforms are a special case. The error message does not indicate a location:
    " error.erl: undefined parse transform 'some_parse_transform'
    let l:pattern_parse_transform = '\v(undefined parse transform .*)$'
    let l:output = []

    let l:pattern_no_module_definition = '\v(no module definition)$'
    let l:pattern_unused = '\v(.* is unused)$'

    let l:is_hrl = fnamemodify(bufname(a:buffer), ':e') is# 'hrl'

    for l:line in a:lines
        let l:match = matchlist(l:line, l:pattern)

        " Determine if the output indicates an error. We distinguish between two cases:
        "
        " 1) normal errors match l:pattern
        " 2) parse_transform errors match l:pattern_parse_transform
        "
        " If none of the patterns above match, the line can be ignored
        if len(l:match) == 0 " not a 'normal' warning or error
            let l:match_parse_transform = matchlist(l:line, l:pattern_parse_transform)

            if len(l:match_parse_transform) == 0 " also not a parse_transform error
                continue
            endif

            call add(l:output, {
            \   'bufnr': a:buffer,
            \   'lnum': 0,
            \   'col': 0,
            \   'type': 'E',
            \   'text': l:match_parse_transform[0],
            \})

            continue
        endif

        let l:line = l:match[2]
        let l:warning_or_text = l:match[3]
        let l:text = l:match[4]

        " If this file is a header .hrl, ignore the following expected messages:
        " - 'no module definition'
        " - 'X is unused'
        if l:is_hrl && (
        \   match(l:text, l:pattern_no_module_definition) != -1
        \   || match(l:text, l:pattern_unused) != -1
        \)
            continue
        endif

        if !empty(l:warning_or_text)
            let l:type = 'W'
        else
            let l:type = 'E'
        endif

        call add(l:output, {
        \   'bufnr': a:buffer,
        \   'lnum': l:line,
        \   'col': 0,
        \   'type': l:type,
        \   'text': l:text,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('erlang', {
\   'name': 'kazoo_erlc',
\   'executable': 'erlc',
\   'command': function('ale_linters#erlang#kazoo_erlc#GetCommand'),
\   'callback': 'ale_linters#erlang#kazoo_erlc#Handle',
\})
