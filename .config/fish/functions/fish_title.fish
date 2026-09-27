function fish_title \
    --description "Set title to current folder and shell name"
    set --local current_folder (prompt_pwd)
    set --local current_command (status current-command 2>/dev/null; or echo $_)[1]

    echo "$TERM: $current_folder $pure_symbol_title_bar_separator $current_command"
end
