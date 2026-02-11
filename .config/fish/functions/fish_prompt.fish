function fish_prompt --description 'Write out the prompt'
    # if not set -q __fish_git_prompt_show_informative_status
    #     set -g __fish_git_prompt_show_informative_status 1
    # end
    # if not set -q __fish_git_prompt_hide_untrackedfiles
    #     set -g __fish_git_prompt_hide_untrackedfiles 1
    # end
    # if not set -q __fish_git_prompt_color_branch
    #     set -g __fish_git_prompt_color_branch magenta --bold
    # end
    # if not set -q __fish_git_prompt_showupstream
    #     set -g __fish_git_prompt_showupstream informative
    # end
    # if not set -q __fish_git_prompt_color_dirtystate
    #     set -g __fish_git_prompt_color_dirtystate blue
    # end
    # if not set -q __fish_git_prompt_color_stagedstate
    #     set -g __fish_git_prompt_color_stagedstate yellow
    # end
    # if not set -q __fish_git_prompt_color_invalidstate
    #     set -g __fish_git_prompt_color_invalidstate red
    # end
    # if not set -q __fish_git_prompt_color_untrackedfiles
    #     set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
    # end
    # if not set -q __fish_git_prompt_color_cleanstate
    #     set -g __fish_git_prompt_color_cleanstate green --bold
    # end

    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
    set -l normal (set_color normal)
    set -q fish_color_status
    or set -g fish_color_status red

    # Color the prompt differently when we're root
    set -l color_cwd $fish_color_cwd
    set -l suffix '>'
    set -l prefix '➜'
    if functions -q fish_is_root_user; and fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix '#'
        set prefix '➤'
    end

    # Write pipestatus
    # If the status was carried over (if no command is issued or if `set` leaves the status untouched), don't bold it.
    set -l bold_flag --bold
    set -q __fish_prompt_status_generation; or set -g __fish_prompt_status_generation $status_generation
    if test $__fish_prompt_status_generation = $status_generation
        set bold_flag
    end
    set __fish_prompt_status_generation $status_generation
    set -l status_color (set_color $fish_color_status)
    set -l statusb_color (set_color $bold_flag $fish_color_status)
    set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)

    # If we're running via SSH, change the host color.
    set -l _login_prompt $prefix
    if set -q SSH_TTY
        set _login_prompt $prefix " " (prompt_login)
    end

    echo -n -s (set_color brmagenta) $_login_prompt (set_color normal) " " (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status " "$suffix " "
    # echo -n -s (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
end
