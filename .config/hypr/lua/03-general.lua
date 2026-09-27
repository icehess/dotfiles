
-- local color_primary = "#c7a1d8"
-- local color_surface = "#1c1822"
-- local color_secondary = "#a984c4"
-- local color_error = "#e9899d"
-- local color_tertiary = "#e0b7c9"
-- local surface_lowest = "#1e1924"


local color_primary = "#d79921"
local color_surface = "#282828"
local color_secondary = "#d3869b"
local color_error = "#fb4934"
local color_tertiary = "#83a598"
local surface_lowest = "#1d2021"


-- Refer to https://wiki.hypr.land/configuring/core/config-options/
hl.config({
    general = {
        gaps_in  = 5,
        gaps_out = 5,

        border_size = 2,

        resize_on_border = true,
        extend_border_grab_area = 120,

        allow_tearing = false,

        layout = "scrolling",

        col = {
            active_border = color_primary,
            inactive_border = color_surface,
        }
    },
    group = {
        col = {
            border_active = color_secondary,
            border_inactive = color_surface,
            border_locked_active = color_error,
            border_locked_inactive = color_surface,
        },
        groupbar = {
            font_size = 13,
            -- height = 1,
            -- text_offset = -9,
            indicator_height = 0,
            keep_upper_gap = false,
            gradients = true,
            gradient_rounding = 5,
            gradient_rounding_power = 2,
            gradient_round_only_edges = true,
            rounding = 5,
            rounding_power = 2,
            round_only_edges = true,
            col = {

                active = color_secondary,
                inactive = color_surface,
                locked_active = color_error,
                locked_inactive = color_surface,
            },
        },
    },
    decoration = {
        rounding       = 5,
        rounding_power = 2,

        -- Change transparency of focused and unfocused windows
        active_opacity   = 1.0,
        inactive_opacity = 1.0,

        shadow = {
            enabled      = true,
            range        = 4,
            render_power = 3,
            color        = 'rgba(1a1a1aee)',
        },

        blur = {
            enabled   = true,
            size      = 3,
            passes    = 1,
            vibrancy  = 0.1696,
        },
    },

    animations = {
        enabled = true,
    },

    binds = {
        allow_pin_fullscreen = true,
        allow_workspace_cycles = true,
        workspace_back_and_forth = true,
        workspace_center_on = 0,
    },
})

-- See https://wiki.hypr.land/configuring/layouts/dwindle-layout/ for more
hl.config({
    dwindle = {
        preserve_split = true, -- You probably want this
        force_split = true,
    },
})

-- See https://wiki.hypr.land/configuring/layouts/master-layout/ for more
hl.config({
    master = {
        new_status = "master",
    },
})

-- See https://wiki.hypr.land/configuring/layouts/scrolling-layout/ for more
hl.config({
    scrolling = {
        fullscreen_on_one_column = true,
        follow_focus = false,
        wrap_focus = false,
    },
})

----------------
----  MISC  ----
----------------

hl.config({
    misc = {
        force_default_wallpaper = -1,    -- Set to 0 or 1 to disable the anime mascot wallpapers
        disable_hyprland_logo   = false, -- If true disables the random hyprland logo / anime girl background. :(
        allow_session_lock_restore = true,
    },
})
