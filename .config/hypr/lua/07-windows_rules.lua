--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- See https://wiki.hypr.land/configuring/core/rules/


-- Noctalia Settings
hl.window_rule({
    match = { class = "dev.noctalia.Noctalia" },
    float = true,
    size = { 1080, 920 },
})

hl.layer_rule({
  name = "noctalia",
  match = {
    namespace = "^noctalia-(bar-.+|notification|dock|panel|attached-panel|osd|window-switcher)$",
  },
  no_anim = true,
  ignore_alpha = 0.5,
  blur = true,
  blur_popups = true,
})

-- Ref https://wiki.hypr.land/configuring/core/rules/workspace-rules/
-- "Smart gaps" / "No gaps when only"
-- uncomment all if you wish to use that.
hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "f[1]",   gaps_out = 0, gaps_in = 0 })
hl.window_rule({
    name  = "no-gaps-wtv1",
    match = { float = false, workspace = "w[tv1]" },
    border_size = 0,
    rounding    = 0,
})
hl.window_rule({
    name  = "no-gaps-f1",
    match = { float = false, workspace = "f[1]" },
    border_size = 0,
    rounding    = 0,
})

-- local suppressMaximizeRule = hl.window_rule({
--     -- Ignore maximize requests from all apps. You'll probably like this.
--     name  = "suppress-maximize-events",
--     match = { class = ".*" },

--     suppress_event = "maximize",
-- })
-- suppressMaximizeRule:set_enabled(false)

hl.window_rule({
    -- Fix some dragging issues with XWayland
    name  = "fix-xwayland-drags",
    match = {
        class      = "^$",
        title      = "^$",
        xwayland   = true,
        float      = true,
        fullscreen = false,
        pin        = false,
    },

    no_focus = true,
})

-- Hyprland-run windowrule
hl.window_rule({
    name  = "move-hyprland-run",
    match = { class = "hyprland-run" },

    move  = "20 monitor_h-120",
    float = true,
})


hl.window_rule({
    name  = "Browser's picture-in-picture",
    match = { title = "^picture-in-picture|picture in picture|Picture in picture$" },

    move  = {"(monitor_w-(window_w+210))", "(monitor_h-(window_h+170))"},
    size = {"(monitor_w*0.30)", "(monitor_h*0.30)"},
    float = true,
})
hl.window_rule({
    name  = "Nautilus save dialog",
    match = {
        initial_class = "^xdg-desktop-portal-gtk$",
        initial_title = "^All Files",
    },

    size = {"(monitor_w*0.50)", "(monitor_h*0.50)"},
    float = true,
    center = true,
})
hl.window_rule({
    name  = "Blackout password managers in screen capture",
    match = {
        initial_class = "^com.onepassword.OnePassword$",
    },
    no_screen_share = true,
})

-- window-rule {
--     match app-id=r#"^org\.keepassxc\.keepassxc$"#
--     match app-id=r#"^org\.gnome\.world\.secrets$"#
--     match app-id=r#"^1password$"#
--     block-out-from "screen-capture"
-- }
