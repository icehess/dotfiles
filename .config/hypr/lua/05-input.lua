---------------
---- INPUT ----
---------------

hl.config({
    input = {
        kb_layout  = "us,ir",
        kb_variant = "",
        kb_model   = "",
        kb_options = "grp:alt_caps_toggle",
        kb_rules   = "",
        repeat_delay = 350,
        repeat_rate = 70,
        numlock_by_default = true,

        follow_mouse = 1,

        sensitivity = 0.6,
        accel_profile = "adaptive",

        touchpad = {
          natural_scroll = true,
          disable_while_typing = false,
          drag_3fg = true,
          drag_lock = 1,
          scroll_factor = 2.5,
        },
    },
})

hl.gesture({
    fingers = 3,
    direction = "horizontal",
    action = "workspace"
})
