------------------
---- MONITORS ----
------------------

-- monitor=,preferred,auto,auto

-- monitor= eDP-1, preferred, 3840x0, 1.6

-- See https://wiki.hypr.land/configuring/core/monitors/
hl.monitor({
    output   = "eDP-1",
    mode     = "preferred",
    position = "3840x0",
    scale    = "1.6",
    icc      = "/usr/share/color/icc/colord/BOE0CB4.icm",
})
