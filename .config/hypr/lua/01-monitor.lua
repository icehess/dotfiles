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

hl.monitor({
    output = "DP-2",
    mode = "1920x1200",
    scale = "1.0",
    position = "0x0",
    icc = "/usr/share/ghostscript/iccprofiles/srgb.icc",
})

hl.monitor({
    output = "DP-3",
    mode = "1920x1200",
    scale = "1.0",
    position = "1920x0",
    icc = "/usr/share/ghostscript/iccprofiles/srgb.icc",
})
