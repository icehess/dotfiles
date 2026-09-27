hl.on("hyprland.start", function ()
  hl.exec_cmd("systemctl --user start hyprland-session.target")
  hl.exec_cmd("/usr/lib/polkit-kde-authentication-agent-1 &")
  hl.exec_cmd("noctalia")
end)

hl.on("hyprland.shutdown", function()
    os.execute("systemctl --user stop hyprland-session.target && sleep 0.1")
    -- uses a blocking exec function and sleeps a bit to give things time to close
    -- you might also want to kill troublesome/crashing non-systemd background services here:
    -- os.execute("pkill wallpaperthing; systemctl --user stop hyprland-session.target && sleep 0.1")
end)

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
hl.env("ELECTRON_OZONE_PLATFORM_HINT", "auto")
hl.env("QT_QPA_PLATFORM", "wayland")
hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")
hl.env("QT_WAYLAND_DISABLE_WINDOWDECORATION", "1")
hl.env("QS_ICON_THEME", "Mint-Y-Purple")
hl.env("SDL_VIDEO_DRIVER", "wayland")
