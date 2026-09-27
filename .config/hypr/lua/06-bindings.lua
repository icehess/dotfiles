---------------------
---- KEYBINDINGS ----
---------------------

local terminal    = "foot"
local fileManager = "nautilus"
local ipc = "noctalia msg "

-- Example binds, see https://wiki.hypr.land/configuring/core/binds/ for more

local mainMod = "SUPER" -- Sets "Windows" key as main modifier

hl.bind(mainMod .. " + E", hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + D", hl.dsp.exec_cmd(ipc .. "panel-toggle launcher"))

-- window
hl.bind(mainMod .. " + T", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + Return", hl.dsp.exec_cmd(terminal))
local closeWindowBind = hl.bind(mainMod .. " + W", hl.dsp.window.close())
-- closeWindowBind:set_enabled(false)
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.window.kill())
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + F", hl.dsp.window.fullscreen_state({ action = "toggle", internal = 1, client = 0 }))
hl.bind(mainMod .. " + SHIFT + F", hl.dsp.window.fullscreen_state({ action = "toggle", internal = 2, client = 0 }))

-- Move focus with mainMod + arrow keys
-- hl.bind(mainMod .. " + left",  hl.dsp.focus({ direction = "left" }))
-- hl.bind(mainMod .. " + right", hl.dsp.focus({ direction = "right" }))
-- hl.bind(mainMod .. " + up",    hl.dsp.focus({ direction = "up" }))
-- hl.bind(mainMod .. " + down",  hl.dsp.focus({ direction = "down" }))
hl.bind(mainMod .. " + left",  hl.dsp.layout("focus left"))
hl.bind(mainMod .. " + right", hl.dsp.layout("focus right"))
hl.bind(mainMod .. " + up",    hl.dsp.layout("focus up"))
hl.bind(mainMod .. " + down",  hl.dsp.layout("focus down"))

hl.bind(mainMod .. " + SHIFT + left",  hl.dsp.focus({ monitor = "left" }))
hl.bind(mainMod .. " + SHIFT + right", hl.dsp.focus({ monitor = "right" }))
hl.bind(mainMod .. " + SHIFT + up",    hl.dsp.focus({ monitor = "up" }))
hl.bind(mainMod .. " + SHIFT + down",  hl.dsp.focus({ monitor = "down" }))

-- Move window
hl.bind(mainMod .. " + CTRL + left", hl.dsp.window.move({ direction = "left", group_aware = true }))
hl.bind(mainMod .. " + CTRL + right", hl.dsp.window.move({ direction = "right", group_aware = true }))
hl.bind(mainMod .. " + CTRL + up", hl.dsp.window.move({ direction = "up", group_aware = true }))
hl.bind(mainMod .. " + CTRL + down", hl.dsp.window.move({ direction = "down", group_aware = true }))

hl.bind(mainMod .. " + SHIFT + CTRL + left",  hl.dsp.window.move({ monitor = "left" }))
hl.bind(mainMod .. " + SHIFT + CTRL + right", hl.dsp.window.move({ monitor = "right" }))
hl.bind(mainMod .. " + SHIFT + CTRL + up",    hl.dsp.window.move({ monitor = "up" }))
hl.bind(mainMod .. " + SHIFT + CTRL + down",  hl.dsp.window.move({ monitor = "down" }))


hl.bind(mainMod .. " + ALT + left",   hl.dsp.layout("consume_or_expel prev"))
hl.bind(mainMod .. " + ALT + right",   hl.dsp.layout("consume_or_expel next"))
hl.bind(mainMod .. " + CTRL + A",   hl.dsp.group.toggle())
hl.bind(mainMod .. " + A",   hl.dsp.group.next())
hl.bind(mainMod .. " + SHIFT + A",   hl.dsp.group.move_window())

hl.bind(mainMod .. " + R", hl.dsp.layout("colresize +conf"))
hl.bind(mainMod .. " + SHIFT + R", hl.dsp.submap("resize"))

-- Start a submap called "resize".
hl.define_submap("resize", function()

    -- Set repeating binds for resizing the active window.
    hl.bind("right", hl.dsp.window.resize({ x = 10, y = 0, relative = true}), { repeating = true })
    hl.bind("left", hl.dsp.window.resize({ x = -10, y = 0, relative = true}), { repeating = true })
    hl.bind("up", hl.dsp.window.resize({ x = 0, y = 10, relative = true}), { repeating = true })
    hl.bind("down", hl.dsp.window.resize({ x = 0, y = -10, relative = true}), { repeating = true })

    -- Use `reset` to go back to the global submap
    hl.bind("escape", hl.dsp.submap("reset"))

end)

-- dwindle
hl.bind(mainMod .. " + P", hl.dsp.window.pseudo())
hl.bind(mainMod .. " + J", hl.dsp.layout("togglesplit"))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + " .. key,             hl.dsp.focus({ workspace = i}))
    hl.bind(mainMod .. " + CTRL + " .. key,     hl.dsp.window.move({ workspace = i }))
end

-- Example special workspace (scratchpad)
hl.bind(mainMod .. " + S",         hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + CTRL + S", hl.dsp.window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Noctalia
hl.bind("ALT + Tab", hl.dsp.exec_cmd(ipc .. "window-switcher"))

-- Laptop multimedia keys for volume and LCD brightness
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd(ipc .. "volume-up"))
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd(ipc .. "volume-down"))
hl.bind("XF86AudioMute", hl.dsp.exec_cmd(ipc .. "volume-mute"))
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd(ipc .. "msg mic-mute"))
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd(ipc .. "brightness-up"))
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd(ipc .. "brightness-down"))


hl.bind("XF86AudioNext", hl.dsp.exec_cmd(ipc .. "msg media next"))
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd(ipc .. "msg media previous"))
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd(ipc .. "msg media toggle"))
hl.bind("XF86AudioPause", hl.dsp.exec_cmd(ipc .. "msg media toggle"))


-- Per-layout binds
local function layout_bind(bind_table)
    return function ()
        local workspace = hl.get_active_special_workspace() or
                          hl.get_active_workspace()

        if not workspace then
            return
        end

        local layout = workspace.tiled_layout

        if bind_table[layout] then
            hl.dispatch(bind_table[layout])
        end
    end
end
-- /Per-layout binds

-- Windows Magnifier-like cursor zoom
local MAX_ZOOM = 3
local MIN_ZOOM = 1
local ZOOM_TOGGLE_FACTOR = 1.5

---@param offset number
---@return nil
local function zoom(offset)
    local current = hl.get_config("cursor.zoom_factor")
    if offset ~= nil then
        current = current + offset
    elseif current ~= MIN_ZOOM then
        current = MIN_ZOOM
    else
        current = ZOOM_TOGGLE_FACTOR
    end
    current = math.max(MIN_ZOOM, math.min(MAX_ZOOM, current))
    hl.config({ cursor = { zoom_factor = current } })
end

hl.bind("SUPER + SHIFT + equal", function()
    zoom(0.5)
end)
hl.bind("SUPER + SHIFT + minus", function()
    zoom(-0.5)
end)
-- /Windows Magnifier-like cursor zoom
