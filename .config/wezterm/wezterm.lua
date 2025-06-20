-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end
local act = wezterm.action

-- config.debug_key_events = true
config.use_ime = false

config.initial_cols = 150
config.initial_rows = 40

local font_size = 11

if string.find(wezterm.target_triple, 'darwin') then
  font_size = 15
end

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'GruvboxDarkHard'

config.font = wezterm.font_with_fallback {
	    'FantasqueSansM Nerd Font Mono',
	    'FantasqueSansM Nerd Font',
	   }
config.font_size = font_size
-- config.freetype_load_target = 'HorizontalLcd'

config.window_background_opacity = 0.9

config.scrollback_lines = 200000

-- config.window_decorations = "RESIZE"
-- config.window_decorations = "INTEGRATED_BUTTONS | RESIZE"
config.enable_wayland = false

-- config.audible_bell = "SystemBeep"

config.keys = {
  -- Clears the scrollback and viewport, and then sends CTRL-L to ask the
  -- shell to redraw its prompt
  {
    key = 'k',
    -- mods = 'CTRL|SHIFT',
    mods = 'CMD',
    action = act.Multiple {
      act.ClearScrollback 'ScrollbackAndViewport',
      act.SendKey { key = 'L', mods = 'CTRL' },
    },
  },
}

config.window_frame = {
  font = wezterm.font_with_fallback {
	    'FantasqueSansM Nerd Font Mono',
	    'FantasqueSansM Nerd Font',
	   },
  font_size = font_size,
}

config.mouse_bindings = {
    -- Disable the default click behavior
    {
      event = { Up = { streak = 1, button = "Left"} },
      mods = "NONE",
      action = wezterm.action.CompleteSelection("ClipboardAndPrimarySelection"),
    },
    -- Ctrl-click will open the link under the mouse cursor
    {
        event = { Up = { streak = 1, button = "Left" } },
        mods = "CTRL",
        action = wezterm.action.OpenLinkAtMouseCursor,
    },
    -- Disable the Ctrl-click down event to stop programs from seeing it when a URL is clicked
    {
        event = { Down = { streak = 1, button = "Left" } },
        mods = "CTRL",
        action = wezterm.action.Nop,
    },
}
-- config.xcursor_theme = 'Bibata-Modern-Classic'

-- and finally, return the configuration to wezterm

return config
