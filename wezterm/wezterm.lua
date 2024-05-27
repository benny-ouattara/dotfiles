local wezterm = require("wezterm")
local action = wezterm.action

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.window_decorations = "RESIZE"
config.color_scheme = 'Catppuccin Mocha'
config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.default_cursor_style = 'SteadyBar'
config.font = wezterm.font_with_fallback({
  {
    family = 'IosevkaTerm Nerd Font',
    weight = "Regular"
  },
  {
    family = "Monaco",
    weight = "Regular"
  }
})

config.harfbuzz_features = { 'calt=1', 'clig=1', 'liga=1' }
config.send_composed_key_when_left_alt_is_pressed = true
config.font_size = 23.0
config.cell_width = 0.88888888887 -- 8 / 9 rounded down
config.front_end = "WebGpu"
config.animation_fps = 1

config.leader = { key = 'Space', mods = 'SHIFT|CTRL' }
config.audible_bell = "Disabled"

config.keys = {
  {
    key = 'j',
    mods = 'CTRL',
    action = action.ActivatePaneDirection "Left"
  },
  {
    key = 'k',
    mods = 'CTRL',
    action = action.ActivatePaneDirection "Right"
  },
  {
    key = 'l',
    mods = 'CTRL',
    action = action.ActivatePaneDirection "Down"
  },
  {
    key = 'h',
    mods = 'CTRL',
    action = action.ActivatePaneDirection "Down"
  },
  {
    key = 's',
    mods = 'CTRL',
    action = action.SplitHorizontal { domain = "CurrentPaneDomain" }
  },
  {
    key = 'v',
    mods = 'CTRL',
    action = action.SplitVertical { domain = "CurrentPaneDomain" }
  },
  {
    key = 'f',
    mods = 'CTRL',
    action = action.TogglePaneZoomState
  },
  {
    key = 'l',
    mods = 'CTRL',
    action = action.ClearScrollback 'ScrollbackAndViewport',
  }
}
return config
