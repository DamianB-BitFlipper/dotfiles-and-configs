# Configuration

Install `sketchybar` following: https://github.com/FelixKratz/SketchyBar

The `sketchybar` finds the `sketchybarrc` in this directory automatically. So no need to `ln`.

Fonts will be broken and need to be installed.
`brew install --cask font-hack-nerd-font`

To reload `sketchybar` when the configuration has been changed use: `sketchybar --reload`

## WiFi Name

To get the WiFi name to display correctly, you need to enable verbose mode for ipconfig:

```bash
sudo ipconfig setverbose 1
```
