# Configuration Files for a Tiling Window Manager on Mac

Be sure to ln the `.skhdrc` and `.yabairc` to `$HOME`

`ln -s $HOME/.config/tiling-window-manager/.skhdrc $HOME/.skhdrc`
`ln -s $HOME/.config/tiling-window-manager/.yabairc $HOME/.yabairc`

# If you get an error, usually upon rebooting, that the scripting-addition failed, simply run
sudo yabai --load-sa

Somtimes you may need to first `sudo yabai --uninstall-sa` before running `--load-sa` to get things working.
