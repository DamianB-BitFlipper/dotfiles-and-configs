# Configuration Files for a Tiling Window Manager on Mac

Install AeroSpace following: https://github.com/nikitabobko/AeroSpace

Be sure to ln the `.aerotspace.toml` to `$HOME`

`ln -s $HOME/.config/aerospace-window-manager/.aerospace.toml $HOME/.aerospace.toml`

## Required macOS Settings

### Displays have separate Spaces

When multiple instances of the same application are open (e.g., multiple terminals), AeroSpace may focus the wrong window. To fix this, you must change the "Displays have separate Spaces" setting:

1. Open **System Settings** (or System Preferences on older macOS)
2. Go to **Desktop & Dock**
3. Scroll down to the **Mission Control** section
4. Toggle **"Displays have separate Spaces"** on or off

**Note:** You'll need to log out and log back in for the change to take effect.
