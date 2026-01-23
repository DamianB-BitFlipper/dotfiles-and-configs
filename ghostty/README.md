# Ghostty Configuration

Configuration for the [Ghostty](https://ghostty.org/) terminal emulator. The `ghostty` finds the `config` file in this directory automatically. So no need to `ln`.

## Features

- **Theme**: Homebrew
- **Emacs-like keybindings** for splits, windows, and tabs
- Copy on select to system clipboard
- macOS tabs titlebar style

## Key Bindings

| Keys | Action |
|------|--------|
| `Cmd+Shift+R` | Reload config |
| `Ctrl+X, 2` | Split down |
| `Ctrl+X, 3` | Split right |
| `Ctrl+X, K` | Close split |
| `Ctrl+X, 5, 2` | New window |
| `Cmd+Left/Right` | Navigate splits |
| `Ctrl+X, O` | Next split |
| `Ctrl+X, B` | New tab |
| `Ctrl+Cmd+Left/Right` | Previous/Next tab |
| `Cmd+A/E` | Previous/Next tab |
| `Cmd+Shift+,/.` | Scroll to top/bottom |
| `Ctrl+Up/Down` | Scroll page |

## Known Issue: Bell Sound on Ctrl+Cmd+Arrow Keys

On macOS, using `Ctrl+Cmd+Arrow` keys (used here for tab navigation) triggers a system alert/bell sound. This happens because macOS interprets these key combinations for system-wide navigation (e.g., Mission Control), and when the action doesn't apply, it plays a beep.

See: https://apple.stackexchange.com/questions/23981/command-control-arrow-beeps-plays-alert-sound-in-lion/24011#24011

### Fix

Create (or append to) `~/Library/KeyBindings/DefaultKeyBinding.dict` with the following contents:

```
{
  "@^\UF701" = "noop";
  "@^\UF702" = "noop";
  "@^\UF703" = "noop";
}
```

- `@` = Command
- `^` = Control
- `\UF701` = Down Arrow
- `\UF702` = Left Arrow
- `\UF703` = Right Arrow

After creating/modifying this file, **restart Ghostty** for the changes to take effect.
