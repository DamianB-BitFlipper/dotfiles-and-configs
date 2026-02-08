# Tmux Configuration

Emacs-style tmux config with Homebrew aesthetic.

## Installation

```bash
# Install tmux
brew install tmux

# Link config (tmux looks for ~/.tmux.conf by default)
ln -s ~/.config/tmux/tmux.conf ~/.tmux.conf

# Install TPM (Tmux Plugin Manager)
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Install libtmux (required for tmux-window-name plugin)
uv pip install --system --break-system-packages libtmux
```

## Post-Install

1. Start tmux: `tmux`
2. Install plugins: `C-x I` (prefix + Shift+i)

## Key Bindings

Prefix: `C-x` (Ctrl+x)

### Windows (Emacs-style)

| Binding | Action |
|---------|--------|
| `C-x 2` | Split below |
| `C-x 3` | Split right |
| `C-x 0` | Close pane |
| `C-x 1` | Zoom/maximize pane |
| `C-x o` | Cycle panes |
| `C-x k` | Kill pane (confirm) |

### Window Management

| Binding | Action |
|---------|--------|
| `C-x c` | New window |
| `C-x n` | Next window |
| `C-x p` | Previous window |
| `C-x b` | Window list |

### Session

| Binding | Action |
|---------|--------|
| `C-x s` | Choose session |
| `C-x d` | Detach |
| `C-x q q` | Quit tmux |

### Navigation (No Prefix)

| Binding | Action |
|---------|--------|
| `Alt+Arrow` | Navigate panes |
| `Shift+Arrow` | Resize panes |

### Misc

| Binding | Action |
|---------|--------|
| `C-x C-h` | Show all keybindings (help) |
| `C-x r` | Reload config |
| `C-x I` | Install plugins (TPM) |
| `C-x U` | Update plugins (TPM) |

## Plugins

- **tmux-sensible** - Sensible defaults
- **tmux-yank** - Clipboard support
- **tmux-window-name** - Smart automatic window naming based on current program/directory
- **tmux-resurrect** - Save/restore sessions
- **tmux-continuum** - Auto-save sessions
