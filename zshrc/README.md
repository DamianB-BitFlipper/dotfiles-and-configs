# ZSH Dotfile

## Configuration

Two zshrc files are available:

- `.zshrc` - Standalone zsh configuration (no framework)
- `.oh-my-zshrc` - Oh My Zsh compatible configuration

Symlink your preferred configuration to `$HOME/.zshrc`:

```sh
# For standalone zsh
ln -sf $HOME/.config/zshrc/.zshrc $HOME/.zshrc

# For Oh My Zsh
ln -sf $HOME/.config/zshrc/.oh-my-zshrc $HOME/.zshrc
```

## Requirements

- `pipx`
- `uv`
- `direnv`
- `opencode`

### Optional

- [Oh My Zsh](https://ohmyz.sh/) (for `.oh-my-zshrc`)
- `ng` (Angular CLI)
- Google Cloud SDK

## Authentication

The zshrc requires some authenticated resources in the `~/.authinfo` file.

Copy the following and edit `<...>` accordingly:

```
machine api.anthropic.com login api-key password <api-key for inference>
machine api.openai.com login api-key password <api-key for inference>
```
