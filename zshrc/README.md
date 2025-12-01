# ZSH Dotfile

## Configuration

Symlink the configuration to `$HOME/.zshrc`:

```sh
ln -sf $HOME/.config/zshrc/.oh-my-zshrc $HOME/.zshrc
```

## Requirements

- [Oh My Zsh](https://ohmyz.sh/)
- `pipx`
- `uv`
- `direnv`
- `opencode`

### Optional

- `ng` (Angular CLI)
- Google Cloud SDK

## Authentication

The zshrc requires some authenticated resources in the `~/.authinfo` file.

Copy the following and edit `<...>` accordingly:

```
machine api.anthropic.com login api-key password <api-key for inference>
machine api.openai.com login api-key password <api-key for inference>
```
