# ZSH Dotfile

## Configuration

Symlink the configuration to `$HOME/.zshrc`:

```sh
ln -sf $HOME/.config/zshrc/.oh-my-zshrc $HOME/.zshrc
```

### Custom Plugins

Clone the following plugins into Oh My Zsh custom plugins:

```sh
git clone git@github.com:hsaunders1904/pyautoenv.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/pyautoenv
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
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
