# ZSH Dotfile

## Installation

First install [Oh My Zsh](https://ohmyz.sh/)

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
git clone https://github.com/DamianB-BitFlipper/zsh-ai.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-ai
```

### Pure Theme

Clone the Pure prompt theme:

```sh
mkdir -p "$HOME/.zsh"
git clone https://github.com/sindresorhus/pure.git "$HOME/.zsh/pure"
```

### ZSH AI Plugin

The [zsh-ai](https://github.com/DamianB-BitFlipper/zsh-ai) plugin provides AI-powered shell suggestions using [copilot-api](https://github.com/ericc-ch/copilot-api) as the backend.

This assumes you have a valid GitHub Copilot subscription.

Install copilot-api via pnpm:

```sh
pnpm config set global-bin-dir /opt/homebrew/bin
pnpm install -g copilot-api
```

Run `copilot-api start` to authenticate and verify it works.

Symlink the Copilot API plist to LaunchAgents to run copilot-api on startup:

```sh
ln -sf $HOME/.config/zshrc/com.damianb-bitflipper.copilot-api.plist ~/Library/LaunchAgents/
```

The plugin is configured via environment variables in `.oh-my-zshrc`:

```sh
export ZSH_AI_PROVIDER="openai-compatible"
export ZSH_AI_OPENAI_MODEL="claude-haiku-4.5"
export ZSH_AI_OPENAI_URL="http://localhost:4141/v1/chat/completions"
```

The URL `http://localhost:4141` is the local copilot-api server endpoint.

## Requirements

- [Oh My Zsh](https://ohmyz.sh/)
- `pipx`
- `uv`
- `direnv`
- `thefuck` (`brew install thefuck`)
- `fzf` (`brew install fzf`)
- `zoxide` (`brew install zoxide`)
- `opencode`
- `tuitube` (https://github.com/remorses/tuitube)
- `bun`

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
