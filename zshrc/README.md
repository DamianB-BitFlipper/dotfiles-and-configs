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

### Pure Theme

Clone the Pure prompt theme:

```sh
mkdir -p "$HOME/.zsh"
git clone https://github.com/sindresorhus/pure.git "$HOME/.zsh/pure"
```

### Smart Suggestions

This assumes you have a valid GitHub Copilot subscription.

Install [copilot-api](https://github.com/ericc-ch/copilot-api) via pnpm:

```sh
pnpm install -g copilot-api
```

Run `copilot-api start` to authenticate and verify it works.

Symlink the Copilot API plist to LaunchAgents:

```sh
ln -sf $HOME/.config/zshrc/com.damianb-bitflipper.copilot-api.plist ~/Library/LaunchAgents/
```

Requires Go: `brew install go`

Follow the [Oh My Zsh installation guide](https://github.com/tizee/smart-suggestion?tab=readme-ov-file#oh-my-zsh) from this repo: https://github.com/tizee/smart-suggestion.git

Run the commands relative to the cloned repository.

## Requirements

- [Oh My Zsh](https://ohmyz.sh/)
- `pipx`
- `uv`
- `direnv`
- `fzf` (`brew install fzf`)
- `zoxide` (`brew install zoxide`)
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
