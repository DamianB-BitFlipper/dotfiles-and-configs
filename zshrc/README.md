# ZSH Dotfile

## Configuration

Be sure to ln `.zshrc` to `$HOME`.

`ln -s $HOME/.config/zshrc/.zshrc $HOME/.zshrc`

## Requirements

- `pipx`
- `uv`
- `aider`

## Authentication

The zshrc requires some authenticated resources in the `~/.authinfo` file.

Copy the following and edit <...> accordingly.
```
machine api.anthropic.com login api-key password <api-key for inference>
machine api.openai.com login api-key password <api-key for inference>
```
  
