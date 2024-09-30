# ZSH Dotfile

## Configuration

Be sure to ln `.zshrc` to `$HOME`.

`ln -s $HOME/.config/zshrc/.zshrc $HOME/.zshrc`

## Authentication

The zshrc requires some authenticated resources in the `~/.authinfo` file.

- Anthropic Resource
  - machine: api.anthropic.com
  - login: api-key
  - password: <api-key for inference>
- OpenAI Resource
  - machine: api.openai.com
  - login: api-key
  - password: <api-key for inference>
