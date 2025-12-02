# My Doom Configuration

## Configuration

Install `emacs` via `brew install --cask jimeh/emacs-builds/emacs-app`

Install `doom` following https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#doom-emacs


## Copilot API

The `magit-gptcommit-llm-provider` uses the locally hosted copilot API. To set up, refer to the **Smart Suggestions** section of the zshrc README.

## Installation
- Run `doom sync` followed by `doom doctor`. Install any missing dependencies reported by doom doctor.
- For python lsp, run `pip install "python-lsp-server[all]"`
- For grip, run `pipx install grip`.
  - Then create a fine-grained access token on GitHub. No need for any repository access. 
    - This allows more access to the GitHub Markdown API
    - Add this configuration to `~/.authinfo` as per https://www.emacswiki.org/emacs/GnusAuthinfo
      - machine: api.github.com
      - login: <github username>
      - password: <access token>
