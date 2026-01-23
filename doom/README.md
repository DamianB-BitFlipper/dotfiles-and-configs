# My Doom Configuration

## Configuration

Install `emacs` via `brew install --cask jimeh/emacs-builds/emacs-app`

Install `doom` following https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#doom-emacs


## Copilot API

The `magit-gptcommit-llm-provider` uses the locally hosted copilot API. To set up, refer to the **ZSH AI Plugin** section of the zshrc README.

## Installation
- `brew install ripgrep`
- `brew install fd`
- `brew install fontconfig`
- `brew install coreutils cmake aspell clang-format glslang`
- `brew install go && go install github.com/jessfraz/dockfmt@latest`
- `brew install pandoc`
- `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
- `brew install shfmt shellcheck`
- For grip, run `pipx install grip`.
  - Then create a fine-grained access token on GitHub. No need for any repository access. 
    - This allows more access to the GitHub Markdown API
    - Add this configuration to `~/.authinfo` as per https://www.emacswiki.org/emacs/GnusAuthinfo
      - machine: api.github.com
      - login: <github username>
      - password: <access token>
- Run `doom sync` followed by `doom doctor`. Install any missing dependencies reported by doom doctor. Python errors are OK since they will be provided in the venvs
- For pdf viewing, run `brew reinstall gpgme poppler` and then `M-x pdf-tools-install`
