# ==============================================================================
# PATH Configuration
# ==============================================================================

# Doom Emacs
if [ -d "$HOME/.emacs.d/bin" ]; then
    PATH="$HOME/.emacs.d/bin:$PATH"
fi

# pipx (Created on 2024-07-01)
export PATH="$PATH:$HOME/.local/bin"

# ==============================================================================
# Completion System
# ==============================================================================

# Initialize zsh completion system
autoload -Uz compinit && compinit

# uv shell completion
eval "$(uv generate-shell-completion zsh)"

# Angular CLI autocompletion
if [ -x "$(command -v ng)" ]; then
    source <(ng completion script)
fi

# ==============================================================================
# Prompt Configuration
# ==============================================================================

setopt PROMPT_SUBST
autoload -U colors && colors

# Main prompt: user@host directory
PROMPT='%F{cyan}%n%f@%F{magenta}%m%f %F{green}%~%f %# '

# Git branch info in right prompt
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT=\$vcs_info_msg_0_
zstyle ':vcs_info:git:*' formats '%F{240}(%b)%r%f'
zstyle ':vcs_info:*' enable git

# ==============================================================================
# Tool Hooks & Integrations
# ==============================================================================

# Direnv
eval "$(direnv hook zsh)"

# pyautoenv - automatic Python virtual environment activation
source "$HOME/.config/zshrc/pyautoenv/pyautoenv.plugin.zsh"

# Google Cloud SDK
if [ -f "/usr/local/google-cloud-sdk/path.zsh.inc" ]; then
    . "/usr/local/google-cloud-sdk/path.zsh.inc"
fi
if [ -f "/usr/local/google-cloud-sdk/completion.zsh.inc" ]; then
    . "/usr/local/google-cloud-sdk/completion.zsh.inc"
fi

# ==============================================================================
# Aliases
# ==============================================================================

# Remove all local branches that have been deleted on the remote
alias git-prune-branches='git fetch -p && git branch -vv | grep ": gone]" | awk "{print \$1}" | xargs git branch -D'

# ==============================================================================
# Functions
# ==============================================================================

# Move aerospace workspace to specified monitor
move-workspace() {
    if [[ $# -lt 1 ]]; then
        echo "Usage: move-workspace <workspace_id> [monitor_id]"
        echo "If monitor_id is not provided, it will use 'next'"
        return 1
    fi

    local workspace_id="$1"
    local monitor="${2:-next}"

    aerospace move-workspace-to-monitor --workspace "$workspace_id" "$monitor"
}

# Capture stdout/stderr output to clipboard while displaying it
clip() {
    { "$@" 2>&1 | tee /dev/tty | pbcopy; }
}
