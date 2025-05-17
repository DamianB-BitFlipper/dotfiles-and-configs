if [ -d "$HOME/.emacs.d/bin" ]; then    
    PATH="$HOME/.emacs.d/bin:$PATH"
fi

# Created by `pipx` on 2024-07-01 12:50:58
export PATH="$PATH:$HOME/.local/bin"

# For poetry autocompletion
fpath+=~/.zfunc
autoload -Uz compinit && compinit

# Direnv hook
eval "$(direnv hook zsh)"

# Remove all local branches that have been deleted on the remote
alias git-prune-branches='git fetch -p && git branch -vv | grep ": gone]" | awk "{print \$1}" | xargs git branch -D'

# New colorful prompt configuration
setopt PROMPT_SUBST
autoload -U colors && colors

# Set the prompt
PROMPT='%F{cyan}%n%f@%F{magenta}%m%f %F{green}%~%f %# '

# Optional: Add git branch information to the prompt
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT=\$vcs_info_msg_0_
zstyle ':vcs_info:git:*' formats '%F{240}(%b)%r%f'
zstyle ':vcs_info:*' enable git

get_api_key() {
    local machine="$1"
    awk -v machine="$machine" '$1 == "machine" && $2 == machine {
        for (i=1; i<=NF; i++) {
            if ($i == "password") {
                print $(i+1)
                exit
            }
        }
    }' "$HOME/.authinfo"
}

aider() {
    local cmd="$HOME/.local/bin/aider"

    # Read API keys from .authinfo
    local anthropic_api_key=$(get_api_key "api.anthropic.com")
    local openai_api_key=$(get_api_key "api.openai.com")

    # OpenAI Aider
    local args=(
        "--anthropic-api-key" "$anthropic_api_key"
        "--openai-api-key" "$openai_api_key"
        "--architect"
        # "--model" "o3-mini"
        # "--reasoning-effort" "high"
        "--model" "sonnet"
        "--editor-model" "sonnet"
        "--cache-prompts"
        "--cache-keepalive-pings" "12"
        "--no-suggest-shell-commands"
        "--dark-mode"
        "--no-auto-lint"
        "--watch-files"
    )

    if [[ -f ".aider.conventions.md" ]]; then
        args+=(".aider.conventions.md")
    fi

    $cmd "${args[@]}" "$@"
}
eval "$(uv generate-shell-completion zsh)"

# For OpenAI codex
export OPENAI_API_KEY=$(get_api_key "api.openai.com")

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/damianb/Documents/my_code/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/damianb/Documents/my_code/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/damianb/Documents/my_code/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/damianb/Documents/my_code/google-cloud-sdk/completion.zsh.inc'; fi


# Load Angular CLI autocompletion.
source <(ng completion script)

# Function to move aerospace workspace to specified monitor
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
