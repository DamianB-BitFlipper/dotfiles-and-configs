if [ -d "$HOME/.emacs.d/bin" ]; then    
    PATH="$HOME/.emacs.d/bin:$PATH"
fi

# Created by `pipx` on 2024-07-01 12:50:58
export PATH="$PATH:/Users/damianb/.local/bin"

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
    local cmd="/Users/damianb/.local/bin/aider"

    # Read API keys from .authinfo
    local anthropic_api_key=$(get_api_key "api.anthropic.com")
    local openai_api_key=$(get_api_key "api.openai.com")

    # OpenAI Aider
    local args=(
        "--anthropic-api-key" "$anthropic_api_key"
        "--openai-api-key" "$openai_api_key"
        "--sonnet"
        # "--weak-model" "gpt-4o-mini"
        "--architect"
        #"--cache-prompts"
        #"--cache-keepalive-pings" "12"
        "--no-suggest-shell-commands"
        "--dark-mode"
        "--no-auto-lint"
    )

    if [[ -f ".aider.conventions.md" ]]; then
        args+=(".aider.conventions.md")
    fi

    $cmd "${args[@]}" "$@"
}
eval "$(uv generate-shell-completion zsh)"
