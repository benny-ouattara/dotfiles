# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(fzf git podman sudo tailscale aliases aws history z)

export FZF_BASE=/home/ben/.guix-home/profile/bin/fzf
export FZF_DEFAULT_COMMAND='fzf'
source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

export GPG_TTY=$(tty)
eval "$(direnv hook zsh)"
export PATH=/home/ben/.local/share/gem/ruby/3.3.0/bin:$PATH
if [ -S $XDG_RUNTIME_DIR/ssh-agent/socket ]; then
    export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent/socket
    ssh-add -q ~/.ssh/jazacash
    ssh-add -q ~/.ssh/id_rsa
fi

export MANPAGER="bat -plman"

alias ls='eza'
alias less='bat'
alias tree='eza --long --tree'
alias reload='exec $SHELL -l'
alias u='up'
alias us='up status'
alias ur='up system-reconfigure'
alias uh='up home-reconfigure'
alias guix-rm-cache="sudo rm -rf ~/.cache/guix/"
alias pc="podman-compose"
alias edit="nvim"
alias vim="nvim"
alias info="info --vi-keys"
alias oc-perms-fix="sudo -i -u openclaw podman unshare chmod -R 2775 /home/openclaw/.openclaw"
alias oc-edit="sudo -i -u openclaw nvim /home/openclaw/.openclaw"
alias tail-oc-upgrade="tail -f ~/.local/state/log/openclaw-upgrade.log"
alias tail-lama="tail -f ~/.local/state/log/ollama.logs"
alias tail-tailscale="sudo tail -f /var/log/tailscaled.log"

if [ -f ~/Code/google-cloud-sdk/path.zsh.inc ]; then
    source ~/Code/google-cloud-sdk/path.zsh.inc
fi

oc-podman() {
    sudo -i -u openclaw podman "$@"
}

oc-setup() {
    sudo -i -u openclaw /home/openclaw/run-openclaw-podman.sh launch setup
}

openclaw() {
    oc-perms-fix
    sudo -i -u openclaw podman run -it --rm \
        --name openclaw-cli \
        --network container:openclaw \
        --userns keep-id \
        -v "/home/openclaw/.openclaw:/home/node/.openclaw:rw,z" \
        "openclaw:local" \
        openclaw "$@"
}

openclaw-onboard() {
    sudo -i -u openclaw podman run -it --rm \
        --name openclaw-cli \
        --userns keep-id \
        -v "/home/openclaw/.openclaw:/home/node/.openclaw:rw,z" \
        "openclaw:local" \
    node dist/index.js onboard 
}

oc-cli-shell() {
    sudo -i -u openclaw podman run -it --rm \
        --name openclaw-cli \
        --network container:openclaw \
        --userns keep-id \
        -v "/home/openclaw/.openclaw:/home/node/.openclaw:rw,z" \
        "openclaw:local" \
        /bin/sh
}

oc-gateway-shell() {
    sudo -i -u openclaw podman exec -it openclaw /bin/sh
}

# Simple completion for the up command
_up() {
    local makefile="$HOME/Code/dotfiles/guix/Makefile"
    reply=($(grep -oE '^[a-zA-Z_-]+:' "$makefile" | sed 's/://'))
}
compctl -K _up up
