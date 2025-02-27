if [[ "$TERM" == (alacritty|*termite) ]]; then
  export COLORTERM=truecolor
fi

set -o emacs

if [[ "$OSTYPE" = darwin* ]]; then
  if [[ "$(arch)" = "arm64" ]]; then
      eval "$(/opt/homebrew/bin/brew shellenv)"
  else
      eval "$(/usr/local/bin/brew shellenv)"
  fi
fi

if [[ -z "$CODE" ]] && [[ -n "$DISPLAY" ]] && ! xprop -id "$WINDOWID" 2> /dev/null \
        | grep -q Scratchpad ; then
  # If not running interactively, do not do anything
  [[ $- != *i* ]] && return
  [[ -z "$TMUX" ]] && exec tmux
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

setopt INC_APPEND_HISTORY
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=10000

if brew --prefix &> /dev/null; then
  fpath+=("$(brew --prefix)"/share/{zsh/site-functions,zsh-completions})
else
  fpath+=(/usr/share/zsh/site-functions/)
fi

autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true
setopt COMPLETE_ALIASES

autoload -Uz zmv
autoload -Uz add-zsh-hook

#_dotnet_zsh_complete() {
#  local completions=("$(dotnet complete "$words")")
#  reply=( "${(ps:\n:)completions}" )
#}
#compctl -K _dotnet_zsh_complete dotnet

function xterm_title_precmd () {
  print -Pn -- '\e]2;%n@%m %~\a'
}

function xterm_title_preexec () {
  print -Pn -- '\e]2;%n@%m %~ %# ' && print -n -- "${(q)1}\a"
}

if [[ "$TERM" == (alacritty|gnome*|konsole*|putty*|rxvt*|screen*|tmux*|xterm*) ]]; then
  add-zsh-hook -Uz precmd xterm_title_precmd
  add-zsh-hook -Uz preexec xterm_title_preexec
fi

alias cal='cal --color=auto'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias diff='diff --color=auto'

alias ll='ls -la'

alias ccat='highlight -O xterm256 -l'
alias virsh='virsh --connect qemu:///system'
alias fedora='virsh start fedora && sleep 20s; ssh fedora'

alias dgit="git --git-dir ~/.dotfiles/.git --work-tree=$HOME"

# for compile_commands.json + Ninja
alias cmake="cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -GNinja -Bbuild"

alias bkrssh="ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -l root"

eval $(dircolors -b "$XDG_CONFIG_HOME/.dir_colors")
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

export LESS_COMMON_OPTS='-R --mouse --wheel-lines=3'
export LESSOPEN='| /usr/bin/source-highlight-esc.sh %s'
export LESS="$LESS_COMMON_OPTS -c"
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin blink
export LESS_TERMCAP_md=$'\E[1;36m'     # begin bold
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

export GIT_PAGER="LESS='$LESS_COMMON_OPTS' less -F"

# Env
export BROWSER=firefox
export TERMINAL=alacritty
export VISUAL=vim
export EDITOR="$VISUAL"
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export CLICOLOR_FORCE=1
export HOMEBREW_NO_ANALYTICS=1

# ccache
export PATH="/usr/lib/ccache/bin:$PATH"

# python user-site
export PATH="$PATH:$HOME/.local/bin"

# Aliases
alias vimrc="$VISUAL ~/.vim/vimrc"
alias zshrc="$VISUAL ~/.zshrc && source ~/.zshrc"
alias tmuxrc="$VISUAL $XDG_CONFIG_HOME/tmux/tmux.conf"
alias xmonadrc="$VISUAL $XDG_CONFIG_HOME/xmonad/xmonad.hs && xmonad --recompile && xmonad --restart"
alias xmobarrc="$VISUAL $XDG_CONFIG_HOME/xmobar/xmobar*.hs && xmonad --restart"

# ccache
if brew --prefix &> /dev/null; then
  export PATH="$(brew --prefix ccache)/libexec:$PATH"
else
  export PATH="/usr/lib/ccache/bin:$PATH"
fi

# Enable VA-API with X11 and Firefox
export MOZ_X11_EGL=1

# zsh plugins
if brew --prefix &> /dev/null; then
  PLUGIN_DIR="$(brew --prefix)/share"
else
  PLUGIN_DIR=/usr/share/zsh/plugins
fi

source $PLUGIN_DIR/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
source $PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source $PLUGIN_DIR/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND=bold,fg=39
