if [[ "$TERM" == (alacritty|*termite) ]]; then
  export COLORTERM=truecolor
fi

if [[ -n "$DISPLAY" ]] && ! xprop -id "$WINDOWID" 2> /dev/null \
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
bindkey -v

fpath=(/usr/share/zsh/site-functions/ $fpath)

autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true
setopt COMPLETE_ALIASES

autoload -Uz add-zsh-hook

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

alias aisa='ssh aisa'
alias ccat='highlight -O xterm256 -l'
alias fedora32='virsh start fedora32 2> /dev/null; ssh fedora32'

alias dgit="git --git-dir ~/.dotfiles/.git --work-tree=$HOME"
alias vimrc="$VISUAL ~/.vim/vimrc"
alias zshrc="$VISUAL ~/.zshrc && source ~/.zshrc"
alias tmuxrc="$VISUAL $XDG_CONFIG_HOME/tmux/tmux.conf"
alias xmonadrc="$VISUAL $XDG_CONFIG_HOME/xmonad/xmonad.hs && xmonad --recompile && xmonad --restart"
alias xmobarrc="$VISUAL $XDG_CONFIG_HOME/xmobar/xmobar*.hs && xmonad --restart"

# for compile_comamnds.json
alias cmake="cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"

eval $(dircolors -b "$XDG_CONFIG_HOME/.dir_colors")
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Nastavení kláves
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "^H" backward-delete-word
# pro rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
# pro ne RH/Debian xterm, nemůže ublížit RH/DEbian xtermu
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
# pro freebsd konzoli
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line
# doplňování uprostřed řádku
bindkey '^i' expand-or-complete-prefix

bindkey '^R' history-incremental-search-backward

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

# Enable VA-API with X11 and Firefox
export MOZ_X11_EGL=1

PLUGIN_DIR=/usr/share/zsh/plugins

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
source $PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source $PLUGIN_DIR/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND=bold,fg=39
