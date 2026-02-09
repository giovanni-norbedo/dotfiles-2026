autoload -U colors && colors

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt APPEND_HISTORY
setopt SHARE_HISTORY

autoload -Uz compinit
compinit

if [ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

if [ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

eval "$(starship init zsh)"

alias ls='eza --icons --group-directories-first'
alias ll='eza -alF --icons --group-directories-first --git'
alias la='eza -a --icons --group-directories-first'
alias lt='eza --tree --level=2 --icons'

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'
alias cat='bat --paging=never'
alias grep='grep --color=auto'
alias mkdir='mkdir -p'
alias h='history | grep'

alias ..='cd ..'
alias ..2='cd ../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'

alias pacsyu='sudo pacman -Syu'
alias pacs='sudo pacman -S'
alias pacr='sudo pacman -Rns'
alias pacq='pacman -Q | grep'
alias paco='sudo pacman -Qtdq | sudo pacman -Rns -'
alias yays='yay -S'
alias yayu='yay -Sua --noconfirm'

alias reload='source ~/.zshrc && echo "Config reloaded."'
alias myip='curl ifconfig.me'


extract () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1     ;;
      *.tar.gz)    tar xzf $1     ;;
      *.bz2)       bunzip2 $1     ;;
      *.rar)       unrar e $1     ;;
      *.gz)        gunzip $1      ;;
      *.tar)       tar xf $1      ;;
      *.tbz2)      tar xjf $1     ;;
      *.tgz)       tar xzf $1     ;;
      *.zip)       unzip $1       ;;
      *.Z)         uncompress $1  ;;
      *.7z)        7z x $1        ;;
      *)           echo "'$1' cannot be extracted via extract()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}


if command -v fastfetch &> /dev/null; then
    fastfetch
fi
