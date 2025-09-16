export LANG=en_GB.UTF-8
export MANPAGER=most
export EDITOR='emacsclient -nw'
export PATH="$PATH:/usr/local/sbin:$HOME/Developer/go/bin:/opt/homebrew/bin:$HOME/.bin:$HOME/.deno/bin"
export GOPATH="$HOME/Developer/go"
export TERM=xterm-256color

alias ls='ls -l'
alias emacs='emacsclient -nw'
alias tailscale="/Applications/Tailscale.app/Contents/MacOS/Tailscale"

source <(fzf --zsh)


# setting for gup command (auto generate)
fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i
