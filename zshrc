alias ls='ls -l'
alias emacs='emacsclient -nw'
alias tailscale="/Applications/Tailscale.app/Contents/MacOS/Tailscale"

source <(fzf --zsh)

fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit && compinit -i

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"
