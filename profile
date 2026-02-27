# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export LANG=en_GB.UTF-8
export MANPAGER=most
export EDITOR='emacsclient -nw'
export PATH="$PATH:$HOME/Developer/go/bin:$HOME/.bin:$HOME/.opencode/bin:$HOME/.gobrew/current/bin:$HOME/.gobrew/bin:$HOME/.nodejs/current/bin"
export GOPATH="$HOME/Developer/go"
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_STYLE_OVERRIDE=adwaita-dark
export BROWSER=/usr/bin/google-chrome-stable


