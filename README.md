# dotFiles Setup

# .dotFiles
dotFiles repository


# Usage
In your home directory:
```sh
git clone git@github.com:ajitnilakantan/.dotFiles.git
```

# Set up dot files
## .vimrc

Include the lines
```viml
" ~/.vimrc
let s:file_list = [
\ "~/.dotFiles/_vimrc",
\ "$HOME/.dotFiles/_vimrc",
\ findfile('.dotFiles/_vimrc', fnamemodify($MYVIMRC, ':p:h')), ]
:for filename in s:file_list
  :if filereadable(filename)
    :execute 'source '.filename
    :break
  :endif
:endfor
```

## .emacsrc
Include the lines
```lisp
; ~/.emacs
(catch 'exitLoop
    (setq my-list '("~/.dotFiles/emacs/my-site-start"))
    (dolist (x my-list)
        (if (file-exists-p (concat x ".el"))
                (progn (load x)
                       (message (concat "Loading " x ))
                       (throw 'exitLoop nil) ) ) ) )
```

## .profile  (Bash)
Include the lines
```sh
#
# Add to ~/.profile:
FILE=~/.dotFiles/_profile; [ -f $FILE ] && source $FILE; unset FILE
#
```

## .zprofile  (Zsh)
Include the lines
```sh
#
# Add to ~/.zprofile:
FILE=~/.dotFiles/_zprofile; [ -f $FILE ] && source $FILE; unset FILE
#
```

## .zshrc  (Zsh)
Include the lines
```sh
#
# Add to ~/.zshrc:
FILE=~/.dotFiles/_zshrc; [ -f $FILE ] && source $FILE; unset FILE
#
```


## Git setup
```sh
git config --global user.name "Full Name"
git config --global user.email "name@email.com"

git config --global pull.rebase true
git config --global fetch.prune true
git config --global diff.colorMoved zebra
git config --global color.ui true
#git config --global core.editor vi

```

## Scoop setup (Windows)
See: https://github.com/ScoopInstaller/Scoop/issues/1606
```sh
scoop config shim kiennq
scoop reset *
```

## MacOs setup
```sh
git --version   # Will install xcode cli tools
# Install homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# Install,  Brewfile created with: brew bundle dump --force
brew bundle
```

