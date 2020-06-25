# New Document

# .dotFiles
dotFiles repository


# Usage
In your home directory:
git clone https://github.com/ajitnilakantan/.dotFiles.git

# Set up dot files
## .vimrc

Include the lines
```viml
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
# Add to .profile:
(FILE=~/.dotFiles/_profile; [ -f $FILE ] && source $FILE)
#
```

## .zprofile  (Zsh)
Include the lines
```sh
#
# Add to .zprofile:
(FILE=~/.dotFiles/_zprofile; [ -f $FILE ] && source $FILE)
#
```

