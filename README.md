# dotFiles Setup

# .dotFiles
dotFiles repository


# Usage
In your home directory:
git clone https://github.com/ajitnilakantan/.dotFiles.git

# Set up dot files

## .profile  (Bash)
Include the lines
```bash
#
# Add to ~/.profile:
FILE=~/.dotFiles/_profile; [ -f $FILE ] && source $FILE; unset FILE
#
```

## .zshrc  (Zsh)
Include the lines
```zsh
#
# Add to ~/.zshrc:
FILE=~/.dotFiles/_zshrc; [ -f $FILE ] && source $FILE; unset FILE
#
```


## Git setup
```bash
git config --global user.name "Full Name"
git config --global user.email "name@email.com"

git config --global pull.rebase true
git config --global fetch.prune true
git config --global diff.colorMoved zebra
git config --global color.ui true
#git config --global core.editor vi
```

git-wip installed from https://github.com/bartman/git-wip

Useful commands:
- `git wip` _#Create new commit on the wip/topic branch (creating it if needed)_
- `git wip save "description"` _#Allows for a custom commit message_
- `git wip log` _#Show the list of the work that leads upto the last WIP commit. This is similar to invoking:_ `git log --stat wip/$branch...$(git merge-base wip/$branch $branch)`
- `git push . :wip/BRANCHNAME` _#Delete wips_



<!--
## Scoop setup (Windows)
See: https://github.com/ScoopInstaller/Scoop/issues/1606
```sh
scoop config shim kiennq
scoop reset *
```
-->
## Windows setup
- Install scoop
```pwsh
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope Process
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression
# scoop export | Out-File ~/.dotFiles/Scoopfile.json
scoop import ~/.dotFiles/Scoopfile.json
scoop cleanup *
```

## MacOS Setup
```zsh
xcode-select --install # Will install xcode cli tools
git --version          # Will install xcode cli tools

# Install brew
# Brewfile created with:
#   brew bundle dump --force --file=Brewfile
brew bundle install --file=~/.dotFiles/Brewfile

# Brew diagnostics.   Follow instructions if any for brew link etc.
brew doctor

# Use rustup to install rust and friends
rustup default stable  # set default channel with
rustup toolchain install stable   # Can install "stable" toolchain
rustup component add rustc cargo rustfmt rust-std rust-analyzer clippy rust-src #Install components

# LSP tool for F#
dotnet tool install --global fsautocomplete

# Terminal theme
#   Import ~/.dotFiles/_MacOSTerminalTheme.terminal into Terminal.app

```


