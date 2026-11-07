###
## Add to $PROFILE:
##  E.g. $PROFILE | Select-Object *
##  E.g. ~\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1 # in PS5
##  E.g. ~\Documents\PowerShell\profile.ps1 # powershell-core7
##
# if (Test-Path "~/.dotFiles/_powershell.psm1") { Import-Module -DisableNameChecking "~/.dotFiles/_powershell.psm1" }
###

# This script location
$Script:ThisFolder = (Split-Path $MyInvocation.MyCommand.Path -Parent)

# Default is 4096
$global:MaximumHistoryCount = 32767

# Produce UTF-8 by default
$PSDefaultParameterValues["Out-File:Encoding"]="utf8"

# Show selection menu for tab
Set-PSReadlineKeyHandler -Chord Tab -Function MenuComplete

# Starship prompt
if (Get-Command starship -ErrorAction Ignore) {
   Invoke-Expression (&starship init powershell)
}

# Set font colour to red in admin windows
if ( ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator) )
{
    $Host.UI.RawUI.ForegroundColor = "Red";
    $Host.UI.RawUI.WindowTitle = '***Administrator***'
    cd C:\
}

##
# Autocomplete: https://dev.to/ofhouse/add-a-bash-like-autocomplete-to-your-powershell-4257
##
# Shows navigable menu of all options when hitting Tab
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete
# Gives you a list view and it shows what kind of input the parameter takes (string, string array, int etc.)
Set-PSReadlineOption -ShowToolTips

# Autocompletion for arrow keys
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadlineKeyHandler -Key UpArrow -ScriptBlock { [Microsoft.PowerShell.PSConsoleReadLine]::HistorySearchBackward(); }
Set-PSReadlineKeyHandler -Key DownArrow -ScriptBlock { [Microsoft.PowerShell.PSConsoleReadLine]::HistorySearchForward(); }

##
# Aliases
##
function dirs()
{
    dir -Directory -Filter $($args | Out-String -NoNewline) -Recurse -ErrorAction SilentlyContinue | Select Fullname
    dir -File -Filter $($args | Out-String -NoNewline) -Recurse -ErrorAction SilentlyContinue | Select Fullname
}
function ff()
{
    dirs
}

function tldr([string]$cmd)
{
    & curl.exe -s "https://raw.githubusercontent.com/tldr-pages/tldr/master/pages/common/$($cmd).md" | & glow.exe --pager -
}

function time([string]$cmd)
{
    # echo "z$($cmd)zx$($args)x";
    $a=$args;
    if ($a) {
        $result = (Measure-Command {Start-Process -Wait -NoNewWindow $cmd -ArgumentList $a}).TotalSeconds
    } else {
        $result = (Measure-Command {Start-Process -Wait -NoNewWindow $cmd}).TotalSeconds
    }
    Write-Host -ForeGround Yellow "$result seconds"
}

function gitdiff()
{
    git fetch; git diff   '@{upstream}'; echo '==='; git diff '@' '@{upstream}'; echo '==='
}

function emacs()
{
    $local:emacsinit = (Join-Path $ThisFolder 'emacs')
    $local:noWait = @{}
    if ($args -contains "-nw") {
       $local:noWait = @{Wait = $true}
    }
    Start-Process -NoNewWindow @local:noWait "$($(Get-Command 'emacs.exe').Path)"  $("--init-directory",$local:emacsinit,$args | Join-String -DoubleQuote -Separator ' ')
}

function emacsw() {emacs @args}

function vim()
{
    if (Get-Command "nvim.exe" -ErrorAction Ignore) {
        $local:rcfile = (Join-Path $ThisFolder 'nvim/init.lua')
        Start-Process -NoNewWindow -Wait -Environment @{ XDG_CONFIG_HOME="$($HOME)/.config"; XDG_DATA_HOME="$($HOME)/.local/share"; XDG_STATE_HOME="$($HOME)/.local/state"; XDG_CACHE_HOME="$($HOME)/.cache"; } "$($(Get-Command 'nvim.exe').Path)" $("-u",$local:rcfile,$args | Join-String -DoubleQuote -Separator ' ')
    } else {
        $local:rcfile = (Join-Path $ThisFolder '_vimrc')
        Start-Process -NoNewWindow -Wait "$($(Get-Command 'vim.exe').Path)" $("-u",$local:rcfile,$args | Join-String -DoubleQuote -Separator ' ')
    }
}

function vi() {vim @args}

function gvim()
{
    if (Get-Command "neovide.exe" -ErrorAction Ignore) {
        $local:rcfile = (Join-Path $ThisFolder 'nvim/init.lua')
        Start-Process -NoNewWindow -Environment @{ XDG_CONFIG_HOME="$($HOME)/.config"; XDG_DATA_HOME="$($HOME)/.local/share"; XDG_STATE_HOME="$($HOME)/.local/state"; XDG_CACHE_HOME="$($HOME)/.cache"; } "$($(Get-Command 'neovide.exe').Path)" $("--fork",$args,"--","-u",$local:rcfile | Join-String -DoubleQuote -Separator ' ')
    } else {
        $local:rcfile = (Join-Path $ThisFolder '_vimrc')
        Start-Process -NoNewWindow "$($(Get-Command 'gvim.exe').Path)" $("-u",$local:rcfile,$args | Join-String -DoubleQuote -Separator ' ')
    }
}

function ssh-copy-id($server) {
    type $env:USERPROFILE\.ssh\id_rsa.pub | ssh $server "mkdir -p ~/.ssh && chmod 700 ~/.ssh && cat >> ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys"
}

function rga-fzf {
    param([string]$query = "")
    
    if (Get-Command rga -ErrorAction SilentlyContinue) {
        $search_cmd = "rga"
        $rg_prefix  = "rga --line-number --no-heading"
    } elseif (Get-Command rg -ErrorAction SilentlyContinue) {
        $search_cmd = "rg"
        $rg_prefix  = "rg --line-number --no-heading"
    } else {
        Write-Error "Neither 'rga' nor 'rg' was found in your PATH."
        return
    }
    
    $env:FZF_DEFAULT_COMMAND = "$rg_prefix '$query'"
    $has_bat = [bool](Get-Command bat -ErrorAction SilentlyContinue)
    
    # We use escaped double quotes for cross-platform compatibility inside fzf
    if ($has_bat) {
        $preview_cmd = "if ('{1}' -match '\.pdf$') { $search_cmd --pretty --context 5 {q} {1} } else { bat --style=numbers --color=always --highlight-line {2} {1} }"
    } else {
        $preview_cmd = "$search_cmd --pretty --context 5 {q} {1}"
    }
    
    # 'default:shell=powershell' forces fzf's background terminal worker out of cmd.exe
    $selected = fzf --sort `
        --phony `
        -q "$query" `
        --ansi `
        --delimiter ":" `
        --preview="$preview_cmd" `
        --preview-window="default:+{2}/2:70%:wrap" `
        --bind "start:reload:$rg_prefix {q} || echo ." `
        --bind "change:reload:$rg_prefix {q} || echo ."
        
    if ($selected) {
        $file = ($selected -split ":")[0]
        Start-Process $file
    }
}
function frg {
    # See: https://news.ycombinator.com/item?id=38471822
    # Usage: frg "searchpattern"
    $result = rg --ignore-case --color=always --line-number --no-heading @Args |
      fzf --ansi `
          --color 'hl:-1:underline,hl+:-1:underline:reverse' `
          --delimiter ':' `
          --preview "bat --color=always {1} --theme=zenburn --highlight-line {2}" `
          --preview-window 'up,60%,border-bottom,+{2}+3/3,~3'
    if ($result) {
        & ($env:EDITOR).Trim("`"'") $result.Split(': ')[2]
    }
}

function scoop_diff {
    # Run a "diff" of the currently installed scoop applications with the checked-in "Scoopfile.json"
    $tmpFile = New-TemporaryFile
    &scoop export | Out-File $tmpFile
    Start-Process WinMergeU.exe -ArgumentList "$($Env:USERPROFILE)/.dotFiles/Scoopfile.json",$tmpFile.FullName -Wait
    Remove-Item $tmpFile
}

function copy-safe([string]$src, [string]$dst)
{
    if ($src -and $dst -and -not (Test-Path -Path $dst)) {
        Copy-Item -Path $src -Destination $dst -Verbose
    }
}

Set-Alias -Name pd -Value Push-Location
Set-Alias -Name po -Value Pop-Location


If (Test-Path Alias:r)  {rm alias:\r}  # Conflicts with "R"

If (Test-Path Alias:ls) {Remove-Item Alias:ls}
If (Test-Path Alias:ls) {Remove-Item Alias:ls}

##
# Autoload 'z': https://github.com/ajeetdsouza/zoxide#powershell
##
#Invoke-Expression (& {
#    $hook = if ($PSVersionTable.PSVersion.Major -lt 6) { 'prompt' } else { 'pwd' }
#    (zoxide init --hook $hook powershell) -join "`n"
#})

##
# Difftastic integration with Git
##
if (Get-Command difft -ErrorAction Ignore) {
    $env:GIT_EXTERNAL_DIFF='difft'
}

# Environment variables for interactive use only
$env:LESS='eFRX -i -G -M -X -s'
$env:PAGER='less -+e -+E -i -G -M -X -s'
$env:RIPGREP_CONFIG_PATH=(Join-Path $ThisFolder '_ripgreprc')
$env:YAZI_FILE_ONE="C:\Users\ajit\scoop\apps\git\current\usr\bin\file.exe"

# Interactive Python startup script
$env:PYTHONSTARTUP=(Join-Path $ThisFolder '_pythonstartup')


