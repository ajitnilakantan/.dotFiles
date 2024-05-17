###
## Add to $PROFILE:
##  E.g. $PROFILE | Select-Object *
##  E.g. ~\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1 # in PS5
##  E.g. ~\Documents\PowerShell\profile.ps1 # powershell-core7
##
# if (Test-Path "~/.dotFiles/_powershell.psm1") { Import-Module "~/.dotFiles/_powershell.psm1" }
###

# This script location
$Script:ThisFolder = (Split-Path $MyInvocation.MyCommand.Path -Parent)

# Starship prompt
if (Get-Command starship) {
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
    & curl.exe -s "https://raw.githubusercontent.com/tldr-pages/tldr/master/pages/common/$($cmd).md" | & bat.exe --language md --style plain
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
    Start-Process -NoNewWindow -Wait "$($(Get-Command 'emacs.exe').Path)"  $("--init-directory",$local:emacsinit,"-nw",$args | Join-String -DoubleQuote -Separator ' ')
}

function emacsw()
{
    $local:emacsinit = (Join-Path $ThisFolder 'emacs')
    Start-Process -NoNewWindow "$($(Get-Command 'emacs.exe').Path)" $("--init-directory",$local:emacsinit,$args | Join-String -DoubleQuote -Separator ' ')
}

function gvim()
{
    Start-Process -NoNewWindow "$($(Get-Command 'gvim.exe').Path)" $($args | Join-String -DoubleQuote -Separator ' ')
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
if (Get-Command difft) {
    $env:GIT_EXTERNAL_DIFF='difft'
}

# Environment variables for interactive use only
$env:LESS='eFRX -i -G -M -X -s'
$env:RIPGREP_CONFIG_PATH=(Join-Path $ThisFolder '_ripgreprc')

# Interactive Python startup script
$env:PYTHONSTARTUP=(Join-Path $ThisFolder '_pythonstartup')


