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

# Startship prompt
Invoke-Expression (&starship init powershell)

# Set font colour to red in admin windows
if ( ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator) )
{
    $Host.UI.RawUI.ForegroundColor = "Red";
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
    & curl.exe -s "https://raw.githubusercontent.com/tldr-pages/tldr/master/pages/common/$($cmd).md" | & glow.exe - -p
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


Set-Alias -Name emacs -Value C:\bat\emacs.cmd
Set-Alias -Name pd -Value Push-Location
Set-Alias -Name po -Value Pop-Location

If (Test-Path Alias:ls) {Remove-Item Alias:ls}
If (Test-Path Alias:ls) {Remove-Item Alias:ls}

##
# Autoload 'z': https://github.com/ajeetdsouza/zoxide#powershell
##
Invoke-Expression (& {
    $hook = if ($PSVersionTable.PSVersion.Major -lt 6) { 'prompt' } else { 'pwd' }
    (zoxide init --hook $hook powershell) -join "`n"
})

# Environment variables for interactive use only
$env:LESS='eFRX -i -G -M -X -s'
$env:RIPGREP_CONFIG_PATH=(Join-Path $ThisFolder '_ripgreprc')
