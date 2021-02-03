# Add to $PROFILE ( e.g. ~\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1 )
# if (Test-Path "~/.dotFiles/_powershell.psm1") { Import-Module "~/.dotFiles/_powershell.psm1" }

# Startship prompt
Invoke-Expression (&starship init powershell)

# Set font colour to red in admin windows
if ( ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator) )
{
    $Host.UI.RawUI.ForegroundColor = "Red";
    cd C:\
}


##
# Aliases
##
function dirs()
{
    dir -Directory -Filter $($args | Out-String) -Recurse -ErrorAction SilentlyContinue | Select Fullname
    dir -File -Filter $($args | Out-String) -Recurse -ErrorAction SilentlyContinue | Select Fullname
}



Set-Alias -Name emacs -Value C:\bat\emacs.cmd
Set-Alias -Name pd -Value Push-Location
Set-Alias -Name po -Value Pop-Location

If (Test-Path Alias:ls) {Remove-Item Alias:ls}
