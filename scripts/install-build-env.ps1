#Requires -RunAsAdministrator
# Installs the Keyman build environment on Windows (everything except Delphi).
# Logs to <repo-root>\install-build-env.log (gitignored).

$ErrorActionPreference = 'Continue'
$ProgressPreference = 'SilentlyContinue'

# Auto-detect repo root. Override by setting $env:KEYMAN_ROOT.
if ($env:KEYMAN_ROOT) {
  $REPO = $env:KEYMAN_ROOT
} else {
  # This script lives at <repo>\scripts\, so repo root is one level up.
  $REPO = Split-Path $PSScriptRoot -Parent
}

$log = Join-Path $REPO 'install-build-env.log'
Start-Transcript -Path $log -Force | Out-Null

Write-Host "=== Step 1/5: Install Chocolatey ==="
if (-not (Get-Command choco -ErrorAction SilentlyContinue)) {
    Set-ExecutionPolicy Bypass -Scope Process -Force
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
    iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
    $env:PATH = "$env:PATH;$env:ProgramData\chocolatey\bin"
} else {
    Write-Host "Chocolatey already installed."
}

Write-Host "=== Step 2/5: Install base deps (git, jq, python, ninja, pandoc, meson) ==="
choco install -y git jq python ninja pandoc meson

Write-Host "=== Step 3/5: Install nvm-windows + node 20.16.0 ==="
choco install -y nvm
# refresh PATH so nvm is callable
$env:PATH = [System.Environment]::GetEnvironmentVariable('PATH','Machine') + ';' + [System.Environment]::GetEnvironmentVariable('PATH','User')
if (Get-Command nvm -ErrorAction SilentlyContinue) {
    nvm install 20.16.0
    nvm use 20.16.0
} else {
    Write-Host "WARNING: nvm not on PATH after install. Open a new shell and run: nvm install 20.16.0; nvm use 20.16.0"
}

Write-Host "=== Step 4/5: Install release-build tools (7zip, html-help-workshop, wixtoolset 3.14.1) ==="
choco install -y 7zip html-help-workshop
choco install -y wixtoolset --version=3.14.1

Write-Host "=== Step 5/5: Install Visual Studio 2022 Community (Native Desktop) ==="
Write-Host "This is a large install (~10GB) and may take 20-40 minutes."
$vsArgs = '--passive --add Microsoft.VisualStudio.Workload.NativeDesktop ' +
          '--add Microsoft.VisualStudio.Component.VC.Tools.ARM64 ' +
          '--add Microsoft.VisualStudio.Component.CppBuildInsights ' +
          '--add Microsoft.VisualStudio.Component.Debugger.JustInTime ' +
          '--add Microsoft.VisualStudio.Component.VC.ASAN ' +
          '--add Microsoft.VisualStudio.Component.VC.DiagnosticTools ' +
          '--add Microsoft.VisualStudio.Component.VC.TestAdapterForGoogleTest ' +
          '--add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 ' +
          '--add Microsoft.VisualStudio.Component.Windows11SDK.26100 ' +
          '--add Microsoft.VisualStudio.Component.Windows11Sdk.WindowsPerformanceToolkit ' +
          '--add Microsoft.VisualStudio.Component.Windows10SDK.19041'
winget install --id=Microsoft.VisualStudio.2022.Community -e --accept-source-agreements --accept-package-agreements --override $vsArgs

Write-Host "=== Done. Log saved to $log ==="
Stop-Transcript | Out-Null
