#Requires -RunAsAdministrator
# Overlay dev builds onto an installed Keyman 19. Equivalent of what
# `windows/src/build.sh install:engine install:desktop` would do, but standalone
# (doesn't require the build.sh cascade, which is blocked by Delphi CE).
#
# Run elevated after each Delphi/kmcmplib rebuild.

$ErrorActionPreference = 'Continue'

# Auto-detect repo root. Override by setting $env:KEYMAN_ROOT.
if ($env:KEYMAN_ROOT) {
  $REPO = $env:KEYMAN_ROOT
} else {
  # This script lives at the repo root.
  $REPO = $PSScriptRoot
}

$INSTALL_DESKTOP   = 'C:\Program Files (x86)\Keyman\Keyman Desktop'
$INSTALL_DEVELOPER = 'C:\Program Files (x86)\Keyman\Keyman Developer'
$INSTALL_ENGINE    = 'C:\Program Files (x86)\Common Files\Keyman\Keyman Engine'

function Copy-IfExists($src, $dst, $label) {
  if (Test-Path $src) {
    $dstDir = Split-Path $dst -Parent
    if (-not (Test-Path $dstDir)) {
      New-Item -ItemType Directory -Path $dstDir -Force | Out-Null
    }
    Copy-Item $src $dst -Force
    Write-Host "  [$label] $src -> $dst"
  } else {
    Write-Host "  [$label] SKIP (source missing): $src" -ForegroundColor Yellow
  }
}

function Copy-Tree($src, $dst, $label) {
  if (Test-Path $src) {
    if (Test-Path $dst) { Remove-Item $dst -Recurse -Force }
    Copy-Item $src $dst -Recurse -Force
    Write-Host "  [$label] $src -> $dst (tree)"
  } else {
    Write-Host "  [$label] SKIP (source tree missing): $src" -ForegroundColor Yellow
  }
}

Write-Host "=== Stopping any running Keyman processes ==="
Get-Process -Name kmshell, keyman, keymanmc, tike, tsysinfo, tsysinfox64, kmbrowserhost, kmconfig -ErrorAction SilentlyContinue |
  ForEach-Object { Write-Host "  Stopping $($_.Name) PID $($_.Id)"; Stop-Process -Id $_.Id -Force }
Start-Sleep -Milliseconds 800

Write-Host ""
Write-Host "=== Engine -> $INSTALL_ENGINE ==="
# NOTE: Dev manifest patched to uiAccess="false" so overlay works. See
# [[delphi-12-local-patches]] memory. Caveat: keyboard injection into
# elevated apps will not work in dev builds until uiAccess is restored
# and binary is signed.
Copy-IfExists "$REPO\windows\src\engine\keyman\bin\Win32\Debug\keyman.exe"           "$INSTALL_ENGINE\keyman.exe"           'engine'
Copy-IfExists "$REPO\windows\src\engine\kmcomapi\bin\Win32\Debug\kmcomapi.dll"       "$INSTALL_ENGINE\kmcomapi.dll"         'engine'
Copy-IfExists "$REPO\windows\src\engine\insthelper\bin\Win32\Debug\insthelper.dll"   "$INSTALL_ENGINE\insthelper.dll"       'engine'
Copy-IfExists "$REPO\windows\src\engine\tsysinfo\bin\Win32\Debug\tsysinfo.exe"       "$INSTALL_ENGINE\tsysinfo.exe"         'engine'
Copy-IfExists "$REPO\windows\src\engine\tsysinfox64\bin\Win64\Debug\tsysinfox64.exe" "$INSTALL_ENGINE\tsysinfox64.exe"      'engine'

# C++ engine components (Win32 + x64 + arm64 where applicable)
Copy-IfExists "$REPO\windows\src\engine\keyman32\bin\Win32\Debug\keyman32.dll"       "$INSTALL_ENGINE\keyman32.dll"         'engine-cpp'
Copy-IfExists "$REPO\windows\src\engine\keyman32\bin\x64\Debug\keyman64.dll"         "$INSTALL_ENGINE\keyman64.dll"         'engine-cpp'
Copy-IfExists "$REPO\windows\src\engine\kmtip\bin\Win32\Debug\kmtip.dll"             "$INSTALL_ENGINE\kmtip.dll"            'engine-cpp'
Copy-IfExists "$REPO\windows\src\engine\kmtip\bin\x64\Debug\kmtip64.dll"             "$INSTALL_ENGINE\kmtip64.dll"          'engine-cpp'
Copy-IfExists "$REPO\windows\src\engine\mcompile\bin\Win32\Debug\mcompile.exe"       "$INSTALL_ENGINE\mcompile.exe"         'engine-cpp'
Copy-IfExists "$REPO\windows\src\engine\kmrefresh\bin\Win32\Debug\kmrefresh.x86.exe" "$INSTALL_ENGINE\kmrefresh.x86.exe"    'engine-cpp'
Copy-IfExists "$REPO\windows\src\engine\kmrefresh\bin\x64\Debug\kmrefresh.x64.exe"   "$INSTALL_ENGINE\kmrefresh.x64.exe"    'engine-cpp'
Copy-IfExists "$REPO\windows\src\engine\keymanhp\bin\x64\Debug\keymanhp.x64.exe"     "$INSTALL_ENGINE\keymanhp.x64.exe"     'engine-cpp'

Write-Host ""
Write-Host "=== Desktop -> $INSTALL_DESKTOP ==="
Copy-IfExists "$REPO\windows\src\desktop\kmshell\bin\Win32\Debug\kmshell.exe"               "$INSTALL_DESKTOP\kmshell.exe"        'desktop'
Copy-IfExists "$REPO\windows\src\desktop\kmbrowserhost\bin\Win32\Debug\kmbrowserhost.exe"   "$INSTALL_DESKTOP\kmbrowserhost.exe"  'desktop'
Copy-IfExists "$REPO\windows\src\desktop\kmconfig\bin\Win32\Debug\kmconfig.exe"             "$INSTALL_DESKTOP\kmconfig.exe"       'desktop'
Copy-IfExists "$REPO\windows\src\desktop\insthelp\bin\Win32\Debug\insthelp.dll"             "$INSTALL_DESKTOP\insthelp.dll"       'desktop'

# kmshell expects its support data alongside the .exe
Copy-Tree "$REPO\windows\src\desktop\kmshell\xml"    "$INSTALL_DESKTOP\xml"    'desktop-data'
Copy-Tree "$REPO\windows\src\desktop\kmshell\locale" "$INSTALL_DESKTOP\locale" 'desktop-data'

Write-Host ""
Write-Host "=== Developer -> $INSTALL_DEVELOPER ==="
if (-not (Test-Path $INSTALL_DEVELOPER)) {
  Write-Host "  Keyman Developer is not installed. Skipping. (Install from keyman.com if you want TIKE wired up.)" -ForegroundColor Yellow
} else {
  Copy-IfExists "$REPO\developer\src\tike\bin\Win32\Debug\tike.exe"                          "$INSTALL_DEVELOPER\tike.exe"            'developer'
  Copy-IfExists "$REPO\developer\src\kmconvert\bin\Win32\Debug\kmconvert.exe"                "$INSTALL_DEVELOPER\kmconvert.exe"       'developer'
  Copy-IfExists "$REPO\developer\src\kmcmplib\build\x86\debug\src\kmcmplib-19.dll"           "$INSTALL_DEVELOPER\kmcmplib-19.dll"     'developer'
}

Write-Host ""
Write-Host "=== Re-registering kmcomapi.dll at install path ==="
if (Test-Path "$INSTALL_ENGINE\kmcomapi.dll") {
  & regsvr32 /s "$INSTALL_ENGINE\kmcomapi.dll"
  Write-Host "  regsvr32 exit: $LASTEXITCODE (0 = success)"
}

Write-Host ""
Write-Host "=== Done. Launch kmshell from Start Menu or:" -ForegroundColor Green
Write-Host "    & '$INSTALL_DESKTOP\kmshell.exe'"
