<#
.SYNOPSIS
  Elevated setup for the GH-8064 evidence run: installs the test keyboard, and swaps the installed
  keyman32.dll for the branch build (or restores it).

.DESCRIPTION
  Both operations need administrator rights, so neither is part of host32.exe: installing a Keyman
  keyboard requires admin, and keyman32.dll lives under Program Files (x86) and is loaded into every
  hooked 32-bit process, so Keyman must be stopped before it can be replaced.

  The DLL swap always writes a timestamped backup first, and -Restore puts the most recent one back.
  Nothing here touches the user's own keyboards.

  Intended order for the experiment:

    1. -InstallKeyboard                      (elevated)
    2. run host32 against the RELEASE build  (not elevated)  -- a FAIL is the reproduction
    3. -DeployBranchBuild                    (elevated)
    4. run host32 again                      (not elevated)  -- a PASS closes the issue
    5. -Restore                              (elevated)      -- put the shipped engine back

  Step 2 matters as much as step 4: without it a PASS cannot be told from a defect that never
  reproduced on this machine.

.PARAMETER InstallKeyboard
  Installs common/test/keyboards/baseline/k_0301___multiple_deadkeys.kmx. Chosen because it is
  already compiled and its rules are unshifted -- typing 1x yields "1=OK " with no Shift involved,
  which matters because Shift is the modifier under test.

.PARAMETER DeployBranchBuild
  Backs up the installed keyman32.dll, stops Keyman, copies the branch build over it, restarts
  Keyman.

.PARAMETER Restore
  Stops Keyman, restores the most recent backup, restarts Keyman.

.EXAMPLE
  ./deploy-8064.ps1 -InstallKeyboard
  ./deploy-8064.ps1 -DeployBranchBuild
  ./deploy-8064.ps1 -Restore
#>
[CmdletBinding()]
param(
  [switch]$InstallKeyboard,
  [switch]$DeployBranchBuild,
  [switch]$Restore
)

$ErrorActionPreference = 'Stop'

$repo       = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $PSScriptRoot)))
$branchDll  = Join-Path $repo 'src\engine\keyman32\bin\Win32\Debug\keyman32.dll'
$testKbd    = Join-Path (Split-Path -Parent $repo) 'common\test\keyboards\baseline\k_0301___multiple_deadkeys.kmx'
$engineDir  = 'C:\Program Files (x86)\Common Files\Keyman\Keyman Engine'
$installed  = Join-Path $engineDir 'keyman32.dll'
$kmshell    = 'C:\Program Files (x86)\Keyman\Keyman Desktop\kmshell.exe'
$backupDir  = Join-Path $engineDir 'gh8064-backup'

function Assert-Elevated {
  $id = [Security.Principal.WindowsIdentity]::GetCurrent()
  $pr = New-Object Security.Principal.WindowsPrincipal($id)
  if (-not $pr.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host '[FAIL] This script needs administrator rights.' -ForegroundColor Red
    Write-Host '       Installing a Keyman keyboard requires admin, and keyman32.dll lives under'
    Write-Host '       Program Files (x86). Re-run from an elevated PowerShell.'
    exit 3
  }
}

function Stop-Keyman {
  foreach ($n in @('keyman', 'keymanx64', 'kmshell')) {
    Get-Process $n -ErrorAction SilentlyContinue | ForEach-Object {
      Write-Host "       stopping $($_.ProcessName) (pid $($_.Id))"
      try { $_.Kill(); $_.WaitForExit(5000) } catch { }
    }
  }
  Start-Sleep -Seconds 2
}

function Start-Keyman {
  $exe = Join-Path $engineDir 'keyman.exe'
  if (Test-Path $exe) {
    Start-Process $exe | Out-Null
    Start-Sleep -Seconds 4
    if (Get-Process keyman -ErrorAction SilentlyContinue) {
      Write-Host '[OK]   Keyman restarted'
    } else {
      Write-Host '[WARN] Keyman did not come back up; start it from the Start menu' -ForegroundColor Yellow
    }
  }
}

if (-not ($InstallKeyboard -or $DeployBranchBuild -or $Restore)) {
  Write-Host 'Nothing to do. Pass -InstallKeyboard, -DeployBranchBuild or -Restore.'
  Write-Host 'See the comment at the top of this file for the intended order.'
  exit 0
}

Assert-Elevated

if ($InstallKeyboard) {
  Write-Host '=== installing the test keyboard ===' -ForegroundColor Cyan
  if (-not (Test-Path $testKbd)) { Write-Host "[FAIL] not found: $testKbd" -ForegroundColor Red; exit 3 }
  if (-not (Test-Path $kmshell)) { Write-Host "[FAIL] kmshell not found: $kmshell" -ForegroundColor Red; exit 3 }

  Write-Host "       $testKbd"
  & $kmshell -s -nowelcome -i $testKbd
  Start-Sleep -Seconds 5

  $kbds = (Get-ChildItem 'HKCU:\Software\Keyman\Keyman Engine\Active Keyboards' -ErrorAction SilentlyContinue).PSChildName
  Write-Host ('[OK]   active keyboards now: {0}' -f ($kbds -join ', '))
  # -notmatch on an ARRAY is a filter, not a boolean: it returns the non-matching
  # elements, which are truthy whenever any other keyboard is installed. That made
  # this warn on a successful install. Test for presence explicitly instead.
  if (-not ($kbds | Where-Object { $_ -match '0301' })) {
    Write-Host '[WARN] the test keyboard does not appear in the list. Install it by hand from' -ForegroundColor Yellow
    Write-Host '       Keyman Configuration -> Keyboards -> Install, then continue.' -ForegroundColor Yellow
  }
  Write-Host ''
  Write-Host 'Next: run host32 WITHOUT elevation, against the shipped engine. Select the new'
  Write-Host 'keyboard when it asks (Win+Space). A FAIL there is the reproduction.'
}

if ($DeployBranchBuild) {
  Write-Host '=== deploying the branch build ===' -ForegroundColor Cyan
  if (-not (Test-Path $branchDll)) {
    Write-Host "[FAIL] branch build not found: $branchDll" -ForegroundColor Red
    Write-Host '       Build it: ./windows/src/engine/keyman32/build.sh --debug build:x86'
    exit 3
  }
  if (-not (Test-Path $installed)) { Write-Host "[FAIL] installed engine not found: $installed" -ForegroundColor Red; exit 3 }

  # Never back up a DLL that is already the branch build. Doing so poisons the backup
  # chain: -Restore takes the newest by name, and a second deploy makes that the branch
  # build, so restoring silently reinstates it. Deploying twice is a no-op, not an error.
  if ((Get-Item $installed).Length -eq (Get-Item $branchDll).Length) {
    Write-Host '[SKIP] the branch build is already installed; not backing it up over itself.' -ForegroundColor Yellow
    Write-Host '       Run -Restore first if you meant to redeploy a rebuilt DLL.'
    exit 0
  }

  if (-not (Test-Path $backupDir)) { New-Item -ItemType Directory -Path $backupDir | Out-Null }
  $stamp  = Get-Date -Format 'yyyyMMdd-HHmmss'
  $backup = Join-Path $backupDir "keyman32.dll.$stamp"
  Copy-Item $installed $backup
  Write-Host "[OK]   backed up to $backup"

  Stop-Keyman
  try {
    # keyman32.dll is injected into every hooked 32-bit process, so stopping Keyman does not release
    # it and an overwrite fails with a sharing violation. Renaming works -- open handles reference
    # the file object, not the path -- and host32 and the restarted Keyman load the new DLL fresh.
    $aside = "$installed.inuse-$stamp"
    Move-Item $installed $aside -Force
    Write-Host "[OK]   moved the in-use engine aside: $(Split-Path -Leaf $aside)"
    Copy-Item $branchDll $installed -Force
    Write-Host ('[OK]   deployed branch build ({0:N0} bytes, was {1:N0})' -f `
      (Get-Item $installed).Length, (Get-Item $backup).Length)
  }
  finally {
    # Always bring Keyman back, even if the swap failed. Leaving it stopped takes the user's
    # keyboarding down, which is a worse outcome than a failed deploy.
    Start-Keyman
  }

  Write-Host ''
  Write-Host 'Next: run host32 again. A PASS now, against a FAIL before, is the evidence.'
  Write-Host 'Afterwards: ./deploy-8064.ps1 -Restore'
}

if ($Restore) {
  Write-Host '=== restoring the shipped engine ===' -ForegroundColor Cyan
  if (-not (Test-Path $backupDir)) { Write-Host '[FAIL] no backup directory; nothing to restore' -ForegroundColor Red; exit 3 }
  # Take the newest backup that is NOT a branch build. Running -DeployBranchBuild twice
  # backs the branch DLL up over itself, and "newest by name" then restores the very
  # thing we are trying to undo -- while printing [OK] restored. Observed 2026-08-28:
  # backups ...120633 (1,232,504 shipped) and ...120642 (4,197,376 branch), nine
  # seconds apart, and -Restore chose the branch one.
  $branchLen = 0
  if (Test-Path $branchDll) { $branchLen = (Get-Item $branchDll).Length }
  $all = @(Get-ChildItem $backupDir -Filter 'keyman32.dll.*' | Sort-Object Name -Descending)
  $backup = $all | Where-Object { $branchLen -eq 0 -or $_.Length -ne $branchLen } | Select-Object -First 1
  if (-not $backup) {
    Write-Host '[FAIL] no backup found that differs from the branch build.' -ForegroundColor Red
    Write-Host '       Every backup looks like the branch DLL, so there is nothing safe to' -ForegroundColor Red
    Write-Host '       restore. Reinstall Keyman, or copy a shipped keyman32.dll in by hand.' -ForegroundColor Red
    exit 3
  }
  if ($all.Count -gt 0 -and $backup.Name -ne $all[0].Name) {
    Write-Host ('[NOTE] skipping newer backup {0} -- it is a branch build, not a shipped one' -f $all[0].Name) -ForegroundColor Yellow
  }

  Stop-Keyman
  try {
    if (Test-Path $installed) {
      Move-Item $installed "$installed.replaced-$(Get-Date -Format 'yyyyMMdd-HHmmss')" -Force
    }
    Copy-Item $backup.FullName $installed -Force
    Write-Host ('[OK]   restored {0} ({1:N0} bytes)' -f $backup.Name, (Get-Item $installed).Length)
  }
  finally {
    Start-Keyman
  }
  Write-Host '       stale .inuse-* / .replaced-* copies in the engine directory can be deleted once'
  Write-Host '       every process that had the old DLL loaded has exited (a reboot clears them).'
}
