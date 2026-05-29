<#
.SYNOPSIS
  Generate .res files (and manifest.xml) for every Delphi project in the
  Keyman repo, so the projects open and compile cleanly in the Delphi IDE.

.DESCRIPTION
  Delphi 12 Community Edition blocks CLI builds, which means build.sh's
  do_build rc-step never runs locally. This script is the standalone
  equivalent: walk every directory containing a .dproj, expand manifest.in
  -> manifest.xml using VERSION.md, then compile every .rc next to it with
  rc.exe from the Windows 10 SDK. Output .res files land alongside their
  source .rc.

  Run this:
    * After a fresh clone / `git clean`.
    * After bumping VERSION.md (so manifest.xml is re-stamped).
    * After editing any .rc / .in / referenced .bin (e.g. tsysinfox64.bin).

  After regenerating .res, in Delphi do: right-click project -> Clean,
  then Build. An incremental Build silently re-embeds the stale cached .res.

  Codegen steps that depend on Delphi-built tools (devtools.exe,
  build_standards_data.exe) live in scripts/run-codegen.ps1, not here.

.PARAMETER Project
  Optional. One or more directories to limit the preflight to. May be a
  path relative to the repo (e.g. 'windows\src\engine\keyman') or an
  absolute path. If omitted, every dir containing a .dproj is processed.

.PARAMETER Repo
  Repo root. Defaults to $env:KEYMAN_ROOT, then auto-detects from this
  script's location (assumes the script lives at <repo>\scripts\).

.EXAMPLE
  .\preflight-resources.ps1

.EXAMPLE
  .\preflight-resources.ps1 -Project windows\src\engine\tsysinfo
#>

[CmdletBinding()]
param(
  [string[]]$Project,
  [string]$Repo
)

$ErrorActionPreference = 'Continue'

if (-not $Repo) {
  if ($env:KEYMAN_ROOT) { $Repo = $env:KEYMAN_ROOT }
  else                  { $Repo = Split-Path $PSScriptRoot -Parent }
}

if (-not (Test-Path $Repo)) {
  Write-Host "Repo root not found: $Repo" -ForegroundColor Red
  exit 1
}

# ----------------------------------------------------------------------------
# Locate Windows 10 SDK (rc.exe + um/shared/winrt include dirs)
# ----------------------------------------------------------------------------

function Find-WindowsSdk {
  $sdkRoot = 'C:\Program Files (x86)\Windows Kits\10'
  $includeRoot = Join-Path $sdkRoot 'Include'
  if (-not (Test-Path $includeRoot)) {
    throw "Windows 10 SDK not found at $sdkRoot"
  }

  # Preferred versions first, then newest 10.0.* fallback
  $preferred = @('10.0.26100.0', '10.0.19041.0')
  $available = Get-ChildItem -Path $includeRoot -Directory |
    Where-Object { $_.Name -like '10.0.*' } |
    Select-Object -ExpandProperty Name

  $chosen = $null
  foreach ($v in $preferred) {
    if ($available -contains $v) { $chosen = $v; break }
  }
  if (-not $chosen) {
    # newest by version sort
    $chosen = $available |
      Sort-Object { [version]($_ -replace '^10\.0\.', '10.0.') } -Descending |
      Select-Object -First 1
  }
  if (-not $chosen) {
    throw "No 10.0.* SDK version found under $includeRoot"
  }

  $rcCandidates = @(
    (Join-Path $sdkRoot "bin\$chosen\x64\rc.exe"),
    (Join-Path $sdkRoot "bin\$chosen\x86\rc.exe")
  )
  $rc = $rcCandidates | Where-Object { Test-Path $_ } | Select-Object -First 1
  if (-not $rc) {
    throw "rc.exe not found for SDK $chosen (looked in $($rcCandidates -join '; '))"
  }

  [pscustomobject]@{
    Version = $chosen
    Rc      = $rc
    IncUm     = Join-Path $sdkRoot "Include\$chosen\um"
    IncShared = Join-Path $sdkRoot "Include\$chosen\shared"
    IncWinrt  = Join-Path $sdkRoot "Include\$chosen\winrt"
  }
}

$Sdk = Find-WindowsSdk
Write-Host "Windows SDK: $($Sdk.Version)" -ForegroundColor Cyan
Write-Host "rc.exe:      $($Sdk.Rc)"
Write-Host ""

# ----------------------------------------------------------------------------
# Version string for manifest.xml ($VersionWin substitution)
# ----------------------------------------------------------------------------

$versionFile = Join-Path $Repo 'VERSION.md'
if (-not (Test-Path $versionFile)) {
  Write-Host "VERSION.md not found at $versionFile" -ForegroundColor Red
  exit 1
}
$Version4 = (Get-Content $versionFile).Trim() + '.0'
Write-Host "Version:     $Version4 (from VERSION.md)" -ForegroundColor Cyan
Write-Host ""

# ----------------------------------------------------------------------------
# Counters
# ----------------------------------------------------------------------------

$script:Generated = New-Object System.Collections.Generic.List[string]
$script:Skipped   = New-Object System.Collections.Generic.List[string]
$script:Failed    = New-Object System.Collections.Generic.List[string]

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

function Test-NeedsSdkIncludes {
  param([string]$RcPath)

  if (-not (Test-Path $RcPath)) { return $false }
  $needles = @('<windows.h>', '<winres.h>', '<commctrl.h>')
  $content = Get-Content $RcPath -Raw -ErrorAction SilentlyContinue
  if (-not $content) { return $false }
  foreach ($n in $needles) {
    if ($content -match [regex]::Escape($n)) { return $true }
  }
  return $false
}

function Write-ManifestXml {
  param(
    [string]$ProjectDir,
    [string]$Version
  )

  $manifestIn  = Join-Path $ProjectDir 'manifest.in'
  $manifestXml = Join-Path $ProjectDir 'manifest.xml'
  if (-not (Test-Path $manifestIn)) { return }

  try {
    $raw = Get-Content $manifestIn -Raw
    $expanded = $raw -replace '\$VersionWin', $Version
    # Write UTF-8 without BOM, no trailing newline
    [System.IO.File]::WriteAllText($manifestXml, $expanded, (New-Object System.Text.UTF8Encoding $false))
    $size = (Get-Item $manifestXml).Length
    Write-Host ("  [manifest] {0} ({1} bytes)" -f $manifestXml, $size) -ForegroundColor Green
    $script:Generated.Add($manifestXml)
  } catch {
    Write-Host "  [manifest] FAILED: $manifestIn -> $($_.Exception.Message)" -ForegroundColor Red
    $script:Failed.Add("$manifestIn ($($_.Exception.Message))")
  }
}

function Invoke-RcCompile {
  param(
    [string]$RcPath
  )

  $dir = Split-Path $RcPath -Parent
  $base = [System.IO.Path]::GetFileNameWithoutExtension($RcPath)
  $resPath = Join-Path $dir "$base.res"

  $needsSdk = Test-NeedsSdkIncludes -RcPath $RcPath
  $rcArgs = @('/nologo')
  if ($needsSdk) {
    $rcArgs += '/I'; $rcArgs += $Sdk.IncUm
    $rcArgs += '/I'; $rcArgs += $Sdk.IncShared
    $rcArgs += '/I'; $rcArgs += $Sdk.IncWinrt
  }
  # Pass just the filename so output .res lands in the rc's directory
  $rcArgs += (Split-Path $RcPath -Leaf)

  Push-Location $dir
  try {
    $output = & $Sdk.Rc @rcArgs 2>&1
    $code = $LASTEXITCODE
  } finally {
    Pop-Location
  }

  if ($code -ne 0) {
    $msg = ($output | Out-String).Trim()
    Write-Host ("  [rc]       FAILED: {0}" -f $RcPath) -ForegroundColor Red
    if ($msg) {
      foreach ($line in $msg -split "`r?`n") {
        Write-Host "             $line" -ForegroundColor Red
      }
    }
    $script:Failed.Add("$RcPath (rc.exe exit $code)")
    return
  }

  if (Test-Path $resPath) {
    $size = (Get-Item $resPath).Length
    $tag = if ($needsSdk) { 'rc+sdk' } else { 'rc' }
    Write-Host ("  [{0,-7}] {1} ({2} bytes)" -f $tag, $resPath, $size) -ForegroundColor Green
    $script:Generated.Add($resPath)
  } else {
    Write-Host "  [rc]       no .res produced for $RcPath" -ForegroundColor Yellow
    $script:Skipped.Add("$RcPath (no .res produced)")
  }
}

function Invoke-Preflight {
  param([string]$Dir)

  $rel = $Dir
  if ($Dir.StartsWith($Repo, [System.StringComparison]::OrdinalIgnoreCase)) {
    $rel = $Dir.Substring($Repo.Length).TrimStart('\','/')
  }
  Write-Host ("--- {0}" -f $rel) -ForegroundColor White

  # (a) manifest.in -> manifest.xml (must come before manifest.rc compile)
  Write-ManifestXml -ProjectDir $Dir -Version $Version4

  # (b) Compile every .rc under this dir (recursive picks up langswitch\*.rc etc.)
  $rcFiles = Get-ChildItem -Path $Dir -Recurse -Filter '*.rc' -File -ErrorAction SilentlyContinue

  if (-not $rcFiles -or $rcFiles.Count -eq 0) {
    Write-Host "  (no .rc files in this project dir)" -ForegroundColor DarkGray
    $script:Skipped.Add("$Dir (no .rc files)")
    return
  }

  foreach ($rc in $rcFiles) {
    # Special case: tsysinfo_x64.rc embeds tsysinfox64.bin which is itself
    # the output of building tsysinfox64.dproj in Win64/Debug. If the bin
    # isn't present yet, skip with a pointer to the codegen helper.
    if ($rc.Name -ieq 'tsysinfo_x64.rc') {
      $binPath = Join-Path $rc.DirectoryName 'tsysinfox64.bin'
      if (-not (Test-Path $binPath)) {
        $msg = "tsysinfox64.bin missing -- build tsysinfox64.dproj (Win64/Debug) first, then run scripts\run-codegen.ps1 (or copy the exe to $binPath)"
        Write-Host "  [rc]       SKIP $($rc.FullName): $msg" -ForegroundColor Yellow
        $script:Skipped.Add("$($rc.FullName) ($msg)")
        continue
      }
    }

    Invoke-RcCompile -RcPath $rc.FullName
  }
}

# ----------------------------------------------------------------------------
# Resolve target project dirs
# ----------------------------------------------------------------------------

function Resolve-ProjectArg {
  param([string]$Arg)

  if ([System.IO.Path]::IsPathRooted($Arg)) {
    return $Arg
  }
  return (Join-Path $Repo $Arg)
}

if ($Project -and $Project.Count -gt 0) {
  $targetDirs = @()
  foreach ($p in $Project) {
    $resolved = Resolve-ProjectArg -Arg $p
    if (-not (Test-Path $resolved)) {
      Write-Host "Project path not found: $resolved" -ForegroundColor Red
      $script:Failed.Add("$resolved (path not found)")
      continue
    }
    $targetDirs += (Resolve-Path $resolved).Path
  }
} else {
  Write-Host "Discovering .dproj files under $Repo ..." -ForegroundColor Cyan
  $dprojs = Get-ChildItem -Path $Repo -Recurse -Filter '*.dproj' -File -ErrorAction SilentlyContinue
  $targetDirs = $dprojs |
    ForEach-Object { $_.DirectoryName } |
    Sort-Object -Unique
  Write-Host ("Found {0} project dir(s) with .dproj files." -f $targetDirs.Count) -ForegroundColor Cyan
  Write-Host ""
}

if (-not $targetDirs -or $targetDirs.Count -eq 0) {
  Write-Host "Nothing to do." -ForegroundColor Yellow
  exit 0
}

# ----------------------------------------------------------------------------
# Process each project dir
# ----------------------------------------------------------------------------

foreach ($d in $targetDirs) {
  Invoke-Preflight -Dir $d
}

# ----------------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------------

Write-Host ""
Write-Host "===== Summary =====" -ForegroundColor Cyan
Write-Host ("  Generated: {0}" -f $script:Generated.Count) -ForegroundColor Green
Write-Host ("  Skipped:   {0}" -f $script:Skipped.Count)   -ForegroundColor Yellow
Write-Host ("  Failed:    {0}" -f $script:Failed.Count)    -ForegroundColor $(if ($script:Failed.Count) { 'Red' } else { 'Gray' })

if ($script:Skipped.Count -gt 0) {
  Write-Host ""
  Write-Host "Skipped items:" -ForegroundColor Yellow
  foreach ($s in $script:Skipped) { Write-Host "  - $s" -ForegroundColor Yellow }
}

if ($script:Failed.Count -gt 0) {
  Write-Host ""
  Write-Host "Failed items:" -ForegroundColor Red
  foreach ($f in $script:Failed) { Write-Host "  - $f" -ForegroundColor Red }
  exit 1
}

exit 0
