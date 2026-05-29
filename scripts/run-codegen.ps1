# Run all Keyman Delphi codegen invocations needed for IDE builds.
#
# This is the standalone equivalent of the codegen steps that build.sh would
# run via its cascade -- but build.sh's full cascade is blocked by Delphi 12
# Community Edition (no CLI builds), so we orchestrate the codegen by hand.
#
# Runs:
#   devtools.exe -buildmessageconstants  (MessageIdentifierConsts.pas)
#   devtools.exe -buildsetupstrings      (Keyman.Setup.System.Locales.pas + per-locale .pas)
#   devtools.exe -buildlocaleindex       (kmshell\locale\index.xml)
#   build_standards_data.exe x5          (ISO6393, BCP47 subtag, BCP47 suppress, LCID, langtags)
#   tsysinfox64.exe -> tsysinfox64.bin -> tsysinfo_x64.res (the chicken-and-egg)
#
# Prerequisites: devtools.dproj and build_standards_data.dproj must already
# be built in the Delphi IDE (Win32 / Debug). The script exits cleanly with a
# message if either is missing.
#
# Style: continues past individual failures, logs everything, and prints a
# success/fail/skip summary at the end. No emojis.

$ErrorActionPreference = 'Continue'

# Auto-detect repo root. Override by setting $env:KEYMAN_ROOT.
if ($env:KEYMAN_ROOT) {
  $REPO = $env:KEYMAN_ROOT
} else {
  # This script lives at <repo>\scripts\, so repo root is one level up.
  $REPO = Split-Path $PSScriptRoot -Parent
}

$DEVTOOLS = "$REPO\common\windows\delphi\tools\devtools\bin\Win32\Debug\devtools.exe"
$BSD      = "$REPO\common\windows\delphi\tools\build_standards_data\bin\Win32\Debug\build_standards_data.exe"

# Counters for the summary at the end.
$script:succeeded = 0
$script:failed    = 0
$script:skipped   = 0

function Resolve-RcExe {
  # Locate the most recent Windows 10/11 SDK rc.exe, falling back to a known
  # version if the SDK install dir cannot be enumerated.
  $sdkRoot = 'C:\Program Files (x86)\Windows Kits\10\bin'
  if (Test-Path $sdkRoot) {
    $candidate = Get-ChildItem $sdkRoot -Directory -ErrorAction SilentlyContinue |
      Where-Object { $_.Name -match '^10\.' } |
      Sort-Object @{Expression={[version]$_.Name}} -Descending |
      ForEach-Object { Join-Path $_.FullName 'x64\rc.exe' } |
      Where-Object { Test-Path $_ } |
      Select-Object -First 1
    if ($candidate) { return $candidate }
  }
  $fallback = 'C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\rc.exe'
  return $fallback
}

function Invoke-Codegen {
  param(
    [string]$Label,
    [string]$Exe,
    [string[]]$ExeArgs,
    [string]$ExpectedOutput,
    [string]$WorkingDir
  )

  Write-Host ""
  Write-Host "--- $Label ---" -ForegroundColor Cyan
  Write-Host "  exe : $Exe"
  if ($ExeArgs) { Write-Host "  args: $($ExeArgs -join ' ')" }
  if ($WorkingDir) { Write-Host "  cwd : $WorkingDir" }
  if ($ExpectedOutput) { Write-Host "  out : $ExpectedOutput" }

  if (-not (Test-Path $Exe)) {
    Write-Host "  STATUS: SKIP (exe not found)" -ForegroundColor Yellow
    $script:skipped++
    return
  }

  $pushed = $false
  if ($WorkingDir) {
    if (-not (Test-Path $WorkingDir)) {
      Write-Host "  STATUS: SKIP (working dir missing: $WorkingDir)" -ForegroundColor Yellow
      $script:skipped++
      return
    }
    Push-Location $WorkingDir
    $pushed = $true
  }

  try {
    & $Exe @ExeArgs
    $exit = $LASTEXITCODE
  } catch {
    Write-Host "  STATUS: FAIL (exception: $($_.Exception.Message))" -ForegroundColor Red
    $script:failed++
    if ($pushed) { Pop-Location }
    return
  }
  if ($pushed) { Pop-Location }

  if ($exit -ne 0) {
    Write-Host "  STATUS: FAIL (exit $exit)" -ForegroundColor Red
    $script:failed++
    return
  }

  if ($ExpectedOutput) {
    if (Test-Path $ExpectedOutput) {
      $size = (Get-Item $ExpectedOutput).Length
      Write-Host "  STATUS: OK ($size bytes)" -ForegroundColor Green
      $script:succeeded++
    } else {
      Write-Host "  STATUS: FAIL (expected output not found)" -ForegroundColor Red
      $script:failed++
    }
  } else {
    Write-Host "  STATUS: OK (exit 0)" -ForegroundColor Green
    $script:succeeded++
  }
}

Write-Host "=== Keyman Delphi codegen ==="
Write-Host "REPO    : $REPO"
Write-Host "DEVTOOLS: $DEVTOOLS"
Write-Host "BSD     : $BSD"

# -----------------------------------------------------------------------------
# Sanity checks
# -----------------------------------------------------------------------------
if (-not (Test-Path $DEVTOOLS)) {
  Write-Host ""
  Write-Host "ERROR: devtools.exe not found." -ForegroundColor Red
  Write-Host "  Expected: $DEVTOOLS"
  Write-Host "  Build devtools.dproj in Delphi IDE first (Win32 / Debug)." -ForegroundColor Yellow
  Write-Host "  Project : $REPO\common\windows\delphi\tools\devtools\devtools.dproj"
  exit 1
}

if (-not (Test-Path $BSD)) {
  Write-Host ""
  Write-Host "ERROR: build_standards_data.exe not found." -ForegroundColor Red
  Write-Host "  Expected: $BSD"
  Write-Host "  Build build_standards_data.dproj in Delphi IDE first (Win32 / Debug)." -ForegroundColor Yellow
  Write-Host "  Project : $REPO\common\windows\delphi\tools\build_standards_data\build_standards_data.dproj"
  exit 1
}

# -----------------------------------------------------------------------------
# devtools.exe codegen
# -----------------------------------------------------------------------------
Write-Host ""
Write-Host "=== devtools.exe ==="

# (a) -buildmessageconstants: required by keyman.dproj and kmshell.dproj
$stringsXml          = "$REPO\windows\src\desktop\kmshell\xml\strings.xml"
$messageConstantsPas = "$REPO\windows\src\global\delphi\cust\MessageIdentifierConsts.pas"
Invoke-Codegen `
  -Label 'devtools -buildmessageconstants' `
  -Exe $DEVTOOLS `
  -ExeArgs @('-buildmessageconstants', $stringsXml, $messageConstantsPas) `
  -ExpectedOutput $messageConstantsPas

# (b) -buildsetupstrings: must run FROM the setup dir with relative paths
#     "locale\" and ".\" (trailing backslashes are required -- matches
#     windows/src/desktop/setup/build.sh:37). Produces
#     Keyman.Setup.System.Locales.pas plus ~32 per-locale Keyman.Setup.System.Locale.<bcp47>.pas files.
$setupDir         = "$REPO\windows\src\desktop\setup"
$setupLocalesPas  = "$setupDir\Keyman.Setup.System.Locales.pas"
Invoke-Codegen `
  -Label 'devtools -buildsetupstrings' `
  -Exe $DEVTOOLS `
  -ExeArgs @('-buildsetupstrings', 'locale\', '.\') `
  -WorkingDir $setupDir `
  -ExpectedOutput $setupLocalesPas

# (c) -buildlocaleindex: produces kmshell\locale\index.xml (matches
#     windows/src/desktop/kmshell/build.sh:63-68 do_build_locale_index).
$kmshellLocale      = "$REPO\windows\src\desktop\kmshell\locale"
$kmshellLocaleIndex = "$kmshellLocale\index.xml"
Invoke-Codegen `
  -Label 'devtools -buildlocaleindex' `
  -Exe $DEVTOOLS `
  -ExeArgs @('-buildlocaleindex', $kmshellLocale, $kmshellLocaleIndex) `
  -ExpectedOutput $kmshellLocaleIndex

# -----------------------------------------------------------------------------
# build_standards_data.exe codegen
# (mirrors common/windows/delphi/tools/build_standards_data/build.sh:41-45)
# -----------------------------------------------------------------------------
Write-Host ""
Write-Host "=== build_standards_data.exe ==="

$dataRoot = "$REPO\resources\standards-data"
$outRoot  = "$REPO\common\windows\delphi\standards"

$iso6393Data  = "$dataRoot\iso639-3\iso639-3.tab"
$iso6393Pas   = "$outRoot\Keyman.System.Standards.ISO6393ToBCP47Registry.pas"
$subtagData   = "$dataRoot\language-subtag-registry\language-subtag-registry"
$subtagPas    = "$outRoot\Keyman.System.Standards.BCP47SubtagRegistry.pas"
$suppressData = "$dataRoot\language-subtag-registry\language-subtag-registry"
$suppressPas  = "$outRoot\Keyman.System.Standards.BCP47SuppressScriptRegistry.pas"
$lcidData     = "$dataRoot\windows-lcid-to-bcp-47\map_clean_win.txt"
$lcidPas      = "$outRoot\Keyman.System.Standards.LCIDToBCP47Registry.pas"
$langtagsData = "$dataRoot\langtags\langtags.json"
$langtagsPas  = "$outRoot\Keyman.System.Standards.LangTagsRegistry.pas"

Invoke-Codegen `
  -Label 'build_standards_data iso6393' `
  -Exe $BSD `
  -ExeArgs @('iso6393', $iso6393Data, $iso6393Pas) `
  -ExpectedOutput $iso6393Pas

Invoke-Codegen `
  -Label 'build_standards_data suppress' `
  -Exe $BSD `
  -ExeArgs @('suppress', $suppressData, $suppressPas) `
  -ExpectedOutput $suppressPas

Invoke-Codegen `
  -Label 'build_standards_data subtag' `
  -Exe $BSD `
  -ExeArgs @('subtag', $subtagData, $subtagPas) `
  -ExpectedOutput $subtagPas

Invoke-Codegen `
  -Label 'build_standards_data lcid' `
  -Exe $BSD `
  -ExeArgs @('lcid', $lcidData, $lcidPas) `
  -ExpectedOutput $lcidPas

Invoke-Codegen `
  -Label 'build_standards_data langtags' `
  -Exe $BSD `
  -ExeArgs @('langtags', $langtagsData, $langtagsPas) `
  -ExpectedOutput $langtagsPas

# -----------------------------------------------------------------------------
# tsysinfox64.exe -> tsysinfox64.bin -> tsysinfo_x64.res
#
# tsysinfo.dproj (Win32) embeds the Win64 tsysinfox64 exe as a binary resource.
# engine.groupproj lists tsysinfo BEFORE tsysinfox64, so this has to be
# hand-orchestrated: build tsysinfox64 first, then drop its exe in as
# tsysinfox64.bin, then compile the .rc to produce tsysinfo_x64.res, then
# build tsysinfo.
# -----------------------------------------------------------------------------
Write-Host ""
Write-Host "=== tsysinfox64 -> tsysinfo_x64.res ==="

$tsysinfox64Exe = "$REPO\windows\src\engine\tsysinfox64\bin\Win64\Debug\tsysinfox64.exe"
$tsysinfoDir    = "$REPO\windows\src\engine\tsysinfo"
$tsysinfox64Bin = "$tsysinfoDir\tsysinfox64.bin"
$tsysinfoRc     = "$tsysinfoDir\tsysinfo_x64.rc"
$tsysinfoRes    = "$tsysinfoDir\tsysinfo_x64.res"

if (-not (Test-Path $tsysinfox64Exe)) {
  Write-Host ""
  Write-Host "  SKIP: tsysinfox64.exe not built yet -- skipping tsysinfo_x64.res generation." -ForegroundColor Yellow
  Write-Host "  Build tsysinfox64.dproj in Delphi IDE first (Win64 / Debug)." -ForegroundColor Yellow
  Write-Host "  Expected: $tsysinfox64Exe"
  $script:skipped++
} elseif (-not (Test-Path $tsysinfoRc)) {
  Write-Host ""
  Write-Host "  SKIP: $tsysinfoRc not found." -ForegroundColor Yellow
  $script:skipped++
} else {
  Write-Host ""
  Write-Host "--- copy tsysinfox64.exe -> tsysinfox64.bin ---" -ForegroundColor Cyan
  Write-Host "  src: $tsysinfox64Exe"
  Write-Host "  dst: $tsysinfox64Bin"
  try {
    Copy-Item $tsysinfox64Exe $tsysinfox64Bin -Force
    $size = (Get-Item $tsysinfox64Bin).Length
    Write-Host "  STATUS: OK ($size bytes)" -ForegroundColor Green
    $script:succeeded++
  } catch {
    Write-Host "  STATUS: FAIL ($($_.Exception.Message))" -ForegroundColor Red
    $script:failed++
  }

  $rc = Resolve-RcExe
  Invoke-Codegen `
    -Label 'rc.exe tsysinfo_x64.rc' `
    -Exe $rc `
    -ExeArgs @('/nologo', 'tsysinfo_x64.rc') `
    -WorkingDir $tsysinfoDir `
    -ExpectedOutput $tsysinfoRes
}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
Write-Host ""
Write-Host "=== Summary ===" -ForegroundColor Cyan
Write-Host ("  succeeded: {0}" -f $script:succeeded) -ForegroundColor Green
if ($script:failed -gt 0) {
  Write-Host ("  failed   : {0}" -f $script:failed) -ForegroundColor Red
} else {
  Write-Host ("  failed   : {0}" -f $script:failed)
}
if ($script:skipped -gt 0) {
  Write-Host ("  skipped  : {0}" -f $script:skipped) -ForegroundColor Yellow
} else {
  Write-Host ("  skipped  : {0}" -f $script:skipped)
}

if ($script:failed -gt 0) {
  exit 1
}
exit 0
