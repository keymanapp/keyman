<#
.SYNOPSIS
  Automates the GH-8064 stuck-modifier reproduction: hold a modifier, freeze Keyman so Windows
  silently uninstalls its low level keyboard hook, release the modifier while the hook is gone, then
  make Keyman produce output so an injected batch is assembled.

.DESCRIPTION
  Replaces steps 3 to 7 of README.md's manual procedure. The step that matters is releasing the
  modifier DURING the stall: that is when the KEYUP is dropped.

  The oracle is modifier state, not the text -- a stuck Ctrl or Alt swallows keys silently -- and it
  reads all nine modifier VKs, since do_keybd_event injects the side-agnostic VK.

  READ THIS BEFORE TRUSTING A PASS. An absent stuck modifier means nothing unless a batch was
  actually assembled, which needs a 32-bit host with a Keyman keyboard SELECTED and a keystroke a
  rule transforms. All three are verified here, and an unmet one reports INCONCLUSIVE, not PASS.

  Simulates the user with SendInput, real scan codes and dwExtraInfo 0. That works only because the
  fix identifies Keyman's own events by scan code and dwExtraInfo, not by LLKHF_INJECTED.

.PARAMETER HostApp
  Path to a 32-bit application with a text input field. REQUIRED, and verified to be a WOW64 process
  with a real window: on Windows 11 both notepad.exe and SysWOW64\notepad.exe resolve to the 64-bit
  Notepad, whose engine compiles serialkeyeventserver.cpp out entirely.

.PARAMETER Iterations
  How many times to run the sequence. The defect depends on message ordering, so a single clean run
  is weak evidence. Default 5.

.PARAMETER ReleaseDelayMs
  How long after posting the freeze to release the modifier. Must land inside the five second
  stall. Default 1500.

.PARAMETER Control
  Run the sequence WITHOUT the freeze, so the hook sees both events. Use it to show the harness does
  not wedge the machine on its own.

.PARAMETER Modifier
  Which modifier to hold. RSHIFT is the interesting one for the dwExtraInfo arm of the fix.

.PARAMETER AllowNoTransform
  Proceed even when the typed text comes back unchanged. Only for a keyboard whose rules genuinely
  pass the probe characters through; the run's evidential value drops accordingly.

.EXAMPLE
  ./run-8064-test.ps1 -HostApp 'C:\Path\To\A\32bit\editor.exe'
  ./run-8064-test.ps1 -HostApp ... -Control
  ./run-8064-test.ps1 -HostApp ... -Modifier RSHIFT -Iterations 10
#>
[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)][string]$HostApp,
  [int]$Iterations = 5,
  [int]$ReleaseDelayMs = 1500,
  [switch]$Control,
  [ValidateSet('LSHIFT', 'RSHIFT', 'LCTRL', 'RCTRL', 'LALT', 'RALT')]
  [string]$Modifier = 'LSHIFT',
  [switch]$AllowNoTransform,
  [string]$FakeFreezePath
)

$ErrorActionPreference = 'Stop'

Add-Type -Namespace GH8064 -Name Win32 -MemberDefinition @'
  [StructLayout(LayoutKind.Sequential)]
  public struct KEYBDINPUT {
    public ushort wVk; public ushort wScan; public uint dwFlags; public uint time; public IntPtr dwExtraInfo;
  }
  [StructLayout(LayoutKind.Sequential)]
  public struct INPUT { public uint type; public KEYBDINPUT ki; public int pad1; public int pad2; }
  [DllImport("user32.dll", SetLastError=true)]
  public static extern uint SendInput(uint nInputs, INPUT[] pInputs, int cbSize);
  [DllImport("user32.dll")] public static extern short GetAsyncKeyState(int vKey);
  [DllImport("user32.dll", CharSet=CharSet.Unicode)]
  public static extern IntPtr FindWindow(string cls, string win);
  [DllImport("user32.dll")] public static extern bool SetForegroundWindow(IntPtr hWnd);
  [DllImport("user32.dll")] public static extern IntPtr GetForegroundWindow();
  [DllImport("user32.dll", SetLastError=true)]
  public static extern IntPtr SendMessageTimeout(IntPtr hWnd, uint msg, IntPtr wp, IntPtr lp, uint flags, uint timeout, out IntPtr result);
  [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr h, out uint pid);
  [DllImport("user32.dll")] public static extern IntPtr GetKeyboardLayout(uint tid);
  [DllImport("user32.dll")] public static extern bool EnumChildWindows(IntPtr h, EnumProc cb, IntPtr p);
  public delegate bool EnumProc(IntPtr h, IntPtr p);
  [DllImport("user32.dll", CharSet=CharSet.Unicode)]
  public static extern int GetClassName(IntPtr h, System.Text.StringBuilder s, int n);
  [DllImport("user32.dll", CharSet=CharSet.Unicode)]
  public static extern int SendMessage(IntPtr h, uint m, IntPtr wp, System.Text.StringBuilder lp);
  [DllImport("user32.dll")] public static extern IntPtr SendMessage(IntPtr h, uint m, IntPtr wp, IntPtr lp);
  [DllImport("kernel32.dll", SetLastError=true)]
  public static extern bool IsWow64Process(IntPtr h, out bool wow);
'@

$INPUT_KEYBOARD  = 1
$KEYEVENTF_KEYUP = 0x0002
$KEYEVENTF_EXT   = 0x0001
$WM_GETTEXT      = 0x000D
$WM_GETTEXTLENGTH = 0x000E

$MODIFIERS = [ordered]@{
  LSHIFT = @{ vk = 0xA0; scan = 0x2A; ext = $false }
  RSHIFT = @{ vk = 0xA1; scan = 0x36; ext = $false }
  LCTRL  = @{ vk = 0xA2; scan = 0x1D; ext = $false }
  RCTRL  = @{ vk = 0xA3; scan = 0x1D; ext = $true  }
  LALT   = @{ vk = 0xA4; scan = 0x38; ext = $false }
  RALT   = @{ vk = 0xA5; scan = 0x38; ext = $true  }
}

$ORACLE_VKS = [ordered]@{
  SHIFT = 0x10; CTRL = 0x11; ALT = 0x12
  LSHIFT = 0xA0; RSHIFT = 0xA1; LCTRL = 0xA2; RCTRL = 0xA3; LALT = 0xA4; RALT = 0xA5
}

$PROBE_TEXT = 'aeiouknsd'

# #8064 NOT Write-Host, and this is a correctness constraint rather than style.
# Measured on this machine 2026-08-23 with a congested console host:
#
#     Write-Host               4301 ms per line
#     [Console]::Out.WriteLine    0.4 ms per line
#
# Some of the lines below are emitted BETWEEN the freeze starting and the
# modifier being released -- i.e. inside the race window this test exists to
# open. Four seconds of unplanned dead time there lets the 5 s freeze expire
# before the release, so the iteration silently degenerates into a no-freeze
# control and reports a PASS that means nothing. The failure is asymmetric: it
# can only turn a true FAIL into a false PASS, never the reverse.
function Say([string]$t) { [Console]::Out.WriteLine($t) }

function Send-Key {
  param([int]$Vk, [int]$Scan, [bool]$Up, [bool]$Extended)
  $i = New-Object GH8064.Win32+INPUT
  $i.type = $INPUT_KEYBOARD
  $i.ki.wVk = [uint16]$Vk
  $i.ki.wScan = [uint16]$Scan
  $flags = 0
  if ($Up) { $flags = $flags -bor $KEYEVENTF_KEYUP }
  if ($Extended) { $flags = $flags -bor $KEYEVENTF_EXT }
  $i.ki.dwFlags = [uint32]$flags
  $i.ki.dwExtraInfo = [IntPtr]::Zero
  if ([GH8064.Win32]::SendInput(1, @($i), [System.Runtime.InteropServices.Marshal]::SizeOf($i)) -ne 1) {
    throw ('SendInput failed for vk 0x{0:X2}' -f $Vk)
  }
}

function Get-HeldModifiers {
  $held = @()
  foreach ($e in $ORACLE_VKS.GetEnumerator()) {
    if ([GH8064.Win32]::GetAsyncKeyState($e.Value) -lt 0) { $held += $e.Key }
  }
  return $held
}

function Clear-AllModifiers {
  foreach ($e in $MODIFIERS.GetEnumerator()) {
    Send-Key -Vk $e.Value.vk -Scan $e.Value.scan -Up $true -Extended $e.Value.ext
  }
  foreach ($vk in @(0x10, 0x11, 0x12)) { Send-Key -Vk $vk -Scan 0 -Up $true -Extended $false }
  Start-Sleep -Milliseconds 250
}

function Test-KeymanResponsive {
  # [NullString]::Value, not $null: PowerShell coerces $null to an empty string for a string
  # parameter, and FindWindowW would then match only a window with an empty title.
  $hwnd = [GH8064.Win32]::FindWindow('TfrmKeyman7Main', [NullString]::Value)
  if ($hwnd -eq [IntPtr]::Zero) { return $null }
  $out = [IntPtr]::Zero
  $r = [GH8064.Win32]::SendMessageTimeout($hwnd, 0, [IntPtr]::Zero, [IntPtr]::Zero, 0x0003, 400, [ref]$out)
  return ($r -ne [IntPtr]::Zero)
}

# The first descendant window whose class looks like a text field, plus its text.
function Get-EditChild {
  param([IntPtr]$Root)
  $hit = [IntPtr]::Zero
  $cb = [GH8064.Win32+EnumProc] {
    param($h, $p)
    if ($hit -ne [IntPtr]::Zero) { return $true }
    $c = New-Object System.Text.StringBuilder 128
    [void][GH8064.Win32]::GetClassName($h, $c, 128)
    if ($c.ToString() -match '^(Edit|RichEdit.*|RICHEDIT.*)$') { $script:hit = $h }
    return $true
  }
  $script:hit = [IntPtr]::Zero
  [void][GH8064.Win32]::EnumChildWindows($Root, $cb, [IntPtr]::Zero)
  return $script:hit
}

function Get-WindowText {
  param([IntPtr]$H)
  if ($H -eq [IntPtr]::Zero) { return $null }
  $len = [int][GH8064.Win32]::SendMessage($H, $WM_GETTEXTLENGTH, [IntPtr]::Zero, [IntPtr]::Zero)
  $sb = New-Object System.Text.StringBuilder ($len + 2)
  [void][GH8064.Win32]::SendMessage($H, $WM_GETTEXT, [IntPtr]($len + 1), $sb)
  return $sb.ToString()
}

Write-Host '=== GH-8064 automated reproduction ===' -ForegroundColor Cyan
$abort = @()

# --- Keyman controller ---
$kmWnd = [GH8064.Win32]::FindWindow('TfrmKeyman7Main', [NullString]::Value)
if ($kmWnd -eq [IntPtr]::Zero) { $abort += 'Keyman master controller window (TfrmKeyman7Main) not found; is Keyman running?' }
else { Write-Host ('[OK]   Keyman master controller: 0x{0:X}' -f [int64]$kmWnd) }

# --- fakefreeze ---
if (-not $FakeFreezePath) {
  $repo = Split-Path -Parent (Split-Path -Parent (Split-Path -Parent (Split-Path -Parent $PSScriptRoot)))
  $FakeFreezePath = Join-Path $repo 'src\support\fakefreeze\bin\Win32\Debug\fakefreeze.exe'
}
if (-not (Test-Path $FakeFreezePath)) {
  $abort += "fakefreeze.exe not found at $FakeFreezePath (build: ./windows/src/support/fakefreeze/build.sh --debug build:x86)"
} else { Write-Host "[OK]   fakefreeze: $FakeFreezePath" }

# --- keyboards installed ---
$kbPath = 'HKCU:\Software\Keyman\Keyman Engine\Active Keyboards'
if (Test-Path $kbPath) {
  Write-Host ('[OK]   Keyman keyboards installed: {0}' -f (((Get-ChildItem $kbPath).PSChildName) -join ', '))
} else { $abort += 'No Active Keyboards registry key; no Keyman keyboard is installed.' }

# --- baseline ---
$baseline = Get-HeldModifiers
if ($baseline.Count -gt 0) {
  Write-Host ('[WARN] modifiers held at baseline: {0}; attempting recovery' -f ($baseline -join ', '))
  Clear-AllModifiers
  $baseline = Get-HeldModifiers
  if ($baseline.Count -gt 0) { $abort += ('Modifiers still held after recovery: {0}' -f ($baseline -join ', ')) }
  else { Write-Host '[OK]   recovered; baseline clean' }
} else { Write-Host '[OK]   baseline clean' }

# --- the 32-bit host, verified rather than assumed ---
$proc = $null
$hostWnd = [IntPtr]::Zero
$editWnd = [IntPtr]::Zero
if (-not (Test-Path $HostApp)) {
  $abort += "HostApp not found: $HostApp"
} else {
  $proc = Start-Process -FilePath $HostApp -PassThru
  for ($i = 0; $i -lt 40 -and $hostWnd -eq [IntPtr]::Zero; $i++) {
    Start-Sleep -Milliseconds 250
    try { $proc.Refresh(); $hostWnd = $proc.MainWindowHandle } catch { }
  }
  if ($proc.HasExited) {
    $abort += "HostApp exited immediately; it is probably a launcher for a packaged app. Supply a real 32-bit executable."
  } elseif ($hostWnd -eq [IntPtr]::Zero) {
    $abort += "HostApp never presented a window (MainWindowHandle stayed 0). Without a window this script cannot focus it, and keystrokes would go to whatever window has focus."
  } else {
    $isWow = $false
    try { [void][GH8064.Win32]::IsWow64Process($proc.Handle, [ref]$isWow) } catch { }
    if (-not $isWow) {
      $abort += "HostApp is a 64-bit process. Its engine is keymanx64.dll, where serialkeyeventserver.cpp is compiled out, so the modifier cache under test does not exist in it."
    } else {
      Write-Host ('[OK]   32-bit host: {0} (pid {1}, hwnd 0x{2:X})' -f $HostApp, $proc.Id, [int64]$hostWnd)
    }
    $editWnd = Get-EditChild -Root $hostWnd
    if ($editWnd -eq [IntPtr]::Zero) {
      Write-Host '[WARN] no Edit/RichEdit child found; the text-transform check will be skipped'
    } else {
      Write-Host ('[OK]   text field: 0x{0:X}' -f [int64]$editWnd)
    }
  }
}

# --- a Keyman keyboard SELECTED in the host, not merely installed ---
if ($hostWnd -ne [IntPtr]::Zero) {
  [void][GH8064.Win32]::SetForegroundWindow($hostWnd)
  Start-Sleep -Milliseconds 600
  $fg = [GH8064.Win32]::GetForegroundWindow()
  if ($fg -ne $hostWnd) {
    $abort += ('Could not bring the host to the foreground (foreground is 0x{0:X}). Keystrokes would go elsewhere.' -f [int64]$fg)
  } else {
    $tid = 0
    [void][GH8064.Win32]::GetWindowThreadProcessId($hostWnd, [ref]$tid)
    $hkl = [int64][GH8064.Win32]::GetKeyboardLayout([uint32]$tid)
    # A TIP profile has a non-zero high word that is not a plain layout id. Keyman keyboards are
    # TIPs, so a plain 0x0409xxxx style HKL means the base layout is selected and no rule can fire.
    $isTip = ((($hkl -shr 16) -band 0xF000) -eq 0xF000)
    Write-Host ('       host thread {0}, HKL 0x{1:X8}, TIP profile: {2}' -f $tid, $hkl, $isTip)
    if (-not $isTip) {
      $abort += 'The host has a plain keyboard layout selected, not a Keyman TIP. No rule will fire, no batch will be assembled, and a clean modifier state would prove nothing. Select a Keyman keyboard in the host window and re-run.'
    }
  }
}

if ($abort.Count -gt 0) {
  Write-Host ''
  foreach ($a in $abort) { Write-Host "[FAIL] $a" -ForegroundColor Red }
  if ($proc -and -not $proc.HasExited) { $proc.Kill() }
  Clear-AllModifiers
  Write-Host ''
  Write-Host 'RESULT: INCONCLUSIVE - preconditions not met, nothing was measured.' -ForegroundColor Yellow
  exit 2
}

# ---------------------------------------------------------------------------------------------

$mod = $MODIFIERS[$Modifier]
Write-Host ''
Write-Host ('Holding {0} (vk 0x{1:X2}, scan 0x{2:X2}), {3}, {4} iteration(s), release at {5} ms' -f `
  $Modifier, $mod.vk, $mod.scan, $(if ($Control) { 'NO freeze (control)' } else { 'freeze' }), $Iterations, $ReleaseDelayMs)

$results = @()
try {
  for ($iter = 1; $iter -le $Iterations; $iter++) {
    Say ''
    Say "--- iteration $iter of $Iterations ---"

    [void][GH8064.Win32]::SetForegroundWindow($hostWnd)
    Start-Sleep -Milliseconds 250
    if ([GH8064.Win32]::GetForegroundWindow() -ne $hostWnd) {
      throw 'lost the foreground window mid-run; aborting rather than typing into an unknown window'
    }

    Send-Key -Vk $mod.vk -Scan $mod.scan -Up $false -Extended $mod.ext
    Start-Sleep -Milliseconds 200

    $frozeOk = $null
    if (-not $Control) {
      $ff = Start-Process -FilePath $FakeFreezePath -PassThru -WindowStyle Hidden
      Start-Sleep -Milliseconds 400
      $frozeOk = -not (Test-KeymanResponsive)
      Say ('    freeze active: {0}' -f $(if ($frozeOk) { 'yes' } else { 'NO - controller still responding' }))
    }

    Start-Sleep -Milliseconds $ReleaseDelayMs
    Send-Key -Vk $mod.vk -Scan $mod.scan -Up $true -Extended $mod.ext
    Say ('    modifier released{0}' -f $(if ($Control) { '' } else { ' (inside the stall)' }))

    if (-not $Control) { $ff.WaitForExit(); Start-Sleep -Milliseconds 800 }

    $before = Get-WindowText -H $editWnd
    foreach ($ch in $PROBE_TEXT.ToCharArray()) {
      $vk = [int][char]([string]$ch).ToUpper()
      Send-Key -Vk $vk -Scan 0 -Up $false -Extended $false
      Send-Key -Vk $vk -Scan 0 -Up $true -Extended $false
      Start-Sleep -Milliseconds 60
    }
    Start-Sleep -Milliseconds 600
    $after = Get-WindowText -H $editWnd

    $typed = $null
    if ($editWnd -ne [IntPtr]::Zero) {
      $delta = if ($after.Length -ge $before.Length) { $after.Substring($before.Length) } else { '' }
      $transformed = ($delta -ne '' -and $delta -ne $PROBE_TEXT)
      $typed = $delta
      Say ('    typed "{0}" -> got "{1}"  transformed: {2}' -f $PROBE_TEXT, $delta, $transformed)
    } else {
      $transformed = $null
    }

    $held = Get-HeldModifiers
    if ($held.Count -gt 0) { Say ('    [FAIL] stuck: {0}' -f ($held -join ', ')) }
    else { Say '    [ok] no modifier held' }

    $results += [pscustomobject]@{
      Iteration = $iter; Froze = $frozeOk; Output = $typed; Transformed = $transformed; Stuck = ($held -join ',')
    }

    Clear-AllModifiers
  }
}
finally {
  Clear-AllModifiers
  if ($proc -and -not $proc.HasExited) { $proc.Kill() }
}

Say ''
Write-Host '=== summary ===' -ForegroundColor Cyan
$results | Format-Table -AutoSize

$stuck  = @($results | Where-Object { $_.Stuck -ne '' })
$froze  = @($results | Where-Object { $_.Froze -eq $true })
$xform  = @($results | Where-Object { $_.Transformed -eq $true })

Write-Host ('iterations {0}   freeze confirmed {1}   text transformed {2}   stuck {3}' -f `
  $results.Count, $froze.Count, $xform.Count, $stuck.Count)

$final = Get-HeldModifiers
if ($final.Count -gt 0) {
  Write-Host ('[WARN] modifiers still held at exit: {0}' -f ($final -join ', ')) -ForegroundColor Red
}

if ($stuck.Count -gt 0) {
  Write-Host ''
  Write-Host 'RESULT: FAIL - the defect reproduced. A modifier was held with nothing pressed.' -ForegroundColor Red
  exit 1
}
if (-not $Control -and $froze.Count -eq 0) {
  Write-Host ''
  Write-Host 'RESULT: INCONCLUSIVE - the freeze never took effect, so no KEYUP was ever dropped.' -ForegroundColor Yellow
  Write-Host '        If Keyman runs elevated and this shell does not, UIPI blocks the PostMessage.'
  exit 2
}
if ($xform.Count -eq 0 -and -not $AllowNoTransform) {
  Write-Host ''
  Write-Host 'RESULT: INCONCLUSIVE - Keyman never transformed the typed text, so there is no' -ForegroundColor Yellow
  Write-Host '        evidence a rule fired or a batch was assembled. A clean modifier state here' -ForegroundColor Yellow
  Write-Host '        says nothing about the restore half. Use a keyboard that remaps the probe' -ForegroundColor Yellow
  Write-Host "        characters ('$PROBE_TEXT'), or pass -AllowNoTransform if you are certain." -ForegroundColor Yellow
  exit 2
}

Write-Host ''
Write-Host 'RESULT: PASS - batches were assembled and no modifier stuck in any iteration.' -ForegroundColor Green
exit 0
