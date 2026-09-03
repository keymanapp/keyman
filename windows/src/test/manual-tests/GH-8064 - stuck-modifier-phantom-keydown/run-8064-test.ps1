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
  actually assembled, which needs a host with a Keyman keyboard SELECTED and a keystroke a rule
  transforms. Both are verified here, and an unmet one reports INCONCLUSIVE, not PASS.

  Host bitness is NOT a precondition. It was treated as one until 2026-08-31 -- see the host block
  in the body for why that was wrong. It is still reported on the result line, because no run of
  this script against a 64-bit host has been recorded yet.

  Simulates the user with SendInput, real scan codes and dwExtraInfo 0. That works only because the
  fix identifies Keyman's own events by scan code and dwExtraInfo, not by LLKHF_INJECTED.

.PARAMETER HostApp
  Path to an application with a text input field. REQUIRED, and verified to present a real window --
  without one this script cannot focus it and keystrokes would land wherever focus happens to be.

  host32/ is the RECOMMENDED host and the one behind the recorded before/after pair: a plain Win32
  window with a single Edit control, so it avoids the packaged-app stub and multi-threaded frame
  window complications Windows 11 Notepad brings.

  DO NOT use Windows 11 Notepad. Neither copy works as a host here: System32
otepad.exe hands the
  request to the packaged Notepad -- a separate, already-running process that owns the tabbed
  window -- and then sits there with no window of its own, so this script aborts before measuring
  anything. Measured 2026-08-31, see evidence/notepad64-stock-19.0.276.txt.

  For a stock host, charmap.exe is the one that behaves: SysWOW64\charmap.exe for 32-bit,
  System32\charmap.exe for 64-bit. Both present a real top-level window with an Edit control.
  Run those from a 64-BIT shell: under a 32-bit PowerShell the file system redirector turns
  System32 into SysWOW64 and you silently measure the 32-bit host twice. The bitness printed on
  the result line is the one that was actually launched, so a saved run always says which it was.

  Bitness is recorded, not enforced. See the host block in the body.

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
  # An ordinary app with a text field. NOT host32.exe -- that is a separate harness
  # which drives its own sequence; run it directly with --fakefreeze.
  ./run-8064-test.ps1 -HostApp 'C:\Windows\SysWOW64\charmap.exe'   # 32-bit stock host
  ./run-8064-test.ps1 -HostApp 'C:\Windows\System32\charmap.exe'    # 64-bit stock host
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
  [StructLayout(LayoutKind.Sequential)]
  public struct RECT { public int left, top, right, bottom; }
  [StructLayout(LayoutKind.Sequential)]
  public struct GUITHREADINFO {
    public uint cbSize; public uint flags;
    public IntPtr hwndActive, hwndFocus, hwndCapture, hwndMenuOwner, hwndMoveSize, hwndCaret;
    public RECT rcCaret;
  }
  [DllImport("user32.dll", SetLastError=true)]
  public static extern bool GetGUIThreadInfo(uint tid, ref GUITHREADINFO gti);
'@

# Whether a Keyman keyboard is SELECTED cannot be read off the HKL -- see the precondition
# block near the bottom for why -- so ask TSF directly. ITfInputProcessorProfiles::
# GetActiveLanguageProfile answers for the CALLING thread, which under the Windows default
# ("use the same input method for all app windows") is the system-wide selection; the host's
# own thread is corroborated separately by its HKL langid.
Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
namespace GH8064 {
  public class TsfProfile {
    public int Hr; public ushort LangId; public Guid Profile;
    public bool Active { get { return Hr >= 0 && Profile != Guid.Empty; } }
  }
  [ComImport, Guid("1F02B6C5-7842-4EE6-8A0B-9A24183A95CA"),
   InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
  public interface ITfInputProcessorProfiles {
    // Vtable order matters and nothing before the last slot is ever called, so the
    // unused entries are declared as bare placeholders to hold their positions.
    void Register(); void Unregister(); void AddLanguageProfile(); void RemoveLanguageProfile();
    void EnumInputProcessorInfo(); void GetDefaultLanguageProfile(); void SetDefaultLanguageProfile();
    void ActivateLanguageProfile();
    [PreserveSig] int GetActiveLanguageProfile([In] ref Guid rclsid, out ushort langid, out Guid guidProfile);
  }
  public static class Tsf {
    public static TsfProfile ActiveProfileOf(string clsid) {
      var o = (ITfInputProcessorProfiles)Activator.CreateInstance(
        Type.GetTypeFromCLSID(new Guid("33C53A50-F456-4884-B049-85FD643ECFED")));  // CLSID_TF_InputProcessorProfiles
      try {
        Guid c = new Guid(clsid); ushort lang; Guid prof;
        int hr = o.GetActiveLanguageProfile(ref c, out lang, out prof);
        return new TsfProfile { Hr = hr, LangId = lang, Profile = prof };
      } finally { Marshal.ReleaseComObject(o); }
    }
  }
}
'@

# Keyman Engine Text Service, registered under TFCAT_TIP_KEYBOARD.
# See windows/src/engine/inst/components.wxs:198 and windows/include/kmtip_guids.h.
$KMTIP_CLSID = 'FE0420F1-38D1-4B4C-96BF-E7E20A74CFB7'

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

# The profile GUID alone does not say WHICH keyboard is selected, and "a Keyman keyboard is
# active" is a weaker statement than "Cameroon QWERTY is active" when reading a saved run.
# Best effort only: an unnamed profile is still a valid one.
function Get-ProfileName {
  param([string]$Clsid, [int]$LangId, [guid]$Profile)
  try {
    $k = 'HKLM:\SOFTWARE\Microsoft\CTF\TIP\{0}\LanguageProfile\0x{1:X8}\{{{2}}}' -f `
      "{$Clsid}", $LangId, $Profile.ToString().ToUpper()
    return (Get-ItemProperty -Path $k -ErrorAction Stop).Description
  } catch { return $null }
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

# --- the host, with its bitness recorded rather than required ---
$proc = $null
$hostWnd = [IntPtr]::Zero
$editWnd = [IntPtr]::Zero
# Defined up front so the result line can print it on every path, including the ones that never
# reach the bitness probe below.
$hostBitness = 'unknown'
if (-not (Test-Path $HostApp)) {
  $abort += "HostApp not found: $HostApp"
} else {
  $proc = Start-Process -FilePath $HostApp -PassThru
  for ($i = 0; $i -lt 40 -and $hostWnd -eq [IntPtr]::Zero; $i++) {
    Start-Sleep -Milliseconds 250
    try { $proc.Refresh(); $hostWnd = $proc.MainWindowHandle } catch { }
  }
  # MainWindowHandle on an exited process yields $null, NOT [IntPtr]::Zero, and
  # `$null -ne [IntPtr]::Zero` is TRUE -- so every downstream guard passes and the first
  # P/Invoke dies with "Cannot convert null to type System.IntPtr" nineteen lines before
  # the abort list would have reported the real problem. Normalise it here.
  if ($null -eq $hostWnd) { $hostWnd = [IntPtr]::Zero }

  if ($proc.HasExited) {
    $abort += "HostApp exited immediately. NOTE: host32.exe is NOT a passive host for this script -- it is a separate harness that drives its own sequence and requires --fakefreeze, so run it directly instead. This script needs an ordinary application with a text field."
  } elseif ($hostWnd -eq [IntPtr]::Zero) {
    $abort += "HostApp never presented a window (MainWindowHandle stayed 0). Without a window this script cannot focus it, and keystrokes would go to whatever window has focus. On Windows 11 this is what a packaged app looks like from out here: System32
otepad.exe hands the request to the packaged Notepad -- a SEPARATE, already-running process that owns the tabbed window -- and then stays alive with no window of its own. Adopting that window is not an option either, since the cleanup below would kill the user's Notepad with it. Use System32\charmap.exe for a 64-bit host, SysWOW64\charmap.exe for a 32-bit one, or host32/ (recommended)."
  } else {
    # Host bitness is REPORTED, not required. It was a hard abort until 2026-08-31, on the
    # reasoning that serialkeyeventserver.cpp being #ifndef _WIN64 makes a 64-bit host immune.
    # That does not follow: the server, the hook and the cache live in 32-bit keyman.exe, but
    # WH_KEYBOARD_LL is system-wide and serialkeyeventclient.cpp has no _WIN64 guard -- a 64-bit
    # client reaches the same single server by unsuffixed global name through the memory-mapped
    # file. stuck-mod-test.ps1 reproduces the wedge against 64-bit Notepad and has never had a
    # bitness check.
    #
    # host32 remains the RECOMMENDED host: it is the one used for the recorded before/after pair,
    # it drives the sequence itself, and it removes the packaged-app and multi-threaded-frame
    # complications Windows 11 Notepad brings. A 64-bit run is not yet backed by a recorded
    # measurement through THIS script, so it warns and records rather than passing silently.
    $isWow = $false
    $bitnessKnown = $true
    try { [void][GH8064.Win32]::IsWow64Process($proc.Handle, [ref]$isWow) } catch { $bitnessKnown = $false }
    if (-not $bitnessKnown) {
      $hostBitness = 'unknown'
      Write-Host ('[WARN] could not determine host bitness for {0}; recorded as unknown' -f $HostApp)
    } elseif ($isWow) {
      $hostBitness = '32-bit'
      Write-Host ('[OK]   32-bit host: {0} (pid {1}, hwnd 0x{2:X})' -f $HostApp, $proc.Id, [int64]$hostWnd)
    } else {
      $hostBitness = '64-bit'
      Write-Host ('[WARN] 64-bit host: {0} (pid {1}, hwnd 0x{2:X})' -f $HostApp, $proc.Id, [int64]$hostWnd)
      Write-Host '[WARN] The defect is not 32-bit-only, but no run of THIS script against a 64-bit'
      Write-Host '[WARN] host has been recorded yet. Treat the result as a new measurement, not a'
      Write-Host '[WARN] routine one, and save the output to evidence/. host32/ is the known-good host.'
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
    # Resolve the thread that owns the FOCUS, not the top-level window. A multi-threaded
    # frame keeps its edit control on a different thread and the input profile is per
    # thread, so the frame's thread can report a different keyboard from the one the
    # keystrokes will actually meet. stuck-mod-test.ps1 records the same correction under
    # "THE HKL ORACLE, CORRECTED".
    $focusWnd = $hostWnd
    $gti = New-Object GH8064.Win32+GUITHREADINFO
    $gti.cbSize = [uint32][System.Runtime.InteropServices.Marshal]::SizeOf($gti)
    if ([GH8064.Win32]::GetGUIThreadInfo(0, [ref]$gti)) {
      if ($gti.hwndFocus -ne [IntPtr]::Zero) { $focusWnd = $gti.hwndFocus }
      elseif ($gti.hwndActive -ne [IntPtr]::Zero) { $focusWnd = $gti.hwndActive }
    }

    # GetWindowThreadProcessId RETURNS the thread id and writes the PROCESS id to its out
    # parameter. Until 2026-08-31 this discarded the return value and read the out parameter
    # as the thread id, so GetKeyboardLayout was handed a pid, answered 0 for a thread that
    # does not exist, and every run of this script -- against any host, with any keyboard
    # selected -- aborted with "plain keyboard layout selected". The giveaway in a saved run
    # is "host thread N" printing the same N as the pid on the line above it. See
    # evidence/charmap32-stock-19.0.276.txt for the run that exposed it.
    $hostPid = 0
    $tid = [GH8064.Win32]::GetWindowThreadProcessId($focusWnd, [ref]$hostPid)
    # GetGUIThreadInfo(0) reports whatever is in the foreground, which is the host only because
    # the check above just confirmed it. If it somehow is not, the HKL below would describe some
    # other application, so fall back to the host's own window rather than measure a stranger.
    if ($hostPid -ne $proc.Id) {
      Write-Host ('[WARN] focus window 0x{0:X} belongs to pid {1}, not the host ({2}); reading the keyboard from the host window instead' -f `
        [int64]$focusWnd, $hostPid, $proc.Id)
      $focusWnd = $hostWnd
      $tid = [GH8064.Win32]::GetWindowThreadProcessId($focusWnd, [ref]$hostPid)
    }
    $hkl = ([int64][GH8064.Win32]::GetKeyboardLayout([uint32]$tid)) -band 0xFFFFFFFFL
    $hklLang = $hkl -band 0xFFFF

    # The HKL cannot answer "is a Keyman keyboard selected", and the test that used to stand
    # here -- high word 0xF000 -- does not mean what its comment claimed. 0xF0xx marks a
    # SUBSTITUTED layout (US-Dvorak and friends), not a text service. A Keyman TIP presents
    # as its base layout under a transient langid, 0x04092000 on this machine, whose high
    # word is 0x0409; so the check was false for every Keyman keyboard that has ever existed
    # and could only ever abort. A plain US layout preloaded under that same transient langid
    # presents identically (Substitutes: 00002000 -> 00000409), which is why the langid alone
    # cannot stand in for it either -- see the Resolve-Arm note in stuck-mod-test.ps1.
    #
    # TSF knows. GetActiveLanguageProfile against the Keyman Engine Text Service returns
    # GUID_NULL when that service is not the active profile, and the profile's own GUID when
    # it is. It answers for THIS thread, so the host's focused thread is corroborated by
    # comparing langids.
    $km = $null
    try { $km = [GH8064.Tsf]::ActiveProfileOf($KMTIP_CLSID) }
    catch {
      Write-Host ('[WARN] could not ask TSF which profile is active ({0}).' -f $_.Exception.Message)
      Write-Host '[WARN] The Keyman-selected precondition is unverified; the text-transform check at the'
      Write-Host '[WARN] end is now the only thing standing between a real PASS and a meaningless one.'
    }
    Write-Host ('       host thread {0} (pid {1}), HKL 0x{2:X8}, langid 0x{3:X4}' -f $tid, $hostPid, $hkl, $hklLang)

    if ($null -ne $km) {
      if (-not $km.Active) {
        $abort += ('No Keyman keyboard is the active input profile: TSF reports the Keyman Engine Text Service inactive (hr 0x{0:X8}). No rule will fire, no batch will be assembled, and a clean modifier state would prove nothing. Select a Keyman keyboard -- Win+Space, or the language button in the taskbar -- and re-run.' -f $km.Hr)
      } else {
        $name = Get-ProfileName -Clsid $KMTIP_CLSID -LangId $km.LangId -Profile $km.Profile
        Write-Host ('[OK]   Keyman TIP active: {0} (profile {{{1}}}, langid 0x{2:X4})' -f `
          $(if ($name) { $name } else { '<unnamed profile>' }), $km.Profile.ToString().ToUpper(), $km.LangId)
        if ($hklLang -ne [int]$km.LangId) {
          $abort += ('A Keyman keyboard is active for this script (langid 0x{0:X4}) but the host''s focused thread is on langid 0x{1:X4}, so the keystrokes would meet a different keyboard from the one measured. Windows is set to a per-window input method; select the Keyman keyboard in the HOST window and re-run.' -f $km.LangId, $hklLang)
        }
      }
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
# Recorded on the result line so a saved run states which bitness it measured. See the host block
# above: bitness is reported, not required.
Write-Host ('host {0}   {1}' -f $hostBitness, $HostApp)

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
