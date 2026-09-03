<#
  ============================================================================
  stuck-mod-test.ps1 - GH-8064 REGRESSION GATE
  ============================================================================

  Adapted from a standalone research rig that remains the source of the
  experiment below. This repo copy adds one thing: a -Gate mode. The original rig
  is a DEMONSTRATION - it succeeds when the stuck-modifier wedge reproduces and is
  shown to be Keyman-specific. A regression gate needs the opposite polarity, so
  -Gate inverts it.

  WHAT THIS PROVES
  ----------------
  A single stimulus - a 5-second Keyman freeze (WM_KEYMAN_CONTROL cmd 20)
  crossed with a modifier release - is applied identically on three keyboards,
  and ONLY the active keyboard varies. If the Keyman arm wedges while the
  Microsoft arms do not, the defect is Keyman's: not the Cameroon layout, not
  Windows dropping a KEYUP, not this harness's own injected keystrokes. The full
  argument, the two oracles and the fingerprinting step are documented in the
  original header immediately below. Nothing there has been removed.

  EXPECTED RESULT
  ---------------
    VANILLA Keyman build (defect present)  ->  -Gate reports FAIL, exit 1
                                               (the Keyman arm wedges)
    Build carrying this branch's fixes     ->  -Gate reports PASS, exit 0
                                               (no arm wedges)

  A FAIL on a supposedly-fixed build is a regression. A PASS on a vanilla build
  means the trigger did not fire, and the run should be repeated with
  -LoadThreads 4..6 and a higher -Repeat. Note that the gate reports
  INCONCLUSIVE, never PASS, whenever a precondition was unmet - precisely so a
  fixed build cannot pass for the wrong reason.

  EXIT-CODE CONTRACT (-Gate only)
  -------------------------------
    0  PASS          every precondition confirmed AND no arm wedged on any trial
    1  FAIL          the Keyman arm wedged on one or more trials - GH-8064 is
                     present. The report names the trial, the oracle and the
                     observed codepoints.
    2  INCONCLUSIVE  a precondition was unmet, so the run proves nothing:
                       - the freeze was not confirmed on a trial
                       - the target text was not transformed (OTHER/NO-OUTPUT)
                       - an oracle was not marked valid for an arm by
                         fingerprinting
                       - a required arm (English or Keyman) is not installed
                       - the arm switch could not be confirmed
                       - a CONTROL arm wedged. That indicts this harness, not
                         Keyman, so it is never reported as FAIL.
    3  SETUP ERROR   no target window, no Document element, keyman.exe not
                     running, or -Gate combined with a mode it does not cover

  Without -Gate the script behaves exactly as the source rig always has,
  including its own verdict prose, and sets no overall exit code. -Gate is
  purely additive.

  GATE MODE AND THE CANDIDATE SET
  -------------------------------
  "The freeze was not confirmed" is a precondition, and only candidate I
  confirms the stall is live before releasing the modifier. Candidate A posts no
  freeze at all and so needs no confirmation; B/C/E/F/G/H guess with a fixed
  delay. So in -Gate mode, when -Only is not given, the candidate set defaults to
  A,I - the internal control and the deterministic trigger. Passing -Only
  explicitly still runs whatever you ask for, but trials from a candidate that
  posts an UNCONFIRMED freeze score INCONCLUSIVE rather than PASS.

  An arm whose fingerprint arrives WEDGED has no valid oracle, so every trial on
  it scores INCONCLUSIVE even if the baseline recovery afterwards succeeds. That
  is deliberate: nothing measured on such an arm can be attributed to this run.

  -Gate does not cover -Sweep, -SwitchStress, -ChargeTest or -FingerprintOnly.
  Combining them exits 3 rather than inventing a verdict those modes cannot
  support.

  PREREQUISITES
  -------------
    * The Keyman keyboard sil_cameroon_qwerty (Cameroon QWERTY, aal-Latn-CM)
      installed and enabled as an input method.               REQUIRED
    * Any Microsoft English QWERTY input method - US, UK, Australian, Canadian,
      NZ, Irish and the rest all serve equally. A substituted layout (Dvorak,
      US-International) is REJECTED, not measured.            REQUIRED
    * The Microsoft MSKLC Cameroon layout (a0000436 / CAMQ2017.dll, under af).
                                                              OPTIONAL
      Absent, the run drops to two arms and states what the missing control
      would have ruled out.
    * An INTERACTIVE DESKTOP session. This is not a CI test: it drives
      Win+Space, reads the focus thread's HKL, and needs a real input queue.
    * A RUNNING keyman.exe. Without it the freeze stimulus is a no-op and every
      trial is silently a no-freeze control, so -Gate treats it as a setup error.
    * A Notepad window, open and empty. The rig clears the field between probes.

  DO NOT TOUCH THE MACHINE WHILE THIS RUNS. It injects real keystrokes into the
  focused window and switches the active keyboard. Typing, clicking or moving
  focus during a run corrupts the trial, and the injected keystrokes will land in
  whatever you focused.

  NEVER USE Write-Host IN THIS SCRIPT
  -----------------------------------
  Measured on a congested console host (the full note sits by the Say function
  below): Write-Host cost 4301 ms per line, against 0.4 ms for
  [Console]::Out.WriteLine and 1.8 ms for Add-Content. Say is called between a
  candidate's trigger action and the probe that reads the result, so seconds of
  dead time there let the 5s freeze expire before the probe runs and silently
  turn trials into no-freeze controls. That is a CORRECTNESS problem for a timing
  experiment, not a speed problem. All output goes through Say or SayAlways,
  which use [Console]::Out.WriteLine plus Add-Content. Do not add Write-Host, and
  do not add Write-Output in a hot path.

  -Quiet exists for anyone still worried about console cost. It silences Say's
  console echo entirely - the log file still receives every line - and prints
  only the final gate block. It has no effect outside -Gate.

  GATE USAGE
    .\stuck-mod-test.ps1 -Gate                       # A,I on every available arm
    .\stuck-mod-test.ps1 -Gate -Quiet                # final block only
    .\stuck-mod-test.ps1 -Gate -Repeat 5 -LoadThreads 4
    .\stuck-mod-test.ps1 -Gate -Arms English,Keyman  # no MSKLC installed

  Everything below this line is the original rig, unchanged in substance.
  ============================================================================

  stuck-mod-test.ps1 (source rig: kmproof.ps1) - three-arm controlled test: is the stuck-modifier wedge specific
                to KEYMAN, or is it a property of the Cameroon layout / of
                Windows / of this test harness?

  WHY THIS EXISTS
  ---------------
  A single-keyboard rig can answer "WHAT transitions Keyman from clean to
  wedged", but it cannot, on its own, support the claim in TRIGGER.md that the
  bug is Keyman's. A single-arm result is consistent with at least four other
  explanations:

    (a) the Cameroon LAYOUT is at fault, whoever implements it
    (b) WINDOWS drops the modifier KEYUP and every IME would suffer
    (c) this HARNESS manufactures the phantom Shift with its own SendInput
    (d) something else on this machine is eating keystrokes

  This script rules (a)-(d) out by holding the stimulus constant and varying
  ONLY the active keyboard:

    English  ANY non-Dvorak English QWERTY       - Microsoft, no special chars.
             US 0x0409, UK 0x0809, Australian      REQUIRED.
             0x0C09, Canadian, NZ, Irish ...
    MSKLC    af / a0000436 / CAMQ2017.dll        - Microsoft clone of the
                                                   Cameroon layout. OPTIONAL.
    Keyman   aal-Latn-CM / TIP 0x2000 /
             sil_cameroon_qwerty                 - Keyman. REQUIRED.

  WHY THE ENGLISH ARM IS NOT US-SPECIFIC. Its job is to be a keyboard Keyman is
  not driving, so a wedge appearing there would indict this harness rather than
  Keyman. Which English locale supplies that is irrelevant - they all type 'abc'
  as 'abc', which is the only thing the cross-arm oracle needs. A reviewer will
  have one already. What IS rejected is a substituted layout (Dvorak,
  US-International): those report a 0xF0xx high word and do not type 'abc' as
  'abc', so the Ascii oracle would silently lie. See Resolve-Arm.

  WHY MSKLC IS OPTIONAL. The load-bearing contrast is KEYMAN vs MICROSOFT, not
  Cameroon vs Cameroon - and the English arm already supplies a Microsoft
  keyboard. MSKLC adds a SAME-LAYOUT control, which is what separates "Keyman
  the engine" from "the Cameroon layout data". Worth having, not worth blocking
  a run over, and not something a reviewer can be expected to have installed.
  When it is absent the script drops the arm, says so, and the verdict states
  exactly what the missing control would have ruled out.

  The freeze stimulus (WM_KEYMAN_CONTROL cmd 20 -> keyman.exe) is posted on
  EVERY arm, including the Microsoft ones. keyman.exe is running throughout.
  That is deliberate and it is the whole point: the stall, the key sequence, the
  timings and the target window are identical in every arm. The only variable is
  which keyboard owns the keystrokes. So:

    Keyman wedges + English does not  =>  kills (c) and (d): the harness's own
                                          keystrokes do not produce a phantom
                                          Shift by themselves, and Windows does
                                          not drop the KEYUP on its own.
    Keyman wedges + MSKLC does not    =>  kills (a) and (b): same layout, same
                                          OS, different implementation,
                                          different outcome. Needs the optional
                                          MSKLC arm.

  Expected mechanism, for reference: Keyman's low-level hook only swallows and
  re-injects keystrokes when a Keyman keyboard is active
  (k32_lowlevelkeyboardhook.cpp:229-240, !isKeymanKeyboardActive -> pass
  through). On the Microsoft arms Keyman never touches the keys, so it has no
  cached modifier state to get wrong. The null result on those arms is PREDICTED
  by the mechanism, not merely observed.

  TWO ORACLES
  -----------
  Deadkey  ';e' then RAlt+N.  CLEAN = U+0259 U+014B ("schwa eng")
                              WEDGED = U+0259 U+014A ("schwa CAPITAL eng")
           Sharp, and it also proves the layout is really doing its job. Valid
           on the Cameroon arms ONLY - no English layout can produce U+0259 at
           all, which is exactly why a second oracle is needed.

  Ascii    'a' 'b' 'c', no Shift sent.  CLEAN = 'abc'   WEDGED = 'ABC'
           Layout-agnostic: those three keys are unshifted on every arm, so a
           phantom Shift shows up as 'ABC' whichever keyboard is active. This is
           the oracle the cross-arm comparison actually rests on, because it is
           the SAME measurement in every arm. It is also why a Dvorak-style
           layout is refused rather than measured.

  Both oracles run on every arm. Which ones are trustworthy per arm is not
  assumed - it is established by a fingerprint step (below) and recorded.

  FINGERPRINTING
  --------------
  Before any trial, each arm types both probes once and the exact codepoints are
  logged. An oracle is marked VALID for that arm only if its clean form actually
  appeared. This is what makes the US arm honest (its deadkey oracle is expected
  to be marked invalid, and the log will show precisely what US produced
  instead) and it is also a guard against a mis-switch: langid 0x2000 is shared
  by the Keyman Cameroon and Keyman Yoruba profiles in the registry, so the
  Keyman arm is only accepted if the deadkey fingerprint really yields
  U+0259 U+014B.

  ARM ORDER
  ---------
  Default order is English, MSKLC, Keyman - controls first, treatment last. The
  Keyman wedge is PERSISTENT (failure counts are strictly bimodal), so measuring
  the controls before it removes any chance of a leftover wedge colouring them. The script also records whether a wedge
  survived an arm switch.

  THE HKL ORACLE, CORRECTED
  -------------------------
  Earlier rigs in this project all recorded that the HKL is not trustworthy.
  That is an artefact of WHICH THREAD was asked. Windows 11 Notepad is a
  multi-threaded WinUI app: the top-level 'Notepad' frame window sits on a
  thread pinned at 0x0409 forever, while the focused 'RichEditD2DPT' edit
  control lives on a different thread that does track the input locale.
  Resolving the thread from MainWindowHandle reads the frame thread and always
  says 0x0409 - which is what produced the old "HKL said 0x0409 while ';e'
  correctly produced U+0259" note.

  Read from GetGUIThreadInfo(0).hwndFocus instead and the HKL is reliable, and
  positively discriminates all three arms. Verified by a same-thread A/B on
  2026-08-23, notepad pid 5500 tid 3196:

      MSKLC active  -> HKL 0xF0C00436  langid 0x0436
      Keyman active -> HKL 0x04092000  langid 0x2000

  Every function here that needs to know the active keyboard uses the focus
  thread. Get-FocusKeyboard is the only place the HKL is read.

  The full HKL matters, not just the langid: en-US carries two input methods on
  this machine (US 00000409 and Dvorak 00010409). Dvorak would silently break
  the Ascii oracle, since 'abc' is not 'abc' on a Dvorak layout. The English arm
  therefore requires an unsubstituted layout - high word not 0xF0xx, and its own
  primary language English - so an accidental landing on Dvorak is rejected
  rather than measured.

  SAFETY
  ------
  ClearField uses Ctrl+A then Delete. That is safe in Notepad and is NEVER safe
  against anything holding data you care about. Notepad is all the repro needs,
  so this script refuses to run against any other process unless
  -IKnowClearFieldIsDestructive is passed.

  Load emulation is capped at 6 runspaces: 32 exhausted memory and crashed the
  host PowerShell during an earlier session.

  This script does not restart Keyman. If an arm cannot reach a clean baseline
  the arm is abandoned and reported, because the documented recovery is a Keyman
  restart and doing that unattended would destroy the state worth looking at.

  USAGE (the original rig; see GATE USAGE above for -Gate)
    .\stuck-mod-test.ps1                                  # every available arm, 3 passes
    .\stuck-mod-test.ps1 -FingerprintOnly                 # just show me the layouts
    .\stuck-mod-test.ps1 -Only I -Repeat 5                # the deterministic trigger
    .\stuck-mod-test.ps1 -SwitchMode Manual               # I will switch by hand
    .\stuck-mod-test.ps1 -Arms Keyman,MSKLC -LoadThreads 4
    .\stuck-mod-test.ps1 -Arms English,Keyman             # two-arm run, no MSKLC needed
#>
[CmdletBinding()]
param(
  [string]$TargetProcess = 'notepad',

  # Arm order is the run order. Controls before treatment by default.
  [string[]]$Arms = @('English','MSKLC','Keyman'),

  # Candidate trigger ids to run, e.g. -Only A,I . Empty = all.
  [string[]]$Only = @(),

  # Passes through the candidate list, per arm.
  [int]$Repeat = 3,

  # Auto   = drive Win+Space until the wanted arm is confirmed on the focus thread
  # Manual = pause and wait for the operator to switch by hand
  [ValidateSet('Auto','Manual')][string]$SwitchMode = 'Auto',

  [int]$SwitchTries = 12,
  [int]$LoadThreads = 0,

  # Switch-stress mode: N cycles of Keyman -> MSKLC -> Keyman with NO freeze
  # ever posted, probing on Keyman each time. Tests whether the arm switch is
  # itself a trigger, independently of the freeze. 0 = off.
  [int]$SwitchStress = 0,

  # Charge test: N repetitions of {run candidate I on MSKLC $ChargeTrials times,
  # confirming MSKLC output stays perfect, then switch to Keyman ONCE and probe}.
  # Tests whether the freeze+release corrupts Keyman's state while a NON-Keyman
  # keyboard is active. Paired control is -SwitchStress (same switches, no
  # freeze), which came back 10/10 clean. 0 = off.
  [int]$ChargeTest = 0,
  [int]$ChargeTrials = 5,

  # Sweep mode: the shortest end-to-end demonstration, as an A/B/A.
  #   phase TRIGGER - walk English -> MSKLC -> Keyman applying the trigger on each
  #   phase WEDGED  - walk the same three again, applying NOTHING, and read what
  #                   each keyboard emits while Keyman is wedged
  #   phase CLEARED - clear the wedge, then walk the three again
  # Phase WEDGED is the sharpest statement of "this is Keyman only": the
  # Microsoft keyboards still type perfectly on the same machine, in the same
  # session, at the same moment that Keyman is producing garbage.
  [switch]$Sweep,
  [int]$SweepTrials = 1,

  [switch]$FingerprintOnly,
  [switch]$IKnowClearFieldIsDestructive,

  # ---- ADDED FOR THE REPO VERSION ----------------------------------------
  # Regression-gate mode. Inverts the polarity of the rig's verdict and sets a
  # process exit code: 0 PASS, 1 FAIL, 2 INCONCLUSIVE, 3 SETUP ERROR. See the
  # header. Without it nothing below changes and no exit code is set.
  [switch]$Gate,

  # Silences Say's console echo (the log file still gets every line) so only the
  # final gate block reaches the console. No effect without -Gate.
  [switch]$Quiet,

  [string]$LogDir = "$env:TEMP\stuck-mod-test"
)
$ErrorActionPreference = 'Stop'

# `powershell -File script.ps1 -Only A,B` passes "A,B" as ONE string, not an
# array, so a comma list silently matches nothing and every candidate is
# skipped. Same trap for -Arms. Split them back out. (Inherited from
# an earlier harness, where this bug cost a whole run.)
function Split-CommaArg([string[]]$v) {
  if ($v.Count -eq 1 -and $v[0] -match ',') { $v = @($v[0] -split '\s*,\s*') }
  return @($v | Where-Object { $_ } | ForEach-Object { $_.Trim() })
}
$Only = @((Split-CommaArg $Only) | ForEach-Object { $_.ToUpper() })
$Arms = @(Split-CommaArg $Arms)

# 'US' is accepted as an alias for the English control arm, which is no longer
# US-specific. Any non-Dvorak English QWERTY serves.
$Arms = @($Arms | ForEach-Object { if ($_ -eq 'US') { 'English' } else { $_ } })
foreach ($a in $Arms) {
  if ($a -notin @('English','MSKLC','Keyman')) { throw "unknown arm '$a' (expected English (alias US), MSKLC or Keyman)" }
}
if ($LoadThreads -gt 6) { $LoadThreads = 6 }
if ($TargetProcess -ne 'notepad' -and -not $IKnowClearFieldIsDestructive) {
  throw "ClearField does Ctrl+A then Delete. That is safe in Notepad and destructive anywhere that holds data you care about. Pass -IKnowClearFieldIsDestructive to override."
}
if (-not (Test-Path $LogDir)) { New-Item -ItemType Directory -Path $LogDir | Out-Null }

Add-Type -AssemblyName UIAutomationClient, UIAutomationTypes

Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
using System.Text;

[StructLayout(LayoutKind.Sequential)]
public struct KpRect { public int Left, Top, Right, Bottom; }

[StructLayout(LayoutKind.Sequential)]
public struct KpGuiThreadInfo {
  public int cbSize;
  public int flags;
  public IntPtr hwndActive, hwndFocus, hwndCapture, hwndMenuOwner, hwndMoveSize, hwndCaret;
  public KpRect rcCaret;
}

public static class Kp {
  [DllImport("user32.dll", CharSet=CharSet.Auto)] public static extern uint RegisterWindowMessage(string s);
  [DllImport("user32.dll")] public static extern bool PostMessage(IntPtr h, uint m, IntPtr w, IntPtr l);
  [DllImport("user32.dll", SetLastError=true)]
  public static extern IntPtr SendMessageTimeout(IntPtr h, uint m, IntPtr w, IntPtr l, uint flags, uint timeout, out UIntPtr res);
  [DllImport("user32.dll")] public static extern short GetAsyncKeyState(int vk);
  [DllImport("user32.dll")] public static extern void keybd_event(byte vk, byte sc, uint f, UIntPtr e);
  [DllImport("user32.dll")] public static extern uint MapVirtualKey(uint c, uint t);
  [DllImport("user32.dll")] public static extern bool SetForegroundWindow(IntPtr h);
  [DllImport("user32.dll")] public static extern IntPtr GetForegroundWindow();
  [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr h, out uint p);
  [DllImport("user32.dll")] public static extern IntPtr GetKeyboardLayout(uint tid);
  [DllImport("user32.dll")] public static extern int GetKeyboardLayoutList(int n, [Out] IntPtr[] list);
  [DllImport("user32.dll")] public static extern bool GetGUIThreadInfo(uint idThread, ref KpGuiThreadInfo gti);
  [DllImport("user32.dll", CharSet=CharSet.Auto)] public static extern int GetClassName(IntPtr h, StringBuilder s, int n);
  public delegate bool EnumWindowsProc(IntPtr h, IntPtr l);
  [DllImport("user32.dll")] public static extern bool EnumWindows(EnumWindowsProc cb, IntPtr p);
}
'@

$UP = 2; $EXT = 1; $FREEZE_CMD = 20
$SCHWA = [string][char]0x0259   # U+0259 LATIN SMALL LETTER SCHWA
$ENG   = [string][char]0x014B   # U+014B LATIN SMALL LETTER ENG     - clean
$ENGUP = [string][char]0x014A   # U+014A LATIN CAPITAL LETTER ENG   - wedged
$CLEAN_DEADKEY = $SCHWA + $ENG
# TWO wedge depths, both real, observed 2026-08-23:
#   partial - only the eng is shifted: schwa + CAPITAL eng
#   full    - Shift is applied to EVERYTHING, so ';' -> ':' and 'e' -> 'E' too.
#             TRIGGER.md already described this as "in the fuller form, :E<ENG>".
# The full form scored OTHER until it was added here, which made a correctly
# reproduced wedge look like an unreadable probe.
$WEDGE_DEADKEY = $SCHWA + $ENGUP                                  # partial
$WEDGE_FULL    = [string][char]0x003A + [string][char]0x0045 + $ENGUP   # ':' 'E' ENG

$stamp   = Get-Date -Format 'yyyyMMdd-HHmmss'
$log     = Join-Path $LogDir "stuck-mod-test-$stamp.txt"
$csvPath = Join-Path $LogDir "stuck-mod-test-$stamp.csv"
$jsonPath= Join-Path $LogDir "stuck-mod-test-$stamp.json"

# DO NOT use Write-Host here. Measured on this machine 2026-08-23, with 15
# conhost processes alive after a few background runs:
#
#     Write-Host              4301 ms per line
#     [Console]::Out.WriteLine   0.4 ms per line
#     Add-Content                1.8 ms per line
#
# A 10,000x difference, and it is NOT the file I/O - Add-Content is fine. This
# is not merely a speed problem, it is a CORRECTNESS problem for a timing
# experiment: Say is called between a candidate's trigger action and the probe
# that reads the result, and candidate I calls it from INSIDE the action. Four
# seconds of unplanned dead time at those points lets a 5s freeze expire before
# the probe runs, and gives Keyman time to recover, so trials silently
# degenerate into no-freeze controls.
#
# The earlier rigs in this project all used Write-Host in their own Say
# functions and were all exposed to this. Earlier runs in this session logged
# sub-millisecond, so the stall appears only once the console host is congested
# - which means past results may have been distorted without anything looking
# wrong in the logs. Worth re-checking any timing-sensitive conclusion drawn
# from a long session.
#
# ADDED FOR THE REPO VERSION: -Quiet suppresses only the CONSOLE echo. Every
# line still reaches the log file, so a quiet run is fully auditable afterwards.
# The primitives are unchanged, and they must stay unchanged.
$script:SayQuiet = ($Gate -and $Quiet)
function Say([string]$t) {
  $l = '{0} {1}' -f (Get-Date -Format 'HH:mm:ss.fff'), $t
  if (-not $script:SayQuiet) { [Console]::Out.WriteLine($l) }
  Add-Content -Path $log -Value $l -Encoding utf8
}

# The final gate block must reach the console even under -Quiet. Same two
# primitives as Say. NEVER Write-Host, for the reason measured above.
function SayAlways([string]$t) {
  $l = '{0} {1}' -f (Get-Date -Format 'HH:mm:ss.fff'), $t
  [Console]::Out.WriteLine($l)
  Add-Content -Path $log -Value $l -Encoding utf8
}

# ============================ REGRESSION GATE ==============================
# ADDED FOR THE REPO VERSION. None of this runs unless -Gate is passed, and it
# changes no timing: every gate line is emitted either BEFORE a candidate's
# trigger action or AFTER the probe that reads the result, never between them.
#
# POLARITY. The rig below succeeds when the wedge REPRODUCES - it is a
# demonstration of the defect. The gate succeeds when the wedge does NOT
# reproduce. It therefore cannot read the rig's verdict; it scores every trial
# itself, and it refuses to say PASS whenever a precondition was unmet.
$script:GatePass         = 0
$script:GateFail         = 0
$script:GateInc          = 0
$script:GateFailDetail   = @()
$script:GateIncReason    = @()
$script:GateKeymanCalls  = 0     # decisive (PASS or FAIL) trials on the Keyman arm
$script:LastFreezeConfirmed = $null
$script:GateExit         = $null

# Setup bails exit 1 in the original rig. Under -Gate they are SETUP ERROR = 3,
# which is distinct from FAIL so a broken bench is never mistaken for a defect.
$script:SETUP_EXIT = 1
if ($Gate) { $script:SETUP_EXIT = 3 }

# Which candidates post a freeze, and whether they CONFIRM it is live before
# releasing the modifier. Only 'confirmed' and 'none' can support a PASS: a
# candidate that guesses with a fixed delay may have released BEFORE the stall
# began, which degenerates the trial into a no-freeze control.
$GATE_FREEZE = @{
  A = 'none';         D = 'none'
  I = 'confirmed'
  B = 'unconfirmed';  C = 'unconfirmed';  E = 'unconfirmed'
  F = 'unconfirmed';  G = 'unconfirmed';  H = 'unconfirmed'
}

function GateSay([string]$t) { if ($Gate) { Say $t } }

function Add-GateInconclusive([string]$reason) {
  if (-not $Gate) { return }
  if ($script:GateIncReason -notcontains $reason) { $script:GateIncReason += $reason }
}

function Get-GateExpectation([string]$oracle) {
  if ($oracle -eq 'Ascii') { return "Ascii 'abc' (U+0061 U+0062 U+0063)" }
  return 'Deadkey U+0259 U+014B (schwa + eng)'
}

function Get-GateWedgeForm([string]$oracle) {
  if ($oracle -eq 'Ascii') { return "'ABC' (U+0041 U+0042 U+0043)" }
  return 'U+0259 U+014A (schwa + CAPITAL eng), or U+003A U+0045 U+014A in the full form'
}

# Scores ONE trial on ONE oracle. The ordering is deliberate:
#
#   A WEDGED reading on the KEYMAN arm is scored FAIL first, before any
#   precondition is considered. A positive observation of the wedge cannot be
#   explained away by a shaky precondition - the characters are on the screen.
#
#   A WEDGED reading on a CONTROL arm is never FAIL. English and MSKLC are
#   keyboards Keyman is not driving, so a wedge there indicts this harness (or
#   Windows) rather than Keyman, and the honest report is INCONCLUSIVE.
#
#   Everything else must clear EVERY precondition before it is allowed to count
#   as PASS. This is the half that matters for a regression gate: a fixed build
#   must not be able to pass because the freeze never fired, because the oracle
#   was meaningless on that arm, or because the keyboard drifted mid-pass.
function Register-GateTrial($arm, $candidate, $pass, $oracle, $probe, $armOk, $fp) {
  if (-not $Gate) { return }
  $expect  = Get-GateExpectation $oracle
  $freeze  = $GATE_FREEZE[$candidate]
  if (-not $freeze) { $freeze = 'unconfirmed' }
  $marker  = '[INCONCLUSIVE]'
  $why     = ''

  if ($probe.State -eq 'WEDGED' -and $arm -eq 'Keyman') {
    $marker = '[FAIL]'
    $why    = 'the Keyman arm wedged - GH-8064 is present'
    $script:GateFail++
    $script:GateFailDetail += [pscustomobject]@{
      Arm=$arm; Candidate=$candidate; Pass=$pass; Oracle=$oracle
      Expected=$expect; Observed=$probe.Cp; Text=$probe.Text; Mods=$probe.Mods
      WedgeForm=(Get-GateWedgeForm $oracle)
    }
  }
  elseif ($probe.State -eq 'WEDGED') {
    $why = ("control arm {0} wedged under the same stimulus - that indicts this harness, not Keyman, so it is not a FAIL" -f $arm)
  }
  elseif (-not $armOk) {
    $why = 'the arm switch could not be confirmed - the active keyboard drifted mid-pass'
  }
  elseif (($oracle -eq 'Ascii'   -and -not $fp.AsciiValid) -or
          ($oracle -eq 'Deadkey' -and -not $fp.DeadkeyValid)) {
    $why = ("the {0} oracle was not marked valid for arm {1} by fingerprinting" -f $oracle, $arm)
  }
  elseif ($freeze -eq 'unconfirmed') {
    $why = ("candidate {0} posts a freeze but never confirms it is live, so the trial may have been a no-freeze control" -f $candidate)
  }
  elseif ($freeze -eq 'confirmed' -and $script:LastFreezeConfirmed -ne $true) {
    $why = ("the freeze was not confirmed on candidate {0} pass {1} - not a valid trial" -f $candidate, $pass)
  }
  elseif ($probe.State -eq 'CLEAN') {
    $marker = '[PASS]'
    $why    = 'clean - the arm did not wedge'
    $script:GatePass++
  }
  else {
    $why = ("the target text was not transformed: probe read {0} ({1})" -f $probe.State, $probe.Cp)
  }

  if ($marker -eq '[INCONCLUSIVE]') { $script:GateInc++; Add-GateInconclusive $why }
  if ($arm -eq 'Keyman' -and $marker -ne '[INCONCLUSIVE]') { $script:GateKeymanCalls++ }

  GateSay ('  [gate]   {0} observed {1} -> {2}; {3} {4}' -f $oracle, $probe.Cp, $probe.State, $marker, $why)
}

# ---- key injection ---------------------------------------------------------
function Kd([int]$v, [switch]$E) { $f = 0; if ($E) { $f = $EXT }; [Kp]::keybd_event([byte]$v, [byte][Kp]::MapVirtualKey($v,0), $f, [UIntPtr]::Zero) }
function Ku([int]$v, [switch]$E) { $f = $UP; if ($E) { $f = $f -bor $EXT }; [Kp]::keybd_event([byte]$v, [byte][Kp]::MapVirtualKey($v,0), $f, [UIntPtr]::Zero) }
function Tp([int]$v, [int]$g = 70, [switch]$E) { Kd $v -E:$E; Start-Sleep -Milliseconds 40; Ku $v -E:$E; Start-Sleep -Milliseconds $g }

# RShift carries E=$false as a matter of form: Right Shift is scan 0x36 and is
# not extended, while only RCtrl (E0 1D) and RAlt (E0 38) are. The flag makes no
# difference to behaviour here either way.
#
# Measured at the wire with kmaltgr.ps1, 2026-08-25. Injecting VK_RSHIFT with the
# extended flag and without it produces byte-identical events at a
# WH_KEYBOARD_LL hook - both `RSHIFT scan=0x36 EXT|INJ`. Windows resolves the
# side from the side-specific VIRTUAL KEY (0xA1), not from the scan code or the
# extended flag, and it reports LLKHF_EXTENDED for Right Shift either way. So
# ClearMods and TapAllMods do release and tap Right Shift correctly, and the
# six-modifier KEYUP sweep is six keys.
#
# WHERE THE EXTENDED BIT DOES DECIDE THE SIDE: when the caller passes the
# GENERIC vk. Keyman's do_keybd_event (keybd_shift.cpp:63-88) collapses
# VK_LSHIFT/VK_RSHIFT to VK_SHIFT, VK_L/RCONTROL to VK_CONTROL and
# VK_L/RMENU to VK_MENU, at which point the scan code and the extended bit are
# the only discriminators left. That is exactly why it sets
# scan = SCANCODE_RSHIFT explicitly for Right Shift - and why it is worth asking
# what its bare 0xFF scan code plus an extended bit resolves to for Ctrl and Alt
# (MODIFIERS.md s2b).
$MODS = @(
  @{V=0xA0;E=$false;L='LShift'}, @{V=0xA1;E=$false;L='RShift'}
  @{V=0xA2;E=$false;L='LCtrl'},  @{V=0xA3;E=$true; L='RCtrl'}
  @{V=0xA4;E=$false;L='LAlt'},   @{V=0xA5;E=$true; L='RAlt'}
)
function ModsHeld {
  $h = @()
  foreach ($m in $MODS) { if ((([Kp]::GetAsyncKeyState($m.V)) -band 0x8000) -ne 0) { $h += $m.L } }
  if ($h.Count -eq 0) { return 'none' }
  return ($h -join ',')
}
function ClearMods { foreach ($m in $MODS) { Ku $m.V -E:$m.E; Start-Sleep -Milliseconds 60 }; Start-Sleep -Milliseconds 250 }
function TapAllMods { foreach ($m in $MODS) { Kd $m.V -E:$m.E; Start-Sleep -Milliseconds 90; Ku $m.V -E:$m.E; Start-Sleep -Milliseconds 90 }; Start-Sleep -Milliseconds 300 }

# ---- the ONLY place the active keyboard is read ----------------------------
# Resolves the thread from GetGUIThreadInfo(0).hwndFocus, not from the
# top-level window. See "THE HKL ORACLE, CORRECTED" in the header.
function Get-FocusKeyboard {
  $g = New-Object KpGuiThreadInfo
  $g.cbSize = [System.Runtime.InteropServices.Marshal]::SizeOf($g)
  if (-not [Kp]::GetGUIThreadInfo(0, [ref]$g)) {
    return [pscustomobject]@{ Ok=$false; Hkl=0; LangId=0; HighWord=0; Tid=0; Class=''; Arm='<no-gui-info>' }
  }
  $h = $g.hwndFocus
  if ($h -eq [IntPtr]::Zero) { $h = $g.hwndActive }
  if ($h -eq [IntPtr]::Zero) {
    return [pscustomobject]@{ Ok=$false; Hkl=0; LangId=0; HighWord=0; Tid=0; Class=''; Arm='<no-focus>' }
  }
  $p = 0
  $tid = [Kp]::GetWindowThreadProcessId($h, [ref]$p)
  $hkl = [Kp]::GetKeyboardLayout($tid).ToInt64()
  $sb = New-Object System.Text.StringBuilder 256
  [void][Kp]::GetClassName($h, $sb, 256)
  $lang = $hkl -band 0xFFFF
  $high = ($hkl -shr 16) -band 0xFFFF
  return [pscustomobject]@{
    Ok       = $true
    Hkl      = $hkl
    LangId   = $lang
    HighWord = $high
    Tid      = $tid
    Pid      = $p
    Class    = $sb.ToString()
    Arm      = (Resolve-Arm $lang $high)
  }
}

# Arm identity from the full HKL. Deliberately strict: anything unrecognised
# comes back as a descriptive string rather than being coerced into an arm, so a
# mis-switch can never be silently measured as a result.
function Resolve-Arm([int64]$lang, [int64]$high) {
  # THE ENGLISH ARM IS ANY NON-DVORAK ENGLISH QWERTY, not specifically US.
  # The arm's whole job is to be a keyboard Keyman is not driving, so that a
  # wedge appearing there would indict this harness rather than Keyman. Which
  # English locale supplies it is irrelevant: US 0x0409, UK 0x0809, Australian
  # 0x0C09, Canadian 0x1009, NZ 0x1409, Irish 0x1809 and the rest all serve
  # equally, and all type 'abc' as 'abc'.
  #
  # What DOES matter is that the layout is not substituted. A layout id whose
  # high word is 0xF0xx is a SUBSTITUTION HANDLE, which is how Windows reports
  # the alternates: US-Dvorak (preload d0010409 -> substitute 00010409) comes
  # back as high word 0xF002, NOT the 0x0001 the layout id would suggest.
  # Observed 2026-08-23. 'abc' is not 'abc' on Dvorak, so the Ascii oracle would
  # silently lie; such a layout is REJECTED rather than measured. A plain layout
  # reports its own language in the high word (US 0x04090409, UK 0x08090809).
  $langPrimary = $lang -band 0x03FF
  $highPrimary = $high -band 0x03FF
  $highIsSubstituted = (($high -band 0xF000) -eq 0xF000)
  if ($langPrimary -eq 0x09 -and -not $highIsSubstituted -and $highPrimary -eq 0x09) { return 'English' }
  if ($langPrimary -eq 0x09) {
    # An English locale carrying an alternate layout. Named, not coerced.
    return ('en-substituted-layout-0x{0:X4}-REJECT' -f $high)
  }
  if ($lang -eq 0x0436)                       { return 'MSKLC' }
  # A Keyman TIP lives under a TRANSIENT langid, and which one it gets is assigned
  # by Windows when the profile is enabled -- it is NOT stable. Installing another
  # Keyman keyboard reshuffles them: this harness was written when Cameroon QWERTY
  # sat at 0x2000, and after a test keyboard was added it moved to 0x2C00 while
  # 0x2000 came to carry a plain US layout (2000:00000409). Matching 0x2000 alone
  # therefore selected the US keyboard, called it the treatment arm, and produced
  # three identical fingerprints.
  #
  # So the whole transient range is a CANDIDATE, and nothing here decides which one
  # is Cameroon. The HKL cannot: both a transient-lang US layout and a Keyman TIP
  # present as 0x0409xxxx, so the high word does not separate them. Only the deadkey
  # fingerprint can, and Switch-ToArm now runs it -- see Confirm-KeymanArm.
  if ($lang -in @(0x2000, 0x2400, 0x2800, 0x2C00)) { return 'Keyman' }
  if ($lang -eq 0x046A)                       { return 'Keyman-Yoruba' }
  if ($lang -eq 0x100C)                       { return 'fr-CH' }
  return ('unknown-0x{0:X4}' -f $lang)
}

function Format-Keyboard($k) {
  if (-not $k.Ok) { return $k.Arm }
  return ('{0} (HKL=0x{1:X8} langid=0x{2:X4} tid={3} cls={4})' -f $k.Arm, $k.Hkl, $k.LangId, $k.Tid, $k.Class)
}

$ARM_LABEL = @{
  English = 'any non-Dvorak English QWERTY - US, UK, Australian, Canadian, NZ, Irish ...'
  MSKLC   = 'Cameroon QWERTY 2017 - Microsoft MSKLC, a0000436 / CAMQ2017.dll, under af (OPTIONAL)'
  Keyman  = 'Cameroon QWERTY - Keyman sil_cameroon_qwerty, TIP {25C4EE49-...} under aal-Latn-CM'
}

# ---- which arms this machine can actually supply --------------------------
# Enumerates the installed HKLs rather than discovering an arm is missing by
# burning $SwitchTries Win+Space presses against a keyboard that is not there.
# MSKLC is optional by design: see the ARMS note in the header.
function Get-InstalledArms {
  $n = [Kp]::GetKeyboardLayoutList(0, $null)
  $buf = New-Object IntPtr[] $n
  [void][Kp]::GetKeyboardLayoutList($n, $buf)
  $found = @{}
  $detail = @()
  foreach ($h in $buf) {
    # 0xFFFFFFFF is an Int32 literal in PowerShell and parses as -1, which makes
    # the mask a no-op and prints sign-extended handles. 0xFFFFFFFFL is Int64.
    $v = $h.ToInt64() -band 0xFFFFFFFFL
    $arm = Resolve-Arm ($v -band 0xFFFF) (($v -shr 16) -band 0xFFFF)
    $detail += ('0x{0:X8} -> {1}' -f $v, $arm)
    if ($arm -in @('English','MSKLC','Keyman')) { $found[$arm] = $true }
  }
  return [pscustomobject]@{ Arms = @($found.Keys); Detail = $detail }
}

# ---- target window + text readback ----------------------------------------
$np = Get-Process -Name $TargetProcess -ErrorAction SilentlyContinue | Where-Object { $_.MainWindowHandle -ne 0 } | Select-Object -First 1
if (-not $np) { Say "[FAIL] no '$TargetProcess' window"; exit $script:SETUP_EXIT }
$target = $np.MainWindowHandle
[void][Kp]::SetForegroundWindow($target); Start-Sleep -Milliseconds 600

$root = [System.Windows.Automation.AutomationElement]::FromHandle($target)
$cond = New-Object System.Windows.Automation.PropertyCondition([System.Windows.Automation.AutomationElement]::ControlTypeProperty,[System.Windows.Automation.ControlType]::Document)
$docEl = $root.FindFirst([System.Windows.Automation.TreeScope]::Descendants,$cond)
if (-not $docEl) { Say '[FAIL] no Document element'; exit $script:SETUP_EXIT }
$vp = $docEl.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern)

function Get-DocText {
  try { $t = $vp.Current.Value } catch { $t = '' }
  if ($null -eq $t) { $t = '' }
  return $t
}
function Show-Cp([string]$t) {
  if (-not $t) { return '<empty>' }
  return (($t.ToCharArray() | ForEach-Object { 'U+{0:X4}' -f [int]$_ }) -join ' ')
}
function Assert-Foreground {
  if ([Kp]::GetForegroundWindow() -ne $target) {
    Tp 0x1B 80      # Escape - dismiss a Start menu or switcher we tripped
    [void][Kp]::SetForegroundWindow($target)
    Start-Sleep -Milliseconds 400
  }
  return ([Kp]::GetForegroundWindow() -eq $target)
}

# ---- keyman.exe control window + the freeze stimulus -----------------------

# Resolves keyman.exe's TApplication window into $script:km, which Freeze posts to.
# A function rather than inline because Reset-KeymanEngine restarts keyman.exe at the top
# of the run, which invalidates the handle -- posting the freeze to a dead HWND silently
# does nothing and every trial would degenerate into a no-freeze control scoring CLEAN.
function Resolve-KeymanControlWindow {
  $script:km = [IntPtr]::Zero
  $pid2 = (Get-Process keyman -ErrorAction SilentlyContinue | Select-Object -First 1).Id
  if ($pid2) {
    $cb = [Kp+EnumWindowsProc]{ param($h,$l)
      $p=0; [void][Kp]::GetWindowThreadProcessId($h,[ref]$p)
      if ($p -eq $pid2) {
        $sb=New-Object System.Text.StringBuilder 256; [void][Kp]::GetClassName($h,$sb,256)
        if ($sb.ToString() -eq 'TApplication') { $script:km=$h; return $false }
      }
      return $true }
    [void][Kp]::EnumWindows($cb,[IntPtr]::Zero)
  }
  return $script:km
}

<#
  Restarts keyman.exe so the modifier cache starts empty.

  WHY THIS EXISTS, and why it is NOT part of the stimulus. The cache lives in
  keyman.exe and the low level hook feeds it on EVERY key event, including while a
  Microsoft keyboard is active -- k32_lowlevelkeyboardhook.cpp posts
  WM_KEYMAN_MODIFIER_EVENT before its `!isKeymanKeyboardActive` pass-through, on
  purpose. Nothing reconciles that cache until a batch is assembled, which only a
  Keyman keyboard does. So the control arms' triggers charge the cache silently, and
  the treatment arm arrives ALREADY WEDGED -- measured 2026-08-31: entry probe CLEAN,
  one trigger each on English and MSKLC, then arm-confirm WEDGED before Keyman's own
  trigger ran. The run then proves nothing about whether Keyman's trigger causes the
  wedge, because there was nothing left to cause.

  Restarting between the controls and the treatment removes that carry-over: a fresh
  keyman.exe re-seeds the cache from live OS state via InitThread. It is logged loudly
  so no reader mistakes it for part of the experiment.

  The charge-carry itself is a real finding and is NOT hidden by this -- phase WEDGED
  still shows the Microsoft arms suffering the wedge with no trigger applied to them.
#>
function Reset-KeymanEngine {
  Say '  ---------------- RESET keyman.exe (not part of the stimulus) ----------------'
  Say '    The control arms charge the modifier cache even though their own output stays'
  Say '    clean. Without this reset the treatment arm arrives already wedged and its own'
  Say '    trigger proves nothing. Restarting re-seeds the cache from live OS state.'

  $proc = Get-Process keyman -ErrorAction SilentlyContinue | Select-Object -First 1
  $exe  = $null
  if ($proc) { try { $exe = $proc.Path } catch { } }
  if (-not $exe) { $exe = 'C:\Program Files (x86)\Common Files\Keyman\Keyman Engine\keyman.exe' }

  # NOT an early return: the manual path below does not need $exe at all, only the
  # automatic restart does. Bailing here would deny the operator the chance to do it.
  $canAutoStart = (Test-Path $exe)
  if (-not $canAutoStart) {
    Say ('    [INFO] keyman.exe not locatable at {0}; automatic restart unavailable.' -f $exe)
  }

  # keyman.exe runs at a higher integrity level than an ordinary shell -- same user, but
  # its ExecutablePath and process handle read back empty, so Stop-Process gets Access
  # Denied unless this script was started elevated. Measured on MLEELOQ, 2026-08-31.
  # Rather than warn and carry on with a pre-wedged treatment arm, ask the operator, the
  # same way phase 3 asks for a physical Left Shift double-tap.
  $oldPid = (Get-Process keyman -ErrorAction SilentlyContinue | Select-Object -First 1).Id
  $auto = $canAutoStart
  if ($auto) {
    try { Stop-Process -Name keyman -Force -ErrorAction Stop } catch { $auto = $false }
  }

  if ($auto) {
    Start-Sleep -Seconds 2
    Start-Process $exe | Out-Null
    Start-Sleep -Seconds 5
  } else {
    Say '    [INFO] cannot stop keyman.exe from an unelevated shell (Access Denied).'
    Say '    ACTION NEEDED: restart Keyman yourself now - exit it from the tray/Start menu'
    Say '                   and start it again. Waiting up to 120s for a new process...'
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    $newPid = $null
    while ($sw.Elapsed.TotalSeconds -lt 120) {
      Start-Sleep -Milliseconds 1500
      $cur = (Get-Process keyman -ErrorAction SilentlyContinue | Select-Object -First 1).Id
      if ($cur -and $cur -ne $oldPid) { $newPid = $cur; break }
    }
    if (-not $newPid) {
      Say '    [WARN] Keyman was not restarted within 120s - NOT reset. The treatment arm may'
      Say '           arrive already wedged, in which case its trigger proves nothing.'
      return $false
    }
    Say ('    [OK] new keyman.exe observed (pid {0} -> {1}) after {2:N0}s' -f $oldPid, $newPid, $sw.Elapsed.TotalSeconds)
    Start-Sleep -Seconds 3   # let it finish standing its hook and server up
  }

  if (-not (Get-Process keyman -ErrorAction SilentlyContinue)) {
    Say '    [WARN] keyman.exe did not come back up. Later trials cannot be trusted.'
    return $false
  }
  # MUST re-resolve: the old TApplication HWND died with the old process, and Freeze
  # posting into a dead handle fails silently.
  $h = Resolve-KeymanControlWindow
  if ($h -eq [IntPtr]::Zero) {
    Say '    [WARN] keyman.exe restarted but its control window was not found - the freeze'
    Say '           stimulus cannot be posted, so every later trial would be a no-freeze control.'
    return $false
  }
  Say ('    [OK] keyman.exe restarted, control window re-resolved to 0x{0:X}' -f $h.ToInt64())
  Say '  -----------------------------------------------------------------------------'
  return $true
}

[void](Resolve-KeymanControlWindow)
$msg = [Kp]::RegisterWindowMessage('WM_KEYMAN_CONTROL')

# ---- gate preflight -----------------------------------------------------
# ADDED FOR THE REPO VERSION.
if ($Gate) {
  if ($Sweep -or $SwitchStress -gt 0 -or $ChargeTest -gt 0 -or $FingerprintOnly) {
    Say '[SETUP ERROR] -Gate does not cover -Sweep, -SwitchStress, -ChargeTest or -FingerprintOnly.'
    Say '              Those modes answer different questions and have no pass/fail contract to invert.'
    exit 3
  }
  # Without keyman.exe the freeze stimulus is a no-op, so every trial would be a
  # no-freeze control and a fixed build and a broken build would look identical.
  # That is exactly the "passes for the wrong reason" this gate exists to stop,
  # so it is a setup error rather than the original rig's warning.
  if ($script:km -eq [IntPtr]::Zero) {
    Say '[SETUP ERROR] keyman.exe TApplication window not found. The freeze stimulus cannot be posted,'
    Say '              so no trial would be valid. Start Keyman and run this again.'
    exit 3
  }
  if ($Only.Count -eq 0) {
    # See "GATE MODE AND THE CANDIDATE SET" in the header: only A (no freeze at
    # all) and I (freeze confirmed live) can support a PASS.
    $Only = @('A','I')
    Say '[gate] candidate set defaults to A,I - A is the no-freeze internal control and I is the only'
    Say '       candidate that CONFIRMS the freeze is live before releasing the modifier. Pass -Only'
    Say '       explicitly to widen it; unconfirmed-freeze trials then score INCONCLUSIVE, not PASS.'
  }
}

# Posted on EVERY arm, including the Microsoft ones. Identical stimulus is the
# basis of the whole comparison - see the header.
function Freeze {
  if ($script:km -ne [IntPtr]::Zero) { [void][Kp]::PostMessage($script:km,$msg,[IntPtr]$FREEZE_CMD,[IntPtr]::Zero) }
}

# PostMessage is ASYNCHRONOUS: posting cmd 20 does not tell us when keyman.exe
# actually enters its Sleep(5000). With a fixed delay the modifier KEYUP can be
# released BEFORE the freeze begins, in which case the candidate degenerates
# into the no-freeze control and comes back clean. That is why candidate B is
# intermittent and candidate I is not.
function WaitForFreeze([int]$timeoutMs = 3000) {
  $sw = [System.Diagnostics.Stopwatch]::StartNew()
  while ($sw.ElapsedMilliseconds -lt $timeoutMs) {
    $r = [UIntPtr]::Zero
    $ok = [Kp]::SendMessageTimeout($script:km, 0, [IntPtr]::Zero, [IntPtr]::Zero, 2, 60, [ref]$r)
    if ($ok -eq [IntPtr]::Zero) { return $true }   # no reply = thread is blocked
    Start-Sleep -Milliseconds 20
  }
  return $false
}

# ---- probes ----------------------------------------------------------------
# Clears the field outright rather than counting backspaces: a dropped deadkey
# changes the character count, and miscounted backspaces then corrupt the NEXT
# probe.
# CRITICAL: clear PROGRAMMATICALLY, not with keystrokes.
#
# The original version sent Ctrl+A then Delete. That works fine on a clean
# machine and FAILS SILENTLY the moment the wedge fires: with a phantom LShift
# latched, Ctrl+A becomes Ctrl+Shift+A and Delete becomes Shift+Delete, so the
# field is never emptied. Every subsequent probe then reads the whole
# accumulated buffer and scores OTHER regardless of which keyboard is active.
#
# That artifact produced a bogus "[CLAIM FAILS] a Microsoft keyboard was also
# not-CLEAN" verdict in the 10:15 sweep - English and MSKLC were fine; the readback
# was broken. Any keystroke-based clear is unusable in exactly the state this
# script exists to measure.
#
# UIA SetValue touches no keys, so it cannot be perturbed by a stuck modifier
# and cannot perturb Keyman's cached state either. Keystrokes remain only as a
# fallback if the pattern refuses.
function ClearField {
  try {
    $vp.SetValue('')
    Start-Sleep -Milliseconds 120
    if ([string]::IsNullOrEmpty((Get-DocText))) { return }
  } catch { }
  # Fallback. Release modifiers first or this cannot work while wedged - but
  # note that releasing them may itself clear the wedge, so a run that lands
  # here is not a clean measurement and says so.
  Say '        [WARN] UIA SetValue clear failed; falling back to keystrokes (may disturb the wedge)'
  ClearMods
  Kd 0x11; Start-Sleep -Milliseconds 70
  Tp 0x41 40
  Ku 0x11; Start-Sleep -Milliseconds 120
  Tp 0x2E 40 -E                       # Delete is an EXTENDED key
  Start-Sleep -Milliseconds 200
}

function ProbeAsciiOnce {
  ClearField
  Tp 0x41 110; Tp 0x42 110; Tp 0x43 110          # 'a' 'b' 'c' - no Shift sent
  Start-Sleep -Milliseconds 450
  $t = Get-DocText
  $state = 'OTHER'
  # -ceq, not -eq: PowerShell's -eq is CASE-INSENSITIVE, so 'abc' -eq 'ABC' is
  # TRUE and the wedged result would compare equal to the clean one.
  if     ($t -ceq 'abc') { $state = 'CLEAN' }
  elseif ($t -ceq 'ABC') { $state = 'WEDGED' }
  elseif ([string]::IsNullOrEmpty($t)) { $state = 'NO-OUTPUT' }
  return [pscustomobject]@{ Oracle='Ascii'; State=$state; Text=$t; Cp=(Show-Cp $t); Mods=(ModsHeld) }
}

function ProbeDeadkeyOnce {
  ClearField
  Tp 0xBA 130; Tp 0x45 130                       # ';' then 'e'  -> U+0259
  Start-Sleep -Milliseconds 200
  Kd 0xA5 -E; Start-Sleep -Milliseconds 130      # RAlt DOWN (extended)
  Tp 0x4E 130                                    # 'N'          -> U+014B
  Ku 0xA5 -E; Start-Sleep -Milliseconds 600      # RAlt UP
  $t = Get-DocText
  $state = 'OTHER'
  # Same -ceq trap, and worse here: U+014A/U+014B are the upper/lowercase ENG
  # pair, so -eq reported every WEDGED state as CLEAN until it was caught.
  $variant = ''
  if     ($t -ceq $CLEAN_DEADKEY) { $state = 'CLEAN' }
  elseif ($t -ceq $WEDGE_DEADKEY) { $state = 'WEDGED'; $variant = 'partial' }
  elseif ($t -ceq $WEDGE_FULL)    { $state = 'WEDGED'; $variant = 'full' }
  elseif ([string]::IsNullOrEmpty($t)) { $state = 'NO-OUTPUT' }
  return [pscustomobject]@{ Oracle='Deadkey'; State=$state; Variant=$variant; Text=$t; Cp=(Show-Cp $t); Mods=(ModsHeld) }
}

function ProbeOnce([string]$oracle) {
  if ($oracle -eq 'Ascii') { return ProbeAsciiOnce }
  return ProbeDeadkeyOnce
}

# Both probes are themselves flaky (a dropped ';' yields 'e' instead of schwa),
# so read up to 3 times and take the first state seen twice. OTHER/NO-OUTPUT are
# treated as unreliable reads to be retried, NOT as evidence of a wedge - only
# CLEAN and WEDGED are trusted verdicts.
function Probe([string]$oracle) {
  $seen = @()
  for ($attempt = 1; $attempt -le 3; $attempt++) {
    $r = ProbeOnce $oracle
    $seen += $r
    $same = @($seen | Where-Object { $_.State -eq $r.State })
    if ($same.Count -ge 2 -and ($r.State -eq 'CLEAN' -or $r.State -eq 'WEDGED')) { return $r }
    Start-Sleep -Milliseconds 250
  }
  $decided = @($seen | Where-Object { $_.State -eq 'CLEAN' -or $_.State -eq 'WEDGED' })
  if ($decided.Count -gt 0) { return $decided[-1] }
  return $seen[-1]
}

# ---- arm switching --------------------------------------------------------
# Auto mode drives the real user path (Win+Space) rather than poking TSF, and
# verifies the landing with Get-FocusKeyboard after every press. TSF profile
# activation is per-thread inside the owning process and cannot be driven from
# here anyway, so Win+Space is not just the faithful route, it is the only one.
function Tap-WinSpace {
  Kd 0x5B -E; Start-Sleep -Milliseconds 140      # LWIN is an extended key
  Tp 0x20 140
  Ku 0x5B -E; Start-Sleep -Milliseconds 500
}

# Is the currently active keyboard the Keyman CAMEROON one, as opposed to some
# other Keyman TIP or a US layout sitting on a transient langid?
#
# The deadkey oracle is the only runtime discriminator there is: nothing else on
# the machine can emit U+0259. CLEAN and WEDGED both confirm the layout -- an arm
# that arrives wedged is still the right keyboard, which is the distinction
# Get-Fingerprint's own comment was written to preserve.
function Confirm-KeymanArm {
  $d = Probe 'Deadkey'
  # #8064: this probe answers "is the arm already wedged on entry?" and used to discard it,
  # leaving the log unable to separate "arrived wedged" from "wedged during fingerprinting"
  # a few seconds later. Recorded, not acted on -- WEDGED still confirms the arm, because a
  # wedged Cameroon keyboard is still the Cameroon keyboard.
  Say ('        arm-confirm deadkey -> {0,-9} {1}' -f $d.State, $d.Cp)
  return (($d.State -eq 'CLEAN') -or ($d.State -eq 'WEDGED'))
}

function Switch-ToArm([string]$want) {
  $k = Get-FocusKeyboard
  if ($k.Arm -eq $want) {
    if ($want -ne 'Keyman') { return $k }
    if (Confirm-KeymanArm) { return $k }
    Say ('        already on {0} but the deadkey says it is not Cameroon - keeping looking' -f (Format-Keyboard $k))
  }

  if ($SwitchMode -eq 'Manual') {
    Say ("        switch to arm '{0}' by hand now ({1})" -f $want, $ARM_LABEL[$want])
    Say  '        waiting up to 120s for the focus thread to confirm it...'
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    while ($sw.Elapsed.TotalSeconds -lt 120) {
      Start-Sleep -Milliseconds 700
      $k = Get-FocusKeyboard
      if ($k.Arm -eq $want) {
        if ($want -ne 'Keyman' -or (Confirm-KeymanArm)) { return $k }
      }
    }
    return $k
  }

  for ($i = 1; $i -le $SwitchTries; $i++) {
    [void](Assert-Foreground)
    Tap-WinSpace
    [void](Assert-Foreground)
    $k = Get-FocusKeyboard
    Say ("        Win+Space #{0} -> {1}" -f $i, (Format-Keyboard $k))
    if ($k.Arm -eq $want) {
      # Several transient langids all classify as 'Keyman'. Only one of them is
      # Cameroon, so the langid match is a candidate and the deadkey is the proof.
      if ($want -ne 'Keyman') { return $k }
      if (Confirm-KeymanArm) { Say '        deadkey confirms Cameroon QWERTY'; return $k }
      Say '        ...that is a different Keyman TIP or a transient-langid layout; continuing'
    }
  }
  return $k
}

# ---- fingerprint ---------------------------------------------------------
# Establishes, rather than assumes, which oracles mean anything on this arm.
#   TWO DIFFERENT QUESTIONS, which the first version of this conflated and got
#   wrong on the 09:26 run:
#     "is this the keyboard I think it is?"  -> LayoutOk
#     "is it currently unwedged?"            -> Valid
#   A deadkey fingerprint of U+0259 U+014A is the STRONGEST POSSIBLE proof that
#   the Cameroon keyboard is active - nothing else on this machine can emit
#   schwa at all - while simultaneously proving it is wedged. Treating that as
#   "wrong keyboard, skip the arm" threw away the entire treatment arm and
#   turned a reproduced wedge into an INCONCLUSIVE verdict. CLEAN and WEDGED
#   both confirm the layout; only OTHER/NO-OUTPUT mean the layout is wrong or
#   unreadable.
function Get-Fingerprint([string]$arm) {
  # 3-attempt Probe, not a single shot: a dropped ';' yields 'e' instead of
  # schwa, and a flaky single read here would mis-identify the layout and skip a
  # good arm.
  $d = Probe 'Deadkey'
  Start-Sleep -Milliseconds 250
  $a = Probe 'Ascii'
  $deadkeyLayoutOk = ($d.State -eq 'CLEAN' -or $d.State -eq 'WEDGED')
  $asciiLayoutOk   = ($a.State -eq 'CLEAN' -or $a.State -eq 'WEDGED')
  $deadkeyValid    = ($d.State -eq 'CLEAN')
  $asciiValid      = ($a.State -eq 'CLEAN')
  Say ("        fingerprint deadkey ';e'+RAlt+N -> {0,-9} {1}" -f $d.State, $d.Cp)
  Say ("        fingerprint ascii   'abc'       -> {0,-9} {1}" -f $a.State, $a.Cp)
  if (-not $deadkeyLayoutOk) {
    Say ("        [NOTE] deadkey oracle does not apply on arm '{0}' - it cannot produce U+0259. Recorded, not counted." -f $arm)
  } elseif (-not $deadkeyValid) {
    Say ("        [NOTE] arm '{0}' is the right keyboard but arrives ALREADY WEDGED. Recovery will be attempted." -f $arm)
  }
  return [pscustomobject]@{
    Arm=$arm
    DeadkeyLayoutOk=$deadkeyLayoutOk; AsciiLayoutOk=$asciiLayoutOk
    DeadkeyValid=$deadkeyValid; AsciiValid=$asciiValid
    ArrivedWedged=(($d.State -eq 'WEDGED') -or ($a.State -eq 'WEDGED'))
    DeadkeyState=$d.State; AsciiState=$a.State
    DeadkeyCp=$d.Cp; DeadkeyText=$d.Text; AsciiCp=$a.Cp; AsciiText=$a.Text
  }
}

# ---- candidate triggers --------------------------------------------------
# Unchanged from the original single-keyboard set, so results stay directly
# comparable across the whole project. Each is one
# discrete action applied from a verified-clean state.
#   A is the internal control: a bare modifier hold with NO freeze. It should
#     stay clean even on the Keyman arm. If A wedges, the freeze is not the
#     mechanism and the story in TRIGGER.md is wrong.
#   I is the primary: B made deterministic by confirming the stall is live
#     before releasing, instead of guessing with a fixed 100ms delay.
$CANDIDATES = @(
  @{ Id='A'; Desc='bare LShift hold 1.5s + release (NO freeze - internal control)'; Act={
       Kd 0xA0; Start-Sleep -Milliseconds 1500; Ku 0xA0; Start-Sleep -Milliseconds 400 } }

  @{ Id='B'; Desc='LShift held, freeze, release INTO the freeze (fixed 100ms delay)'; Act={
       Kd 0xA0; Start-Sleep -Milliseconds 1400; Freeze; Start-Sleep -Milliseconds 100
       Ku 0xA0; Start-Sleep -Milliseconds 400 } }

  @{ Id='C'; Desc='LShift held, freeze, release, then type DURING the freeze'; Act={
       Kd 0xA0; Start-Sleep -Milliseconds 1400; Freeze; Start-Sleep -Milliseconds 100
       Ku 0xA0; Start-Sleep -Milliseconds 150
       Tp 0xBA 60; Tp 0x45 60; Start-Sleep -Milliseconds 300
       for ($i=0;$i -lt 2;$i++){ Tp 8 55 } } }

  @{ Id='D'; Desc='rapid tap of all six modifiers (the "recovery" sweep itself)'; Act={
       TapAllMods } }

  @{ Id='I'; Desc='LShift held, freeze CONFIRMED ACTIVE, then release (primary)'; Act={
       Kd 0xA0; Start-Sleep -Milliseconds 1400
       Freeze
       $live = WaitForFreeze 3000
       # ADDED FOR THE REPO VERSION: the gate needs to know this, and this is the
       # only place it is known. One assignment, no output, no added delay.
       $script:LastFreezeConfirmed = $live
       if (-not $live) { Say '        [WARN] freeze never confirmed - this iteration is not a valid trial' }
       Ku 0xA0; Start-Sleep -Milliseconds 400 } }

  @{ Id='E'; Desc='LShift DOWN, freeze, LShift UP then DOWN then UP inside freeze'; Act={
       Kd 0xA0; Start-Sleep -Milliseconds 1400; Freeze; Start-Sleep -Milliseconds 100
       Ku 0xA0; Start-Sleep -Milliseconds 120
       Kd 0xA0; Start-Sleep -Milliseconds 120
       Ku 0xA0; Start-Sleep -Milliseconds 400 } }

  @{ Id='F'; Desc='Ctrl+Shift chord released OUT OF ORDER during freeze'; Act={
       Kd 0x11; Start-Sleep -Milliseconds 80; Kd 0xA0; Start-Sleep -Milliseconds 1400
       Freeze; Start-Sleep -Milliseconds 100
       Ku 0x11; Start-Sleep -Milliseconds 120     # Ctrl up FIRST
       Ku 0xA0; Start-Sleep -Milliseconds 400 } }

  @{ Id='G'; Desc='RAlt (extended) held, freeze, release into freeze'; Act={
       Kd 0xA5 -E; Start-Sleep -Milliseconds 1400; Freeze; Start-Sleep -Milliseconds 100
       Ku 0xA5 -E; Start-Sleep -Milliseconds 400
       Tp 0x1B 80; Tp 0x1B 80 } }                 # Escape twice: kill any menu

  @{ Id='H'; Desc='LShift held across a freeze re-posted 3x (long stall)'; Act={
       Kd 0xA0; Start-Sleep -Milliseconds 1400
       Freeze; Start-Sleep -Milliseconds 100
       Freeze; Freeze
       Ku 0xA0; Start-Sleep -Milliseconds 400 } }
)

# ---- load emulation ------------------------------------------------------
$loadJobs = @()
if ($LoadThreads -gt 0) {
  for ($i=1; $i -le $LoadThreads; $i++) {
    $loadJobs += Start-Job -ScriptBlock { $x=0.0; while ($true) { $x=[math]::Sqrt([math]::Abs([math]::Sin($x)*1000000.0)) } }
  }
  Start-Sleep -Seconds 2
}

# ---- run ------------------------------------------------------------------
$results      = @()
$fingerprints = @()
$armNotes     = @()
$wedgeCarried = @()

try {
  Say '============== stuck-mod-test: three-arm controlled test =============='
  Say ("target={0} hwnd=0x{1:X}  keyman ctrl=0x{2:X}  load={3}  switch={4}  repeat={5}" -f $TargetProcess,$target.ToInt64(),$script:km.ToInt64(),$LoadThreads,$SwitchMode,$Repeat)
  if ($script:km -eq [IntPtr]::Zero) {
    Say '[WARN] keyman.exe TApplication window not found - the freeze stimulus will be a NO-OP on every arm.'
    Say '       Every result below would then be a no-freeze control. Fix this before quoting any of it.'
  }
  # MSKLC IS OPTIONAL. The load-bearing contrast is Keyman's engine against a
  # keyboard Windows implements itself; the English arm already supplies that.
  # MSKLC-Cameroon adds a SAME-LAYOUT control, which separates "Keyman's engine"
  # from "the Cameroon layout data". Valuable, but not required, and not
  # something a reviewer can be expected to have installed. Absent, the run
  # continues on two arms and the verdict says exactly what the missing arm
  # would have ruled out.
  $installed = Get-InstalledArms
  Say ("installed input methods: {0}" -f ($installed.Detail -join '; '))
  if ($Arms -contains 'MSKLC' -and $installed.Arms -notcontains 'MSKLC') {
    $Arms = @($Arms | Where-Object { $_ -ne 'MSKLC' })
    Say '[NOTE] no Microsoft Cameroon (0x0436) layout installed - dropping the MSKLC arm.'
    Say '       The run continues on English + Keyman. That still attributes the wedge to'
    Say '       Keyman; what it cannot do on its own is separate Keyman the ENGINE from the'
    Say '       Cameroon layout it loads. Install the MSKLC layout to recover that control.'
  }
  if ($Arms -contains 'English' -and $installed.Arms -notcontains 'English') {
    Say '[WARN] no plain (non-substituted) English QWERTY found among the installed layouts.'
    Say '       Any English locale will do - US, UK, Australian, Canadian, NZ, Irish - but a'
    Say '       Dvorak/US-International style alternate is rejected, because the Ascii oracle'
    Say '       assumes QWERTY. Add one as an input method, or the control arm cannot run.'
    Add-GateInconclusive 'no plain (non-substituted) English QWERTY input method is installed - the required control arm cannot run'
  }
  if ($installed.Arms -notcontains 'Keyman') {
    Say '[WARN] no Keyman TIP (transient langid 0x2000/0x2400/0x2800/0x2C00) found. The treatment arm cannot run.'
    Say '       Install the sil_cameroon_qwerty keyboard in Keyman and re-run.'
    Add-GateInconclusive 'no Keyman TIP is installed - the required treatment arm (sil_cameroon_qwerty) cannot run'
  }
  $script:ARMS_ALL = @('English','MSKLC','Keyman') | Where-Object { $Arms -contains $_ }
  Say ("arms  = {0}" -f ($ARMS_ALL -join ' -> '))
  # MSKLC is optional; English and Keyman are the two the gate rests on.
  if ($Gate) {
    foreach ($req in @('English','Keyman')) {
      if ($ARMS_ALL -notcontains $req) {
        Add-GateInconclusive ("required arm '{0}' is not in this run - the gate needs both a Keyman arm and a non-Keyman control arm" -f $req)
      }
    }
    if ($ARMS_ALL -notcontains 'MSKLC') {
      GateSay '[gate] running without the optional MSKLC arm. A PASS still means Keyman did not wedge under'
      GateSay '       the stimulus; what it cannot separately show is Keyman the ENGINE from the Cameroon'
      GateSay '       layout data, since there is no Microsoft build of the same layout to compare against.'
    }
  }

  # -SwitchStress and -ChargeTest both need a NON-KEYMAN keyboard to switch away
  # to. MSKLC is the better choice when it exists, because it can express the
  # Deadkey oracle and so proves the Microsoft side stayed perfect in the
  # sharpest possible way. Without it the English arm does the same job on the
  # Ascii oracle - a phantom Shift still shows as 'ABC' - so neither mode has to
  # be blocked. The oracle in use is recorded on every row.
  $script:AWAY_ARM    = if ($ARMS_ALL -contains 'MSKLC') { 'MSKLC' } else { 'English' }
  $script:AWAY_ORACLE = if ($AWAY_ARM -eq 'MSKLC') { 'Deadkey' } else { 'Ascii' }
  Say  'order = controls first, treatment last (the Keyman wedge is persistent)'
  Say ("log   = {0}" -f $log)
  Say ''
  Say ("startup keyboard: {0}" -f (Format-Keyboard (Get-FocusKeyboard)))

  # ENTRY PROBE. Establishes whether the machine was ALREADY wedged before this
  # script touched anything. Without it, a wedge found later cannot be
  # attributed: the 09:26 run reached the Keyman arm wedged and there was no way
  # to tell whether a trial did it, the arm switch did it, or it walked in that
  # way. Ascii, because it is the one oracle valid on every layout here.
  # #8064: ONCE, here, before anything is measured -- never during a pass. The modifier
  # cache lives in keyman.exe and survives between runs, so a previous run that ended
  # wedged leaves this one starting dirty; that is what turned two earlier gate runs into
  # INCONCLUSIVE with "arm 'Keyman' arrived ALREADY WEDGED". A restart re-seeds the cache
  # from live OS state via InitThread.
  #
  # Deliberately NOT repeated between arms. Restarting mid-experiment is an intervention a
  # reader is right to distrust, and it would erase the carry-over that phases WEDGED and
  # CLEARED exist to show. If the treatment arm still arrives wedged after this, that is a
  # RESULT -- the control arms charged the cache -- and not a setup failure to be tidied away.
  [void](Reset-KeymanEngine)

  # ENTRY CHECK. Restart above, then TYPE to confirm it actually worked, on the Keyman arm
  # specifically -- that is the only arm whose cache can be wedged, and a probe on whatever
  # happened to be active says nothing about it. A dirty start is a SETUP ERROR and stops the
  # run: every earlier INCONCLUSIVE in this directory traces back to a run that began wedged
  # and was allowed to continue anyway.
  $ek = Switch-ToArm 'Keyman'
  if ($ek.Arm -ne 'Keyman') {
    Say ("[SETUP ERROR] could not switch to the Keyman arm for the entry check (saw {0})." -f $ek.Arm)
    exit $script:SETUP_EXIT
  }
  $entry = Probe 'Ascii'
  Say ("entry probe (Ascii, on the Keyman arm): {0} ({1}) mods={2}" -f $entry.State,$entry.Cp,$entry.Mods)
  if ($entry.State -ne 'CLEAN') {
    Say ''
    Say '[SETUP ERROR] Keyman is WEDGED before any trial ran, so nothing measured below could'
    Say '              be attributed to this run. The restart did not clear it.'
    Say ''
    Say '  To clear it: exit Keyman completely, tap both Shift, Ctrl and Alt keys on the'
    Say '  physical keyboard, start Keyman again, and type in Notepad to confirm you get'
    Say '  lowercase text before re-running this script.'
    Say ''
    exit $script:SETUP_EXIT
  }
  Say '  entry check passed - Keyman is clean, starting the run.'
  Say ''

  # ---- switch-stress mode ------------------------------------------------
  # Prompted by the 09:26 run: the Keyman arm was reached ALREADY WEDGED, having
  # been clean minutes earlier, and no trial had run on it. The only things that
  # had happened in between were ten freeze+release trials on the two MICROSOFT
  # arms and the Win+Space switches themselves. This mode isolates the second
  # possibility by removing the freeze entirely.
  if ($SwitchStress -gt 0) {
    Say '================ SWITCH-STRESS MODE ================'
    Say '  No freeze is EVER posted in this mode. The only stimulus is switching keyboards.'
    Say '  If Keyman wedges here, the switch is a trigger in its own right.'
    Say ("  Switching away to the {0} arm." -f $AWAY_ARM)
    Say ''
    $wedgeAt = 0
    for ($c = 1; $c -le $SwitchStress; $c++) {
      $k1 = Switch-ToArm $AWAY_ARM
      if ($k1.Arm -ne $AWAY_ARM)  { Say ("  [ABORT] cycle {0} could not reach {1}" -f $c,$AWAY_ARM); break }
      $k2 = Switch-ToArm 'Keyman'
      if ($k2.Arm -ne 'Keyman') { Say ("  [ABORT] cycle {0} could not reach Keyman" -f $c); break }
      $p = Probe 'Deadkey'
      Say ("  cycle {0,-3} back on Keyman -> {1,-9} ({2}) mods={3}" -f $c,$p.State,$p.Cp,$p.Mods)
      $results += [pscustomobject]@{
        Arm='Keyman'; Pass=$c; Candidate='SWITCH'; Desc=("Win+Space to " + $AWAY_ARM + ' and back, NO freeze'); Oracle='Deadkey'
        State=$p.State; Cp=$p.Cp; Text=$p.Text; Mods=$p.Mods
        LangId=('0x{0:X4}' -f $k2.LangId); Hkl=('0x{0:X8}' -f $k2.Hkl)
        ArmConfirmed=$true; Valid=($p.State -eq 'CLEAN' -or $p.State -eq 'WEDGED'); LoadThreads=$LoadThreads
      }
      if ($p.State -eq 'WEDGED') {
        $wedgeAt = $c
        Say ("  *** WEDGED after {0} switch cycle(s), with NO freeze posted ***" -f $c)
        break
      }
      ClearMods
    }
    Say ''
    Say 'SWITCH-STRESS RESULT'
    if ($wedgeAt -gt 0) {
      Say ("  The arm switch ALONE wedged Keyman after {0} cycle(s). The freeze is NOT required." -f $wedgeAt)
      Say  '  TRIGGER.md would then need to widen its mechanism: a keyboard switch is'
      Say  '  sufficient to desynchronise Keyman''s cached modifier state, and the'
      Say  '  freeze is one way to starve the hook thread rather than the only way.'
    } else {
      Say ("  {0} switch cycles, no wedge. The switch alone is NOT sufficient." -f $SwitchStress)
      Say  '  That leaves the freeze trials on the Microsoft arms as the thing that wedged Keyman,'
      Say  '  which would mean Keyman tracks modifier state even when its own keyboard is inactive.'
    }
    if ($results.Count -gt 0) {
      $results | Export-Csv -Path $csvPath -NoTypeInformation -Encoding UTF8
      Say ("  csv  : {0}" -f $csvPath)
    }
    Say ("  log  : {0}" -f $log)
    Say '==============================================================='
    return
  }

  # ---- sweep mode --------------------------------------------------------
  # One pass through all three keyboards per phase. Returns the rows so the
  # caller owns accumulation ($results += inside a function would only mutate a
  # function-local copy).
  function Invoke-SweepPass([string]$label, [int]$triggers) {
    $rows = @()
    foreach ($arm in $ARMS_ALL) {
      $k = Switch-ToArm $arm
      if ($k.Arm -ne $arm) {
        Say ("  [{0,-7}] {1,-7} SKIP - could not switch (focus thread says {2})" -f $label,$arm,$k.Arm)
        continue
      }
      for ($t = 1; $t -le $triggers; $t++) {
        Kd 0xA0; Start-Sleep -Milliseconds 1400
        Freeze
        $live = WaitForFreeze 3000
        if (-not $live) { Say ("  [{0,-7}] {1,-7} [WARN] freeze not confirmed on trigger {2} - not a valid trial" -f $label,$arm,$t) }
        Ku 0xA0; Start-Sleep -Milliseconds 400
        Start-Sleep -Milliseconds 5200          # let the 5s freeze finish
      }
      # An English layout cannot express the deadkey oracle at all - it cannot
      # produce U+0259. The Cameroon arms can.
      $oracles = @('Ascii')
      if ($arm -ne 'English') { $oracles = @('Ascii','Deadkey') }
      $what = 'observe only'
      if ($triggers -gt 0) { $what = ("{0} trigger(s)" -f $triggers) }
      foreach ($o in $oracles) {
        $p = Probe $o
        Say ("  [{0,-7}] {1,-7} {2,-8} {3,-14} -> {4,-9} ({5}) mods={6}" -f $label,$arm,$o,$what,$p.State,$p.Cp,$p.Mods)
        $rows += [pscustomobject]@{
          Arm=$arm; Pass=0; Candidate=('sweep-' + $label); Desc=("sweep phase " + $label + ', ' + $what); Oracle=$o
          State=$p.State; Cp=$p.Cp; Text=$p.Text; Mods=$p.Mods
          LangId=('0x{0:X4}' -f $k.LangId); Hkl=('0x{0:X8}' -f $k.Hkl)
          ArmConfirmed=$true; Valid=($p.State -eq 'CLEAN' -or $p.State -eq 'WEDGED'); LoadThreads=$LoadThreads
          Phase=$label
        }
      }
    }
    return $rows
  }

  function Get-SweepState($rows, [string]$arm, [string]$oracle) {
    $r = @($rows | Where-Object { $_.Arm -eq $arm -and $_.Oracle -eq $oracle })
    if ($r.Count -eq 0) { return '-' }
    return $r[-1].State
  }

  if ($Sweep) {
    Say '================ SWEEP: trigger / observe-wedged / clear / observe-clean ================'
    Say ("  {0} trigger(s) per keyboard in the TRIGGER phase; later phases apply NOTHING." -f $SweepTrials)
    Say ''

    Say '---- phase 1: TRIGGER (walk all three, trigger on each) ----'
    $p1 = Invoke-SweepPass 'TRIGGER' $SweepTrials
    $results += $p1
    $kmAfter1 = Get-SweepState $p1 'Keyman' 'Deadkey'
    Say ''

    $p2 = @()
    if ($kmAfter1 -eq 'CLEAN') {
      Say ("---- phase 2: SKIPPED - Keyman came back CLEAN after {0} trigger(s) per keyboard ----" -f $SweepTrials)
      Say  '     The bug was not triggered, so there is no wedged state to observe.'
      Say  '     Re-run with a higher -SweepTrials (the charge test needed 5 on MSKLC).'
    } else {
      Say '---- phase 2: WEDGED (walk all three again, applying NOTHING) ----'
      Say  '     This is the Keyman-only claim in its sharpest form: same machine, same'
      Say  '     session, same moment. Do the Microsoft keyboards still type correctly?'
      $p2 = Invoke-SweepPass 'WEDGED' 0
      $results += $p2
    }
    Say ''

    Say '---- phase 3: CLEAR the wedge ----'
    $k = Switch-ToArm 'Keyman'
    $cleared = $false
    if ($k.Arm -eq 'Keyman') {
      ClearMods; TapAllMods
      $rec = Probe 'Deadkey'
      Say ("  injected recovery (ClearMods + TapAllMods) -> {0} ({1})" -f $rec.State,$rec.Cp)
      $cleared = ($rec.State -eq 'CLEAN')
      if (-not $cleared) {
        # A physical double-tap on LShift is known to clear this where the
        # injected sweep does not - injected keys carry LLKHF_INJECTED and
        # Keyman can tell them apart. Ask for one rather than giving up.
        Say  '  injected recovery did not clear it.'
        Say  '  ACTION NEEDED: double-tap the physical LEFT SHIFT key now. Waiting up to 90s...'
        $sw = [System.Diagnostics.Stopwatch]::StartNew()
        while ($sw.Elapsed.TotalSeconds -lt 90) {
          Start-Sleep -Milliseconds 1500
          $rec = Probe 'Deadkey'
          if ($rec.State -eq 'CLEAN') { $cleared = $true; break }
        }
        if ($cleared) { Say ("  cleared by physical keystroke after {0:N0}s" -f $sw.Elapsed.TotalSeconds) }
        else          { Say  '  still not clear. Keyman restart is the documented fallback; not doing that here.' }
      }
    } else {
      Say ("  [SKIP] could not reach Keyman to clear (saw {0})" -f $k.Arm)
    }
    Say ''

    $p3 = @()
    if ($cleared) {
      Say '---- phase 4: CLEARED (walk all three again, applying NOTHING) ----'
      $p3 = Invoke-SweepPass 'CLEARED' 0
      $results += $p3
    } else {
      Say '---- phase 4: SKIPPED - the wedge was never cleared ----'
    }
    Say ''

    # ---- matrix -----------------------------------------------------------
    Say 'SWEEP MATRIX'
    Say ('  {0,-7} {1,-8} {2,-11} {3,-11} {4,-11}' -f 'arm','oracle','TRIGGER','WEDGED','CLEARED')
    foreach ($arm in $ARMS_ALL) {
      $oracles = @('Ascii')
      if ($arm -ne 'English') { $oracles = @('Ascii','Deadkey') }
      foreach ($o in $oracles) {
        Say ('  {0,-7} {1,-8} {2,-11} {3,-11} {4,-11}' -f $arm,$o,
              (Get-SweepState $p1 $arm $o), (Get-SweepState $p2 $arm $o), (Get-SweepState $p3 $arm $o))
      }
    }
    Say ''

    # ---- verdict ----------------------------------------------------------
    # The phase a Microsoft arm goes bad in decides what it MEANS. The first
    # version of this treated any non-CLEAN Microsoft probe as refuting the
    # claim, which is wrong: during the WEDGED phase a Microsoft keyboard
    # emitting ABC is the EXPECTED consequence of Keyman having injected a real
    # LShift KEYDOWN with no matching KEYUP, and is evidence FOR the diagnosis,
    # not against it. Only the TRIGGER phase can refute Keyman-only causation.
    Say 'SWEEP VERDICT'
    $msTrigger = @($p1 | Where-Object { ($_.Arm -eq 'English' -or $_.Arm -eq 'MSKLC') -and $_.State -ne 'CLEAN' })
    $msWedged  = @($p2 | Where-Object { ($_.Arm -eq 'English' -or $_.Arm -eq 'MSKLC') -and $_.State -ne 'CLEAN' })
    $msCleared = @($p3 | Where-Object { ($_.Arm -eq 'English' -or $_.Arm -eq 'MSKLC') -and $_.State -ne 'CLEAN' })

    if ($kmAfter1 -eq 'CLEAN') {
      Say ("  [NOT TRIGGERED] one pass with {0} trigger(s) per keyboard did not wedge Keyman." -f $SweepTrials)
      Say  '                  Raise -SweepTrials and re-run before drawing any conclusion.'
    } elseif ($msTrigger.Count -gt 0) {
      Say ('  [CAUSATION CLAIM FAILS] the trigger itself disturbed a Microsoft keyboard in {0} probe(s):' -f $msTrigger.Count)
      foreach ($r in $msTrigger) { Say ('      {0} {1} {2} -> {3} ({4})' -f $r.Phase,$r.Arm,$r.Oracle,$r.State,$r.Cp) }
      Say  '      That would mean this is not Keyman-specific. Investigate before quoting.'
    } else {
      Say ('  [CAUSED BY KEYMAN ONLY] Under the identical trigger, {0} stayed CLEAN' -f (($ARMS_ALL | Where-Object { $_ -ne 'Keyman' }) -join ' and '))
      Say  '      while Keyman wedged. Windows is not at fault.'
      if ($ARMS_ALL -notcontains 'MSKLC') {
        Say  '      Without the MSKLC arm this does not separately exonerate the Cameroon LAYOUT.'
      } else {
        Say  '      Nor is the layout: MSKLC runs the same layout and stayed clean.'
      }
      if ($msWedged.Count -gt 0) {
        Say  ''
        Say  '  [BUT THE DAMAGE IS MACHINE-WIDE] Once wedged, the Microsoft keyboards are'
        Say  '      affected too, with NO trigger applied to them:'
        foreach ($r in $msWedged) { Say ('      {0,-7} {1,-8} -> {2} ({3}) mods={4}' -f $r.Arm,$r.Oracle,$r.State,$r.Cp,$r.Mods) }
        Say  '      They are not malfunctioning - they are correctly rendering a Shift that is'
        Say  '      genuinely held as far as Windows is concerned. GetAsyncKeyState agrees.'
        Say  '      Keyman synthesised it: keybd_shift_reset() emits a KEYDOWN for every'
        Say  '      modifier its cache believes is held, with no matching KEYUP.'
        Say  '      So: caused only via Keyman, suffered by everything.'
      } elseif ($p2.Count -gt 0) {
        Say  '      During the WEDGED phase the Microsoft keyboards stayed CLEAN, so the bad'
        Say  '      state did NOT escape into OS-level key state on this run.'
      }
      if ($cleared -and $p3.Count -gt 0 -and $msCleared.Count -eq 0) {
        Say  ''
        Say  '  [RECOVERABLE] After clearing, all three keyboards are CLEAN again - a'
        Say  '      recoverable desync, not permanent damage.'
      }
    }

    if ($results.Count -gt 0) {
      $results | Export-Csv -Path $csvPath -NoTypeInformation -Encoding UTF8
      Say ("  csv  : {0}" -f $csvPath)
    }
    Say ("  log  : {0}" -f $log)
    Say '==============================================================='
    return
  }

  # ---- charge test -------------------------------------------------------
  # THE experiment for the sharpened claim. Two runs of the three-arm test both
  # reached the Keyman arm ALREADY WEDGED, with the previous arm's exit probe
  # CLEAN, and -SwitchStress then showed 10/10 clean for the switches alone. By
  # elimination the freeze+release trials are doing it - but those trials ran
  # while a MICROSOFT keyboard was active, which the k32 pass-through reasoning
  # in TRIGGER.md says should be inert. This measures that directly instead of
  # inferring it, and it is the paired treatment for -SwitchStress's control.
  #
  # Read the output as: MSKLC output stays perfect throughout the charging phase
  # (so the Microsoft implementation is genuinely unaffected), and yet Keyman is
  # found corrupted the moment its keyboard becomes active again.
  if ($ChargeTest -gt 0) {
    Say '================ CHARGE TEST ================'
    Say ("  Per rep: {0} x candidate I on {1} (Keyman keyboard INACTIVE), then switch to Keyman and probe." -f $ChargeTrials,$AWAY_ARM)
    Say ("  Charging-arm oracle: {0}." -f $AWAY_ORACLE)
    if ($AWAY_ARM -ne 'MSKLC') {
      Say  '  NOTE: no MSKLC layout installed, so the charging arm is English on the Ascii'
      Say  '        oracle. That still shows a phantom Shift as ABC, which is what this mode'
      Say  '        needs; it just cannot also demonstrate correct Cameroon output meanwhile.'
    }
    Say  '  Paired control is -SwitchStress: identical switches, no freeze.'
    Say ''
    $charged = 0; $reps = 0; $msklcDirty = 0; $msklcTrials = 0
    for ($r = 1; $r -le $ChargeTest; $r++) {
      $k1 = Switch-ToArm $AWAY_ARM
      if ($k1.Arm -ne $AWAY_ARM) { Say ("  [ABORT] rep {0} could not reach {1}" -f $r,$AWAY_ARM); break }

      $pre = Probe $AWAY_ORACLE
      if ($pre.State -ne 'CLEAN') {
        Say ("  rep {0}: {1} does not start clean ({2}) - rep abandoned, not counted" -f $r,$AWAY_ARM,$pre.Cp)
        continue
      }

      $dirtyHere = 0
      for ($t = 1; $t -le $ChargeTrials; $t++) {
        Kd 0xA0; Start-Sleep -Milliseconds 1400
        Freeze
        $live = WaitForFreeze 3000
        if (-not $live) { Say ("      rep {0} trial {1}: [WARN] freeze never confirmed - not a valid charging trial" -f $r,$t) }
        Ku 0xA0; Start-Sleep -Milliseconds 400
        $m = Probe $AWAY_ORACLE
        $msklcTrials++
        if ($m.State -ne 'CLEAN') { $dirtyHere++; $msklcDirty++ }
        Say ("      rep {0} charge trial {1} on {2} -> {3,-9} ({4}) mods={5}" -f $r,$t,$AWAY_ARM,$m.State,$m.Cp,$m.Mods)
        $results += [pscustomobject]@{
          Arm=$AWAY_ARM; Pass=$r; Candidate='I-charge'; Desc='candidate I while Keyman keyboard INACTIVE'; Oracle=$AWAY_ORACLE
          State=$m.State; Cp=$m.Cp; Text=$m.Text; Mods=$m.Mods
          LangId=('0x{0:X4}' -f $k1.LangId); Hkl=('0x{0:X8}' -f $k1.Hkl)
          ArmConfirmed=$true; Valid=($m.State -eq 'CLEAN' -or $m.State -eq 'WEDGED'); LoadThreads=$LoadThreads
        }
        Start-Sleep -Milliseconds 5200
      }

      $k2 = Switch-ToArm 'Keyman'
      if ($k2.Arm -ne 'Keyman') { Say ("  [ABORT] rep {0} could not reach Keyman" -f $r); break }
      $post = Probe 'Deadkey'
      $reps++
      $verdict = 'clean'
      if ($post.State -ne 'CLEAN') { $verdict = '*** ' + $post.State + ' ***'; $charged++ }
      Say ("  rep {0}: {6} dirty {1}/{2} during charging  ->  Keyman on return: {3} ({4}) mods={5}" -f `
            $r,$dirtyHere,$ChargeTrials,$verdict,$post.Cp,$post.Mods,$AWAY_ARM)
      $results += [pscustomobject]@{
        Arm='Keyman'; Pass=$r; Candidate='I-fire'; Desc='first probe after switching back to Keyman'; Oracle='Deadkey'
        State=$post.State; Cp=$post.Cp; Text=$post.Text; Mods=$post.Mods
        LangId=('0x{0:X4}' -f $k2.LangId); Hkl=('0x{0:X8}' -f $k2.Hkl)
        ArmConfirmed=$true; Valid=($post.State -eq 'CLEAN' -or $post.State -eq 'WEDGED'); LoadThreads=$LoadThreads
      }

      if ($post.State -ne 'CLEAN') {
        # Does INJECTED recovery work? A physical LShift double-tap is known to
        # clear this; the six-modifier injected sweep was seen to make it worse
        # (wedged -> NO-OUTPUT). Record which, because it bears on whether
        # LLKHF_INJECTED changes how Keyman treats the keys.
        ClearMods; TapAllMods
        $rec = Probe 'Deadkey'
        Say ("        injected recovery (ClearMods+TapAllMods) -> {0} ({1})" -f $rec.State,$rec.Cp)
        if ($rec.State -ne 'CLEAN') {
          Say  '        injected recovery FAILED. Physical keys may still clear it; this harness cannot test that.'
          Say  '        Stopping: the state is worth examining live, and later reps would not start clean.'
          break
        }
      }
    }
    Say ''
    Say 'CHARGE TEST RESULT'
    Say ("  MSKLC output during charging : {0}/{1} trials NOT clean" -f $msklcDirty,$msklcTrials)
    Say ("  Keyman on return             : {0}/{1} reps corrupted" -f $charged,$reps)
    if ($reps -eq 0) {
      Say '  [INCONCLUSIVE] no rep completed.'
    } elseif ($charged -gt 0 -and $msklcDirty -eq 0) {
      Say  '  [CONFIRMED] The same keystrokes that leave the Microsoft keyboard PERFECT leave'
      Say  '              Keyman corrupted - and Keyman was not even the active keyboard.'
      Say  '              This is stronger than "Keyman-only": the layout is irrelevant, and'
      Say  '              Keyman keeps modifier state it should not be keeping while inactive.'
      Say  '              TRIGGER.md must drop the !isKeymanKeyboardActive pass-through argument.'
    } elseif ($charged -eq 0) {
      Say  '  [NOT REPRODUCED] Keyman came back clean every rep. The charge hypothesis is not supported'
      Say  '                   by this run; the earlier wedges need another explanation.'
    } else {
      Say  '  [MIXED] MSKLC output was also disturbed during charging, so this is not a clean'
      Say  '          Keyman-only result. Investigate before quoting.'
    }
    if ($results.Count -gt 0) {
      $results | Export-Csv -Path $csvPath -NoTypeInformation -Encoding UTF8
      Say ("  csv  : {0}" -f $csvPath)
    }
    Say ("  log  : {0}" -f $log)
    Say '==============================================================='
    return
  }

  foreach ($arm in $Arms) {
    Say ('================ ARM: {0} ================' -f $arm)
    Say ('  {0}' -f $ARM_LABEL[$arm])

    # A wedge left over from the previous arm is itself a datapoint.
    $carriedIn = $null

    $k = Switch-ToArm $arm
    if ($k.Arm -ne $arm) {
      Say ("  [SKIP] could not reach arm '{0}' - focus thread reports {1}" -f $arm, (Format-Keyboard $k))
      Say  '  [SKIP] this arm contributes NOTHING to the result. Not silently dropped: see the summary.'
      $armNotes += [pscustomobject]@{ Arm=$arm; Note='SKIPPED - could not switch'; Detail=(Format-Keyboard $k) }
      Add-GateInconclusive ("the arm switch to '{0}' could not be confirmed on the focus thread" -f $arm)
      continue
    }
    Say ("  confirmed: {0}" -f (Format-Keyboard $k))

    $fp = Get-Fingerprint $arm
    $fingerprints += $fp

    # -FingerprintOnly stops here: switch, identify, record what the keyboard
    # actually emits, run no trials. This is the cheap sanity pass to run first,
    # and it is what establishes for TRIGGER.md that US genuinely cannot produce
    # the special characters.
    if ($FingerprintOnly) { Say '  [FingerprintOnly] no trials run on this arm'; Say ''; continue }

    # The Keyman arm is only accepted if the deadkey fingerprint really yields
    # schwa+eng: langid 0x2000 is shared by the Keyman Cameroon and Keyman
    # Yoruba profiles in the registry.
    # LayoutOk, not Valid: an arm that arrives wedged is the right keyboard and
    # must be recovered and measured, not skipped. See Get-Fingerprint.
    if ($arm -eq 'Keyman' -and -not $fp.DeadkeyLayoutOk) {
      Say '  [SKIP] arm says Keyman/0x2000 but the deadkey fingerprint is neither schwa+eng nor schwa+ENG - this is a different Keyman keyboard.'
      $armNotes += [pscustomobject]@{ Arm=$arm; Note='SKIPPED - 0x2000 but wrong deadkey fingerprint'; Detail=$fp.DeadkeyCp }
      Add-GateInconclusive 'the Keyman arm carries a transient langid but the deadkey fingerprint is not Cameroon QWERTY - the treatment arm was never reached'
      continue
    }
    if (-not $fp.AsciiLayoutOk) {
      Say '  [SKIP] the Ascii oracle is unreadable on this arm, so the cross-arm measurement cannot be made here.'
      $armNotes += [pscustomobject]@{ Arm=$arm; Note='SKIPPED - ascii oracle unreadable'; Detail=$fp.AsciiCp }
      Add-GateInconclusive ("the Ascii oracle was not marked valid for arm '{0}' by fingerprinting - it read {1}" -f $arm, $fp.AsciiCp)
      continue
    }
    if ($fp.ArrivedWedged) {
      Say '  [IMPORTANT] this arm ARRIVED WEDGED. Whatever wedged it happened BEFORE any trial here.'
      Say '              Compare against the previous arm''s exit probe to localise it.'
      $armNotes += [pscustomobject]@{ Arm=$arm; Note='ARRIVED WEDGED before any trial'; Detail=$fp.DeadkeyCp }
      Add-GateInconclusive ("arm '{0}' arrived ALREADY WEDGED, before any trial ran on it, so no reading on this arm can be attributed to this run" -f $arm)
    }

    # Oracles to run on this arm. Ascii always (it is the comparable one);
    # Deadkey wherever the layout can express it, wedged or not.
    $oracles = @('Ascii')
    if ($fp.DeadkeyLayoutOk) { $oracles += 'Deadkey' }
    Say ("  oracles in play: {0}" -f ($oracles -join ', '))

    $base = Probe 'Ascii'
    Say ("  baseline (Ascii): {0} '{1}' ({2}) mods={3}" -f $base.State,$base.Text,$base.Cp,$base.Mods)
    if ($base.State -ne 'CLEAN') {
      Say '  [WARN] not starting clean - attempting recovery'
      ClearMods; TapAllMods
      $base = Probe 'Ascii'
      Say ("  after recovery: {0} ({1})" -f $base.State,$base.Cp)
      if ($base.State -ne 'CLEAN') {
        Say '  [SKIP] cannot reach a clean baseline on this arm. Not restarting Keyman - that would destroy the state.'
        $armNotes += [pscustomobject]@{ Arm=$arm; Note='SKIPPED - no clean baseline'; Detail=$base.Cp }
        Add-GateInconclusive ("arm '{0}' could not reach a clean baseline, so no trial ran on it" -f $arm)
        continue
      }
    }
    Say ''

    for ($pass = 1; $pass -le $Repeat; $pass++) {
      foreach ($c in $CANDIDATES) {
        if ($Only.Count -gt 0 -and $Only -notcontains $c.Id) { continue }

        # Confirm the arm has not drifted under us mid-pass. A Win+Space by a
        # human, or a focus change, would otherwise mis-attribute the trial.
        $kNow = Get-FocusKeyboard
        $armOk = ($kNow.Arm -eq $arm)

        # ADDED FOR THE REPO VERSION. Announced BEFORE the trigger action, never
        # between the trigger and the probe - see the Write-Host note by Say.
        if ($Gate) {
          $gfz = $GATE_FREEZE[$c.Id]
          if (-not $gfz) { $gfz = 'unconfirmed' }
          GateSay ('  [gate] arm={0} candidate={1} trial {2} of {3}; freeze={4}; oracles={5}' -f $arm,$c.Id,$pass,$Repeat,$gfz,($oracles -join ','))
          GateSay ('  [gate]   action: {0}' -f $c.Desc)
          foreach ($gOracle in $oracles) {
            GateSay ('  [gate]   expect {0}; a wedge would read {1}' -f (Get-GateExpectation $gOracle),(Get-GateWedgeForm $gOracle))
          }
        }
        $script:LastFreezeConfirmed = $null

        & $c.Act
        Start-Sleep -Milliseconds 300

        foreach ($oracle in $oracles) {
          $post = Probe $oracle
          $valid = $armOk -and ($post.State -eq 'CLEAN' -or $post.State -eq 'WEDGED')
          $tag = 'clean '
          if ($post.State -ne 'CLEAN') { $tag = '*** ' + $post.State + ' ***' }
          $vtag = ''
          if (-not $valid) { $vtag = '  [INVALID' + $(if (-not $armOk) { '/arm-drift' } else { '/unreadable' }) + ']' }

          Say ("  {0} p{1} [{2}] {3,-9} {4,-58} langid=0x{5:X4} -> {6} ({7}) mods={8}{9}" -f `
                $arm,$pass,$c.Id,$oracle,$c.Desc,$kNow.LangId,$tag,$post.Cp,$post.Mods,$vtag)

          $results += [pscustomobject]@{
            Arm=$arm; Pass=$pass; Candidate=$c.Id; Desc=$c.Desc; Oracle=$oracle
            State=$post.State; Cp=$post.Cp; Text=$post.Text; Mods=$post.Mods
            LangId=('0x{0:X4}' -f $kNow.LangId); Hkl=('0x{0:X8}' -f $kNow.Hkl)
            ArmConfirmed=$armOk; Valid=$valid; LoadThreads=$LoadThreads
          }

          if ($Gate) { Register-GateTrial $arm $c.Id $pass $oracle $post $armOk $fp }
        }

        if ($Gate) {
          GateSay ('  [gate]   running tally: PASS {0}  FAIL {1}  INCONCLUSIVE {2}' -f $script:GatePass,$script:GateFail,$script:GateInc)
        }

        # recover so the next candidate starts fair
        $lastState = ($results | Where-Object { $_.Arm -eq $arm -and $_.Pass -eq $pass -and $_.Candidate -eq $c.Id } | Select-Object -Last 1).State
        if ($lastState -ne 'CLEAN') {
          ClearMods
          $r1 = Probe 'Ascii'
          if ($r1.State -ne 'CLEAN') {
            Say ("        explicit KEYUP sweep did NOT recover ({0}); trying modifier taps" -f $r1.Cp)
            TapAllMods
            $r2 = Probe 'Ascii'
            Say ("        after modifier taps: {0} ({1})" -f $r2.State,$r2.Cp)
            if ($r2.State -ne 'CLEAN') {
              Say '        STILL WEDGED - this is the persistent field symptom. Ending this arm so it can be examined live.'
              Say '        (Keyman restart is the documented recovery. This script will not do it for you.)'
              $armNotes += [pscustomobject]@{ Arm=$arm; Note='arm ended early - persistent wedge'; Detail=("pass $pass candidate " + $c.Id) }
              if ($arm -ne 'Keyman') {
                Add-GateInconclusive ("control arm '{0}' was left persistently wedged, which indicts the bench rather than Keyman" -f $arm)
              }
              $carriedIn = $arm
              break
            }
          } else {
            Say ("        recovered by explicit KEYUP sweep alone ({0})" -f $r1.Cp)
          }
        }
        Start-Sleep -Milliseconds 5200      # let any 5s freeze finish
      }
      if ($carriedIn) { break }
    }

    # EXIT PROBE. Pairs with the next arm's fingerprint to bracket the arm
    # switch. If an arm exits CLEAN and the next arm's fingerprint is WEDGED,
    # then nothing in this arm's trials did it and the switch itself (Win+Space,
    # which holds LWIN across a TSF profile change) becomes the prime suspect -
    # see -SwitchStress, which tests exactly that with no freeze at all.
    $exit = Probe 'Ascii'
    Say ("  exit probe (Ascii): {0} ({1}) mods={2}" -f $exit.State,$exit.Cp,$exit.Mods)
    $armNotes += [pscustomobject]@{ Arm=$arm; Note=('exit probe ' + $exit.State); Detail=$exit.Cp }

    if ($carriedIn) {
      $wedgeCarried += [pscustomobject]@{ Arm=$arm; Note='left wedged at end of arm' }
      Say ('  [NOTE] arm {0} ends WEDGED. The next arm''s fingerprint will show whether it survives the switch.' -f $arm)
    }
    Say ''
  }

  # ---- summary -----------------------------------------------------------
  Say '=========================== SUMMARY ==========================='
  Say ''
  Say 'Fingerprints (what each keyboard actually produced):'
  foreach ($f in $fingerprints) {
    $dk = 'not applicable'
    if ($f.DeadkeyLayoutOk -and $f.DeadkeyValid) { $dk = 'VALID' }
    elseif ($f.DeadkeyLayoutOk)                  { $dk = 'VALID but arrived WEDGED' }
    Say ("  {0,-7} deadkey={1,-28} ascii={2,-20} deadkeyOracle={3}" -f $f.Arm,$f.DeadkeyCp,$f.AsciiCp,$dk)
  }
  Say ''

  $valid = @($results | Where-Object { $_.Valid })
  Say ('Trials: {0} recorded, {1} valid, {2} discarded' -f $results.Count, $valid.Count, ($results.Count - $valid.Count))
  Say ''
  Say 'Wedge rate by arm and candidate (valid trials only, Ascii oracle - the comparable one):'
  Say ('  {0,-10} {1}' -f 'candidate', (($ARMS_ALL | ForEach-Object { '{0,-8}' -f $_ }) -join ' ') + ' description')
  $cands = @($CANDIDATES | Where-Object { $Only.Count -eq 0 -or $Only -contains $_.Id })
  foreach ($c in $cands) {
    $cells = @{}
    foreach ($arm in $ARMS_ALL) {
      $set = @($valid | Where-Object { $_.Arm -eq $arm -and $_.Candidate -eq $c.Id -and $_.Oracle -eq 'Ascii' })
      if ($set.Count -eq 0) { $cells[$arm] = '  -  ' }
      else {
        $w = @($set | Where-Object { $_.State -eq 'WEDGED' }).Count
        $cells[$arm] = ('{0}/{1}' -f $w, $set.Count)
      }
    }
    Say ('  {0,-10} {1}' -f $c.Id, (($ARMS_ALL | ForEach-Object { '{0,-8}' -f $cells[$_] }) -join ' ') + ' ' + $c.Desc)
  }
  Say ''

  if ($fingerprints | Where-Object { $_.DeadkeyLayoutOk }) {
    $dkArms = @($ARMS_ALL | Where-Object { $_ -ne 'English' })
    Say 'Same table, Deadkey oracle (Cameroon arms only - sharper, catches NO-OUTPUT too):'
    Say ('  {0,-10} {1}' -f 'candidate', (($dkArms | ForEach-Object { '{0,-8}' -f $_ }) -join ' ') + ' description')
    foreach ($c in $cands) {
      $cells = @{}
      foreach ($arm in $dkArms) {
        $set = @($valid | Where-Object { $_.Arm -eq $arm -and $_.Candidate -eq $c.Id -and $_.Oracle -eq 'Deadkey' })
        if ($set.Count -eq 0) { $cells[$arm] = '  -  ' }
        else {
          $w = @($set | Where-Object { $_.State -ne 'CLEAN' }).Count
          $cells[$arm] = ('{0}/{1}' -f $w, $set.Count)
        }
      }
      Say ('  {0,-10} {1}' -f $c.Id, (($dkArms | ForEach-Object { '{0,-8}' -f $cells[$_] }) -join ' ') + ' ' + $c.Desc)
    }
    Say ''
  }

  # ---- the verdict, stated conservatively --------------------------------
  function ArmWedges([string]$arm) {
    $set = @($valid | Where-Object { $_.Arm -eq $arm -and $_.Oracle -eq 'Ascii' })
    $w   = @($set | Where-Object { $_.State -eq 'WEDGED' }).Count
    return [pscustomobject]@{ Arm=$arm; N=$set.Count; Wedged=$w }
  }
  $sEN = ArmWedges 'English'; $sMS = ArmWedges 'MSKLC'; $sKM = ArmWedges 'Keyman'
  Say ('Arm totals (Ascii): English {0}/{1}   MSKLC {2}/{3}   Keyman {4}/{5}' -f $sEN.Wedged,$sEN.N,$sMS.Wedged,$sMS.N,$sKM.Wedged,$sKM.N)
  Say ''

  # MSKLC is optional, so it is not counted as missing when it was never asked
  # for. English and Keyman are the two the claim actually rests on.
  $required = @('English','Keyman')
  $missing  = @($required | Where-Object { (ArmWedges $_).N -eq 0 })
  $haveMS   = ((ArmWedges 'MSKLC').N -gt 0)

  Say 'VERDICT'
  if ($missing.Count -gt 0) {
    Say ('  [INCONCLUSIVE] no valid trials on: {0}' -f ($missing -join ', '))
    Say  '  The claim needs a Keyman arm and a non-Keyman control arm. Do not quote this run.'
  }
  elseif ($sKM.Wedged -gt 0 -and $sMS.Wedged -eq 0 -and $sEN.Wedged -eq 0) {
    Say  '  [PROOF] The wedge appeared ONLY on the Keyman arm.'
    if ($haveMS) {
      Say ('          Keyman {0}/{1} wedged; MSKLC 0/{2}; English 0/{3}.' -f $sKM.Wedged,$sKM.N,$sMS.N,$sEN.N)
      Say  '          MSKLC clean rules out the layout and rules out Windows dropping the KEYUP:'
      Say  '          same layout, same OS, same stimulus, different implementation.'
      Say  '          English clean rules out this harness manufacturing the phantom Shift.'
    } else {
      Say ('          Keyman {0}/{1} wedged; English 0/{2}. TWO-ARM RUN - no MSKLC layout installed.' -f $sKM.Wedged,$sKM.N,$sEN.N)
      Say  '          English clean rules out this harness manufacturing the phantom Shift, and'
      Say  '          rules out Windows dropping the KEYUP on its own: same OS, same stimulus,'
      Say  '          same injected events, no wedge.'
      Say  '          WHAT THIS RUN DOES NOT SETTLE: with no Microsoft build of the SAME layout'
      Say  '          to compare against, it cannot separate Keyman the ENGINE from the Cameroon'
      Say  '          layout data it loads. Install the MSKLC Cameroon layout for that control.'
    }
  }
  elseif ($sKM.Wedged -eq 0) {
    Say  '  [NOT REPRODUCED] Keyman did not wedge in this run. Nothing is proven either way.'
    Say  '                   Try -LoadThreads 4..6 and a higher -Repeat; the trigger needs the'
    Say  '                   Keyman main thread starved at the wrong instant.'
  }
  else {
    Say  '  [CLAIM FAILS] the wedge appeared on a non-Keyman arm too.'
    if ($sMS.Wedged -gt 0) { Say ('                MSKLC {0}/{1} - so this is NOT Keyman-specific. TRIGGER.md must be corrected.' -f $sMS.Wedged,$sMS.N) }
    if ($sEN.Wedged -gt 0) { Say ('                English {0}/{1} - the HARNESS is suspect; its own SendInput may be creating the phantom Shift.' -f $sEN.Wedged,$sEN.N) }
  }
  Say ''

  $ctrlA = @($valid | Where-Object { $_.Candidate -eq 'A' -and $_.Arm -eq 'Keyman' -and $_.Oracle -eq 'Ascii' })
  if ($ctrlA.Count -gt 0) {
    $aw = @($ctrlA | Where-Object { $_.State -eq 'WEDGED' }).Count
    if ($aw -eq 0) { Say ('  internal control A (no freeze) on Keyman: 0/{0} wedged - consistent with the freeze being the mechanism.' -f $ctrlA.Count) }
    else { Say ('  [WARN] internal control A (NO freeze) wedged {0}/{1} on Keyman. The freeze is then NOT the mechanism and the TRIGGER.md story needs rework.' -f $aw,$ctrlA.Count) }
  }
  foreach ($n in $armNotes)     { Say ('  [NOTE] {0}: {1} ({2})' -f $n.Arm,$n.Note,$n.Detail) }
  foreach ($n in $wedgeCarried) { Say ('  [NOTE] {0}: {1}' -f $n.Arm,$n.Note) }

  if ($results.Count -gt 0) {
    $results | Export-Csv -Path $csvPath -NoTypeInformation -Encoding UTF8
    @{ Stamp=$stamp; Arms=$Arms; Repeat=$Repeat; LoadThreads=$LoadThreads; SwitchMode=$SwitchMode
       Fingerprints=$fingerprints; Results=$results; ArmNotes=$armNotes } |
      ConvertTo-Json -Depth 6 | Set-Content -Path $jsonPath -Encoding UTF8
    Say ''
    Say ("  csv  : {0}" -f $csvPath)
    Say ("  json : {0}" -f $jsonPath)
  }
  Say ("  log  : {0}" -f $log)
  Say '==============================================================='

  # ======================= REGRESSION GATE VERDICT =======================
  # ADDED FOR THE REPO VERSION. Everything above is the original rig's own
  # verdict, which is stated in the polarity of a demonstration. This block
  # states the gate's, which is the inverse, and it is the only thing that sets
  # an exit code.
  if ($Gate) {
    if ($script:GateKeymanCalls -eq 0) {
      Add-GateInconclusive 'no decisive trial ran on the Keyman arm, so the gate measured nothing at all'
    }
    $code = 0; $overall = 'PASS'
    if     ($script:GateFail -gt 0)              { $code = 1; $overall = 'FAIL' }
    elseif ($script:GateIncReason.Count -gt 0)   { $code = 2; $overall = 'INCONCLUSIVE' }

    SayAlways ''
    SayAlways '===================== GH-8064 REGRESSION GATE ====================='
    SayAlways ('  arms run      : {0}' -f ($ARMS_ALL -join ', '))
    SayAlways ('  candidates    : {0}' -f (($CANDIDATES | Where-Object { $Only.Count -eq 0 -or $Only -contains $_.Id } | ForEach-Object { $_.Id }) -join ','))
    SayAlways ('  trials scored : PASS {0}   FAIL {1}   INCONCLUSIVE {2}' -f $script:GatePass,$script:GateFail,$script:GateInc)
    SayAlways ('  decisive trials on the Keyman arm: {0}' -f $script:GateKeymanCalls)

    if ($script:GateFail -gt 0) {
      SayAlways ''
      SayAlways '  FAILING TRIALS - expected vs observed:'
      foreach ($d in $script:GateFailDetail) {
        SayAlways ('    arm {0}, candidate {1}, trial {2}, {3} oracle' -f $d.Arm,$d.Candidate,$d.Pass,$d.Oracle)
        SayAlways ('      expected  : {0}' -f $d.Expected)
        SayAlways ('      observed  : {0}' -f $d.Observed)
        SayAlways ('      wedged form for this oracle: {0}' -f $d.WedgeForm)
        SayAlways ('      modifiers held at readback : {0}' -f $d.Mods)
      }
    }

    if ($script:GateIncReason.Count -gt 0) {
      SayAlways ''
      SayAlways '  UNMET PRECONDITIONS - each of these on its own forbids a PASS:'
      foreach ($r in $script:GateIncReason) { SayAlways ('    - {0}' -f $r) }
    }

    SayAlways ''
    if ($overall -eq 'PASS') {
      SayAlways '  OVERALL: PASS'
      SayAlways '           No arm wedged, and every precondition was confirmed. This is what a build'
      SayAlways '           carrying the GH-8064 fixes is expected to produce. A vanilla build is'
      SayAlways '           expected to FAIL here instead.'
    } elseif ($overall -eq 'FAIL') {
      SayAlways '  OVERALL: FAIL'
      SayAlways '           The Keyman arm wedged under a stimulus the Microsoft control arms took'
      SayAlways '           without wedging. GH-8064 is present in this build.'
    } else {
      SayAlways '  OVERALL: INCONCLUSIVE'
      SayAlways '           A precondition was unmet, so this run proves nothing in either direction.'
      SayAlways '           It is NOT a pass. Clear the preconditions listed above and run it again.'
    }
    SayAlways ('  exit code: {0}' -f $code)
    SayAlways ('  log      : {0}' -f $log)
    SayAlways '=================================================================='
    $script:GateExit = $code
  }
}
finally {
  foreach ($j in $loadJobs) { Stop-Job $j -ErrorAction SilentlyContinue; Remove-Job $j -Force -ErrorAction SilentlyContinue }
}

# The exit lives outside the try/finally so the load runspaces are always torn
# down first. Only -Gate sets a process exit code; without it the script ends
# exactly as the source rig does.
if ($Gate) {
  if ($null -eq $script:GateExit) {
    SayAlways '  OVERALL: INCONCLUSIVE - the run ended before it reached the gate verdict.'
    $script:GateExit = 2
  }
  exit $script:GateExit
}
