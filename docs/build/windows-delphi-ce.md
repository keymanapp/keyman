# Build Keyman on Windows with Delphi Community Edition

Delphi CE (11 and 12) blocks CLI `dcc32`, so `build.sh` cannot drive
Delphi builds end-to-end. Setting `KEYMAN_DELPHI_CE=1` switches
`delphi_msbuild` to interactive mode: at each Delphi step the script
pauses and prompts for an IDE build. The surrounding pre/post-build
work (`rc.exe`, manifest generation, codegen, binary copies) still
runs automatically.

The body is written against Delphi 12 Athens CE (currently the only
free tier). Delphi 11 CE works the same way; only the `Studio\22.0\`
path differs.

This doc is a delta on top of [windows.md](windows.md) — read that
first for repository layout, base dependencies, and the standard
build. Delphi 11/12 also needs a small set of source-compat patches
under `common/windows/delphi/`; those are documented in
[windows.md § Delphi requirements](windows.md#windows-platform-dependencies)
and are separate from the CE-specific work below.

## 1. Prerequisites (delta from windows.md)

* **Delphi 12 Athens Community Edition** from
  https://www.embarcadero.com/products/delphi/starter/free-download.
  Installs to `C:\Program Files (x86)\Embarcadero\Studio\23.0\`. During
  install, select **DUnit Unit Testing Frameworks** (Keyman test
  projects require it). Launch the IDE once after install so the
  per-user `BDS\23.0` registry hive gets populated.
* **Keyman 19 (official release)** from https://keyman.com/windows is
  strongly recommended — it populates the Keyman install-path registry
  key that kmshell reads via `TKeymanPaths.KeymanDesktopInstallPath()`,
  which simplifies the debugging setup and gives you a working system
  to overlay dev binaries onto. It's possible to debug individual
  components without an official install, but each component then
  needs its support files located manually.
* **Test signing certificates** — one-time setup. Several `build.sh`
  scripts run `signtool.exe` against test certificates at
  `common/windows/delphi/tools/certificates/`. If the `.pfx` files
  don't exist, the `wrap-signcode` step fails with
  `SignTool Error: File not found: ...keymantest-sha1.pfx`. Generate
  them once via:

  ```bash
  ./common/windows/delphi/tools/certificates/build.sh certificates
  ```

  This runs `makecert` + `pvk2pfx` from the Windows SDK and installs
  two Keyman test-CA root certificates into your current-user cert
  store (via `certutil -user -addstore Root`). To clean up later:
  `certutil -user -delstore Root "Keyman Test CA"` and the SHA1 variant.

## 2. Environment variables

Set both:

```bat
SETX KEYMAN_DELPHI_VERSION 23.0
SETX KEYMAN_DELPHI_CE 1
```

* `KEYMAN_DELPHI_VERSION` (from #16043) tells `build.sh` and the
  builder platform probe to look at `Studio\23.0\` instead of the
  default `Studio\20.0\`. Without it, `win,delphi`-gated targets are
  silently skipped on a Delphi-12-only machine.
* `KEYMAN_DELPHI_CE=1` (this PR) makes `delphi_msbuild` prompt at
  each Delphi step:

  ```
  Delphi CE: CLI compilation is not available.
  Please build <project.dproj> in the Delphi IDE now, then press
  Enter to continue (or Ctrl-C to abort).
  ```

`SETX` is persistent but does not affect the current shell — open a
fresh shell to pick the values up.

> [!CAUTION]
> **Verify these variables are set in every terminal you build from.**
> If `KEYMAN_DELPHI_CE` is unset, `delphi_msbuild` falls through to
> `msbuild.exe` which — on Delphi CE — reports `Build succeeded` with
> a small `Time Elapsed` line but produces **no output**. Downstream
> `cp` / `mv` / `sentrytool_delphiprep` / `mt.exe` steps then fail
> with confusing "file not found" errors on paths where Delphi never
> actually wrote anything. If you see a suspiciously fast Delphi
> build followed by a missing-file error, check this first:
>
> ```bash
> echo "KEYMAN_DELPHI_CE='$KEYMAN_DELPHI_CE' KEYMAN_DELPHI_VERSION='$KEYMAN_DELPHI_VERSION'"
> ```
>
> Both must be set. If either is empty, `export` them in the current
> shell before rerunning any `build.sh`.

## 3. Delphi IDE Library Search Paths

`build.sh` passes `-U/-I/-R` flags to `dcc32` that the IDE never sees.
Without them, opening any Keyman `.dproj` fails with `F1026 File not
found: jvcl.inc` / `jedi.inc` / `jcl.inc`. Register the paths once in
the per-user registry. The first six paths mirror `DELPHIINCLUDES` in
[`resources/build/win/delphi_flags.inc.sh`](../../resources/build/win/delphi_flags.inc.sh);
the remaining eleven JCL / JVCL / jedi paths are IDE-only (the CLI
compiler resolves them via `.dproj` `<UsePackage>` and vendored `.inc`
lookups) and have no equivalent in `delphi_flags.inc.sh`.

**Close Delphi first** — the IDE caches this value at startup.

```powershell
# Elevated PowerShell — back up first
reg export 'HKCU\Software\Embarcadero\BDS\23.0\Library' `
  delphi-library-paths.backup.reg /y

$key32 = 'HKCU:\Software\Embarcadero\BDS\23.0\Library\Win32'
$key64 = 'HKCU:\Software\Embarcadero\BDS\23.0\Library\Win64'

$paths = @(
  'C:\Projects\keyman\keyman\common\windows\delphi\ext\cef4delphi\source',
  'C:\Projects\keyman\keyman\common\windows\delphi\ext\dcpcrypt',
  'C:\Projects\keyman\keyman\common\windows\delphi\ext\jwa\Win32API',
  'C:\Projects\keyman\keyman\common\windows\delphi\ext\sentry',
  'C:\Projects\keyman\keyman\developer\src\ext\mbcolor',
  'C:\Projects\keyman\keyman\developer\src\ext\scfontcombobox',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\common',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\prototypes',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\vcl',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\windows',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\include',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jvcl\jvcl\design',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jvcl\jvcl\run',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jvcl\jvcl\common',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jvcl\jvcl\resources',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jedi',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi'
)

foreach ($key in @($key32, $key64)) {
  $cur = (Get-ItemProperty -Path $key -Name 'Search Path' `
            -ErrorAction SilentlyContinue).'Search Path'
  $merged = (@($cur -split ';') + $paths |
             Where-Object { $_ } |
             Select-Object -Unique) -join ';'
  Set-ItemProperty -Path $key -Name 'Search Path' -Value $merged
}
```

Restore later via `reg import delphi-library-paths.backup.reg`.

Only the **Search Path** is set. Ctrl-click navigation into JCL/JVCL
source and step-into-source debugging need the same paths added to
Browsing Path and Debug DCU Path via **Tools → Options → Language →
Delphi → Library** in the IDE.

## 4. Build order and implicit dependencies

Keyman's repo has several cross-project dependencies not expressed
in `.dproj` / `.groupproj` files — a Delphi tool emits code consumed
by another Delphi project, or one project's `.res` embeds another's
`.exe`. `build.sh` sequences these correctly; under CE you just
accept the IDE prompts in the order the script issues them. Below is
a reference of what each prompt is producing for what dependent:

| # | Producer → Consumer | Failure if skipped |
|---|---------------------|---------------------|
| a | `windows/src/global/delphi/build.sh` → `devtools -buildmessageconstants` → `MessageIdentifierConsts.pas` consumed by keyman.dproj, kmshell.dproj, plus `keyman_components.bpl` → `windows/lib/` | `F1026 File not found: 'MessageIdentifierConsts.pas'` OR `File ... keyman_components.bpl does not exist` |
| a' | `windows/src/desktop/setup/build.sh` → `devtools -buildsetupstrings` → ~32 `Keyman.Setup.System.Locale.<bcp47>.pas` consumed by setup.dproj | `F1026 File not found: 'Keyman.Setup.System.Locale.<bcp47>.pas'` |
| b | `common/windows/delphi/tools/build_standards_data/build.sh` → 5 BCP-47 registry `.pas` files → TIKE.dproj | `F1026 File not found: 'Keyman.System.Standards.BCP47SubtagRegistry.pas'` |
| c | `windows/src/engine/tsysinfo/build.sh` → auto-invokes tsysinfox64 publish, copies exe, runs `rc.exe` → `tsysinfo_x64.res` embedded by tsysinfo.dproj | `F1026 File not found: 'tsysinfo_x64.res'` |
| d | `developer/src/kmcmplib/build.sh build` → `kmcmplib-19.dll` needed by TIKE runtime | `"kmcmplib-19.dll not found"` at runtime |
| e | keyman.dproj → kmshell.dproj runtime | `"Keyman failed to start"` when launching Keyman |
| f | `regsvr32 kmcomapi.dll` (both Win32 and Win64 variants) → kmshell startup | `Class not registered {CF46549D-...}` (`REGDB_E_CLASSNOTREG`) |

All generated `.pas` files are `.gitignored` and must be regenerated
after `git clean -fdx`.

Run `./build.sh build` in each child directory (or at the repo root
to fan out) with `KEYMAN_DELPHI_CE=1` set — the script drives the
order and prompts you at each Delphi step. Then, from an **elevated
Git Bash**: `windows/src/engine/build.sh install` and
`windows/src/desktop/build.sh install` overlay the dev binaries and
register `kmcomapi.dll` for dependency (f).

### Multi-platform Delphi packages (kmcomapi)

`kmcomapi.dproj` gets built **twice** by its `build.sh` — once for
Win32 (`kmcomapi.dll`), once for Win64 (`kmcomapi.x64.dll`, renamed
post-build). Two consecutive CE prompts fire for the same `.dproj`.
Between them, **change Delphi's Platform dropdown**: first prompt →
Win32, second prompt → Win64. If you build both as Win32, the Win64
output isn't produced and the script fails at `mv: cannot stat
'bin/Win64/Debug/kmcomapi.dll'`.

### Full clean before rebuild if you see debug-section errors

`sentrytool_delphiprep` can fail on a partially-built `.dll`/`.exe`
with `ERROR: This executable has a debug section. Not able to update
this file.` This happens when Delphi produced a partial binary with
an already-populated debug section (usually from a prior attempt).
Wipe the project's build state fully:

```bash
rm -rf windows/src/engine/<project>/bin windows/src/engine/<project>/obj
```

Then rerun that project's `build.sh` and rebuild in Delphi from
scratch.

> [!IMPORTANT]
> After an IDE build following any `.res`, manifest, version, or icon
> change: right-click → **Clean, then Build**. An incremental Build
> silently embeds the stale cached `.res`.

## 5. Local-only: uiAccess strip for overlaid keyman.exe

Windows refuses to launch unsigned binaries declaring
`uiAccess="true"` (error 8235). To run an unsigned dev `keyman.exe`,
swap in the pre-existing non-elevated manifest:

```bash
cd windows/src/engine/keyman
./build.sh debug-manifest
```

That copies `debug-manifest.in` over `manifest.in` and regenerates
`manifest.res`. Then Clean + Build `keyman.dproj` in the IDE and
re-run `windows/src/engine/build.sh install` elevated.

Trade-off: keyboard injection into elevated apps stops working under
the debug manifest. Not committed. Revert with
`git checkout -- windows/src/engine/keyman/manifest.in`.

## 6. Debugging

Nothing here is CE-specific — the same debugging setup applies under
Pro. Included as a landing point because there's no dedicated doc
for it yet:

* **DLLs (kmcomapi, keymanhp):** Run → Parameters → **Host
  Application** = `<Keyman install path>\kmshell.exe`.
* **Long-running processes (kmshell, TSF text service):** Run →
  **Attach to Process**.
* **C++ engine pieces (keyman32, kmtip, mcompile):** Visual Studio →
  Attach to Process; load `.pdb` from `windows/bin/`. Stepping
  between `keyman.exe` (Delphi) and `keyman32.dll` (C++) requires
  `windbg` — out of scope here.

## 7. Troubleshooting

Source-compilation errors from Delphi 11/12 compat
(`E2003 Undeclared identifier: 'null'`, `E2029 Declaration expected`,
`E2012 Type of expression must be BOOLEAN`) are addressed by the
source-compat patches described in
[windows.md § Delphi requirements](windows.md#windows-platform-dependencies).

### `This version of the product does not support command line compiling`

CE block on `dcc32`. Set `KEYMAN_DELPHI_CE=1` (§2). If `build.sh`
still tries to invoke Delphi as a transitive dep, build the upstream
tool first or pass `--no-deps`.

### `File ...\windows\lib\keyman_components.bpl does not exist` / `F1026 File not found: MessageIdentifierConsts.pas`

Codegen (a). Run `./windows/src/global/delphi/build.sh build` — that
script builds `keyman_components.dproj` (produces `keyman_components.bpl`
at `windows/lib/`) AND runs `devtools -buildmessageconstants` to
regenerate `MessageIdentifierConsts.pas`. Neither is produced by
`devtools/build.sh` on its own.

### `F1026 File not found: Keyman.Setup.System.Locale.<bcp47>.pas`

Codegen (a'). Run `./windows/src/desktop/setup/build.sh build` —
that script's post-build runs `devtools -buildsetupstrings` to
regenerate the ~32 locale `.pas` files.

### `F1026 File not found: Keyman.System.Standards.BCP47SubtagRegistry.pas`

Codegen (b). Rerun
`common/windows/delphi/tools/build_standards_data/build.sh build`.

### `F1026 File not found: tsysinfo_x64.res`

Dependency (c). Let `windows/src/engine/build.sh` sequence
`tsysinfox64` before `tsysinfo`; don't skip prompts.

### `F1026 File not found: version.res` or `manifest.res`

Pre-build resource compilation didn't run. Re-invoke `build.sh build`
for the project — it runs `build_version.res` / `build_manifest.res`
before the IDE prompt.

### `F1026 File not found: jvcl.inc` / `jedi/jedi.inc` / `jcl.inc`

Library Search Paths not registered. See §3. Close and reopen Delphi
after the registry edit.

### `F2613 Unit 'JvComponentBase' not found`

JVCL `run/` (or `design/`) missing from Library Search Path. See §3.

### `SKApplicationTitle has had a fatal error` on kmshell launch

kmshell can't find the Keyman install-path registry key it reads via
`TKeymanPaths.KeymanDesktopInstallPath()`. Either install Keyman 19
from https://keyman.com/windows so the key is populated, or set up
the required support files manually alongside the dev build.

### `Class not registered {CF46549D-...}` on kmshell launch

`kmcomapi.dll` not registered — dependency (f). From an elevated Git
Bash: `cd windows/src/engine/kmcomapi && ./build.sh install`.

### `Could not find keyman.exe (error=8235)`

Windows blocked an unsigned `uiAccess="true"` binary. See §5.

### Delphi pops up an "Unsupported CEF version" dialog

CEF4Delphi_Binary checkout doesn't match `common/windows/CEF_VERSION.md`.
Not CE-specific — fix per
[windows.md § KEYMAN_CEF4DELPHI_ROOT](windows.md#keyman_cef4delphi_root).

### `Build succeeded... Time Elapsed 00:00:0X.XX` in terminal, then `cp: cannot stat '...'` / `File ... does not exist`

`KEYMAN_DELPHI_CE` isn't set in the shell that ran `build.sh`, so
`delphi_msbuild` invoked `msbuild.exe` directly. On CE that produces
a fake success with no output. See §2 for the shell-verification
step — this is the single most common failure signature in the whole
CE workflow.

### `SignTool Error: File not found: ...keymantest-sha1.pfx`

Test-signing certificates never generated. Run once:
`./common/windows/delphi/tools/certificates/build.sh certificates`
(see §1).

### `ERROR: This executable has a debug section. Not able to update this file.`

`sentrytool_delphiprep` complaining about a residual debug section
from a partial prior build. Fully wipe the project's `bin/` and
`obj/` and rebuild from scratch (see §4 sub-section).

### `mv: cannot stat 'bin/Win64/Debug/<lib>.dll'`

You built kmcomapi (or another multi-platform Delphi package) with
Delphi's Platform dropdown set to Win32 for both CE prompts. The
second prompt is for Win64 — toggle the dropdown between the two
prompts. See §4 "Multi-platform Delphi packages".

### `EVariantTypeCastError: Could not convert variant of type (Null)` in `devtools -ai`

Pre-Delphi-12-fix. Ensure you have the source-compat patches from
[PR #16043](https://github.com/keymanapp/keyman/pull/16043) —
specifically `common/windows/delphi/tools/devtools/DevIncludePaths.pas`
— which handles Delphi 12's empty `<PropertyGroup/>` in `EnvOptions.proj`.
Rebuild devtools in Delphi 12 CE after applying.

### `E2010 Incompatible types: 'TCustFile' and 'TObject'` in `CustomisationStorage.pas`

Delphi 12 Win64 (`dcc64`) specific — same PR #16043 also patches
`windows/src/global/delphi/cust/CustomisationStorage.pas` for this.
Only surfaces when building `kmcomapi.dproj` for Win64.
