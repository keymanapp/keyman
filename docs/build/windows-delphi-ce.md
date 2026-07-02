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
build. The source-compat patches Delphi 11/12 need to compile ship
via [PR #16043](https://github.com/keymanapp/keyman/pull/16043); make
sure it's merged or applied before proceeding here.

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

## 3. Delphi IDE Library Search Paths

`build.sh` passes `-U/-I/-R` flags to `dcc32` that the IDE never sees.
Without them, opening any Keyman `.dproj` fails with `F1026 File not
found: jvcl.inc` / `jedi.inc` / `jcl.inc`. Register the paths once in
the per-user registry — they mirror `DELPHIINCLUDES` in
[`resources/build/win/delphi_flags.inc.sh`](../../resources/build/win/delphi_flags.inc.sh),
so if that file gains or drops paths, update this recipe to match.

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
| a | `devtools.exe -buildmessageconstants` → keyman.dproj, kmshell.dproj | `F1026 File not found: 'MessageIdentifierConsts.pas'` |
| a | `devtools.exe -buildsetupstrings` → setup.dproj | `F1026 File not found: 'Keyman.Setup.System.Locale.<bcp47>.pas'` |
| b | `build_standards_data.exe` → TIKE.dproj | `F1026 File not found: 'Keyman.System.Standards.BCP47SubtagRegistry.pas'` |
| c | `tsysinfox64.exe` → `tsysinfo_x64.res` → tsysinfo.dproj | `F1026 File not found: 'tsysinfo_x64.res'` |
| d | `kmcmplib` (CLI build) → TIKE runtime | `"kmcmplib-19.dll not found"` at runtime |
| e | keyman.dproj → kmshell.dproj runtime | `"Keyman failed to start"` when launching Keyman |
| f | `regsvr32 kmcomapi.dll` → kmshell startup | `Class not registered {CF46549D-...}` (`REGDB_E_CLASSNOTREG`) |

All generated `.pas` files are `.gitignored` and must be regenerated
after `git clean -fdx`.

Run `./build.sh build` in each child directory (or at the repo root
to fan out) with `KEYMAN_DELPHI_CE=1` set — the script drives the
order and prompts you at each Delphi step. Then, from an **elevated
Git Bash**: `windows/src/engine/build.sh install` and
`windows/src/desktop/build.sh install` overlay the dev binaries and
register `kmcomapi.dll` for dependency (f).

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

* **DLLs (kmcomapi, keymanhp):** Run → Parameters → **Host
  Application** = `<Keyman install path>\kmshell.exe`.
* **Long-running processes (kmshell, TSF text service):** Run →
  **Attach to Process**.
* **C++ engine pieces (keyman32, kmtip, mcompile):** Visual Studio →
  Attach to Process; load `.pdb` from `windows/bin/`. Note that
  stepping between `keyman.exe` (Delphi) and `keyman32.dll` (C++) is
  not straightforward — `windbg` can bridge but is out of scope here.

## 7. Troubleshooting

Source-compilation errors from Delphi 11/12 compat
(`E2003 Undeclared identifier: 'null'`, `E2029 Declaration expected`,
`E2012 Type of expression must be BOOLEAN`) are addressed by
[PR #16043](https://github.com/keymanapp/keyman/pull/16043) — make
sure that PR is applied.

### `This version of the product does not support command line compiling`

CE block on `dcc32`. Set `KEYMAN_DELPHI_CE=1` (§2). If `build.sh`
still tries to invoke Delphi as a transitive dep, build the upstream
tool first or pass `--no-deps`.

### `F1026 File not found: MessageIdentifierConsts.pas` / `Keyman.Setup.System.Locale.<bcp47>.pas`

Codegen (a). Ensure devtools was built via `build.sh build` (not just
opened in the IDE) — the wrapper is what runs codegen after the IDE
prompt.

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
