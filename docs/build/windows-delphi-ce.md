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

> [!IMPORTANT]
> Registry edits (§3) and the manifest patch (§5) are local-only.
> Revert before opening any PR — CI runs against Delphi 10.3.

## 1. Prerequisites (delta from windows.md)

* **Delphi 12 Athens Community Edition** from
  https://www.embarcadero.com/products/delphi/starter/free-download.
  Installs to `C:\Program Files (x86)\Embarcadero\Studio\23.0\`. During
  install, select **DUnit Unit Testing Frameworks** (Keyman test
  projects require it). Launch the IDE once after install so the
  per-user `BDS\23.0` registry hive gets populated.
* **Keyman 19 (official release)** from https://keyman.com/desktop.
  Required, not optional: `TKeymanPaths.KeymanDesktopInstallPath()`
  runtime-checks `C:\Program Files (x86)\Keyman\Keyman Desktop\` for
  support files. Without an install, kmshell dies at startup with
  `SKApplicationTitle has had a fatal error` before its main form
  appears.

## 2. Environment variables

Set both, persistently:

```powershell
[Environment]::SetEnvironmentVariable('KEYMAN_DELPHI_VERSION', '23.0', 'User')
[Environment]::SetEnvironmentVariable('KEYMAN_DELPHI_CE', '1', 'User')
```

* `KEYMAN_DELPHI_VERSION` (from #16043) tells `build.sh` and the
  builder platform probe to look at `Studio\23.0\` instead of the
  default `Studio\20.0\`. Without it, `win,delphi`-gated targets are
  silently skipped on a Delphi-12-only machine.
* `KEYMAN_DELPHI_CE=1` (this PR) makes `delphi_msbuild` prompt:

  ```
  Delphi CE: CLI compilation is not available.
  Please build <project.dproj> in the Delphi IDE now, then press
  Enter to continue (or Ctrl-C to abort).
  ```

Open a fresh shell after `SetEnvironmentVariable` for the values to
be visible.

## 3. Delphi IDE Library Search Paths

`build.sh` passes `-U/-I/-R` flags to `dcc32` that the IDE never sees.
Without them, opening any Keyman `.dproj` fails with `F1026 File not
found: jvcl.inc` / `jedi.inc` / `jcl.inc`. Register the paths once in
the per-user registry.

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
`.exe`. Under the standard flow `build.sh` sequences these
invisibly; under CE you must respect them at IDE-prompt time.

| # | Producer → Consumer | Failure if skipped |
|---|---------------------|---------------------|
| a | `devtools.exe -buildmessageconstants` → keyman.dproj, kmshell.dproj | `F1026 File not found: 'MessageIdentifierConsts.pas'` |
| a | `devtools.exe -buildsetupstrings` → setup.dproj | `F1026 File not found: 'Keyman.Setup.System.Locale.<bcp47>.pas'` |
| b | `build_standards_data.exe` → TIKE.dproj | `F1026 File not found: 'Keyman.System.Standards.BCP47SubtagRegistry.pas'` |
| c | `tsysinfox64.exe` → `tsysinfo_x64.res` → tsysinfo.dproj | `F1026 File not found: 'tsysinfo_x64.res'` |
| d | `kmcmplib` (CLI build) → TIKE runtime | `"kmcmplib-19.dll not found"` at runtime |
| e | keyman.dproj → kmshell.dproj runtime | `"Keyman failed to start"` when enabling a keyboard |
| f | `regsvr32 kmcomapi.dll` → kmshell startup | `Class not registered {CF46549D-...}` (`REGDB_E_CLASSNOTREG`) |

All generated `.pas` files are `.gitignored` and must be regenerated
after `git clean -fdx`.

The canonical CE build order:

1. CLI-build everything CLI-buildable: `./core/build.sh build`,
   `./developer/src/kmcmplib/build.sh build`, TypeScript modules,
   `./web/build.sh`. C++ engine pieces can wait.
2. `cd common/windows/delphi/tools/devtools && ./build.sh build`
   (prompted for IDE build; codegen (a) runs after Enter).
3. `cd common/windows/delphi/tools/build_standards_data && ./build.sh build`
   (prompted for IDE; codegen (b) runs after Enter).
4. `cd windows/src/engine && ./build.sh build` — `build.sh` handles
   dependency (c) by building `tsysinfox64.dproj` before
   `tsysinfo.dproj`; just accept prompts in order.
5. `cd windows/src/desktop && ./build.sh build`.
6. `cd developer/src && ./build.sh build`.
7. **Elevated Git Bash:** `cd windows/src/engine && ./build.sh install`,
   then `cd windows/src/desktop && ./build.sh install`. Handles (f)
   automatically.

> [!IMPORTANT]
> After an IDE build following any `.res`, manifest, version, or icon
> change: right-click → **Clean, then Build**. An incremental Build
> silently embeds the stale cached `.res`.

## 5. Local-only: uiAccess strip for overlaid keyman.exe

Windows refuses to launch unsigned binaries declaring
`uiAccess="true"` (error 8235). The overlay workflow (§4 step 7)
replaces the installed signed `keyman.exe` with an unsigned dev
build, so uiAccess must be turned off for the overlay to launch.
**Do not commit:**

```powershell
$f = 'C:\Projects\keyman\keyman\windows\src\engine\keyman\manifest.in'
(Get-Content $f) -replace 'uiAccess="true"', 'uiAccess="false"' | Set-Content $f -Encoding UTF8
```

Then Clean + Build `keyman.dproj` and re-run
`windows/src/engine/build.sh install` elevated.

Trade-off: keyboard injection into elevated apps stops working until
uiAccess is restored and the binary is signed via Keyman's test-cert
pipeline.

Revert with `git checkout -- windows/src/engine/keyman/manifest.in`.

## 6. Debugging

* **DLLs (kmcomapi, keymanhp) and library projects:** Run →
  Parameters → **Host Application** =
  `C:\Program Files (x86)\Keyman\Keyman Desktop\kmshell.exe`.
* **Long-running processes (kmshell, TSF text service):** Run →
  **Attach to Process**. Full symbols work only after overlay (§4
  step 7); without overlay the IDE loads source but line numbers
  won't match.
* **C++ engine pieces (keyman32, kmtip, mcompile):** Visual Studio →
  Attach to Process; load `.pdb` from `windows/bin/`.

## 7. Troubleshooting

Source-compilation errors (`E2010 Cardinal/Boolean`, `E2003 Undeclared
identifier 'OldCreateOrder' / 'null'`, `E2029 Declaration expected`,
`E2012 Type of expression must be BOOLEAN`) are handled by
[PR #16043](https://github.com/keymanapp/keyman/pull/16043) — apply
that PR first.

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

Keyman 19 official isn't installed. Install from
https://keyman.com/desktop.

### `Class not registered {CF46549D-...}` on kmshell launch

`kmcomapi.dll` not registered — dependency (f). From an elevated Git
Bash: `cd windows/src/engine/kmcomapi && ./build.sh install`.

### `Could not find keyman.exe (error=8235)`

Windows blocked an unsigned `uiAccess="true"` binary. See §5.

### Delphi pops up an "Unsupported CEF version" dialog

CEF4Delphi_Binary checkout doesn't match `common/windows/CEF_VERSION.md`.
Not CE-specific — fix per
[windows.md § KEYMAN_CEF4DELPHI_ROOT](windows.md#keyman_cef4delphi_root).

## 8. Reverting before a PR

```bash
git status              # only intentional changes
git diff -- '*.res'     # empty (preflight .res are local)
```

Restore the manifest and Library Search Paths:

```powershell
git checkout -- windows/src/engine/keyman/manifest.in
reg import delphi-library-paths.backup.reg
```
