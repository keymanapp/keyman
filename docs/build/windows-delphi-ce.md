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
build.

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

* `KEYMAN_DELPHI_VERSION` tells builder which version of Delphi to target
  (defaults to `20.0`). Without it, Delphi targets are silently skipped
  if Delphi 10.3 is not installed.
* `KEYMAN_DELPHI_CE=1` makes `delphi_msbuild` prompt at each Delphi step:

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
found: jvcl.inc` / `jedi.inc` / `jcl.inc`.

Register them with `devtools -ai`, which writes the IDE **Search** and
**Browsing** paths for both Win32 and Win64 (registry + `EnvOptions.proj`)
in one shot — no manual **Tools → Options** step needed. `devtools` has no
JCL/JVCL dependencies, so it builds before the paths exist: build it first
(one of the CE prompts under `windows/src/global/delphi/build.sh build`),
**close Delphi** (it caches library paths at startup), then register each
path (`-ai` takes one absolute path per call):

```bash
DEVTOOLS="$KEYMAN_ROOT/common/windows/delphi/tools/devtools/bin/Win32/Debug/devtools.exe"
register() { "$DEVTOOLS" -ai "$(cygpath -w "$1")"; }

# The six CLI include paths, read from their single source of truth:
eval "$(grep '^DELPHIINCLUDES=' "$KEYMAN_ROOT/resources/build/win/delphi_flags.inc.sh")"
IFS=';'; for p in $DELPHIINCLUDES; do register "$p"; done; unset IFS

# The IDE-only JCL/JVCL/jedi paths (no build-side equivalent):
for p in \
  developer/src/ext/jedi/jcl/jcl/source/common \
  developer/src/ext/jedi/jcl/jcl/source/prototypes \
  developer/src/ext/jedi/jcl/jcl/source/vcl \
  developer/src/ext/jedi/jcl/jcl/source/windows \
  developer/src/ext/jedi/jcl/jcl/source/include \
  developer/src/ext/jedi/jvcl/jvcl/design \
  developer/src/ext/jedi/jvcl/jvcl/run \
  developer/src/ext/jedi/jvcl/jvcl/common \
  developer/src/ext/jedi/jvcl/jvcl/resources \
  developer/src/ext/jedi/jedi \
  developer/src/ext/jedi ; do
  register "$KEYMAN_ROOT/$p"
done
```

The first block reads the six CLI include paths straight from `DELPHIINCLUDES`
in [`delphi_flags.inc.sh`](../../resources/build/win/delphi_flags.inc.sh), so
they can't drift from the build. The eleven JCL / JVCL / jedi paths are
IDE-only with no build-side equivalent, so they stay listed explicitly.
`devtools` targets the `BDS\<version>` hive matching the Delphi it was built
with (`23.0` under Delphi 12, per `SourceRootPath.pas`), so build it under the
same Delphi you'll open the projects in.

## 4. Building the Delphi projects under CE

The implicit cross-project **build-order dependencies** (codegen `.pas`
files, embedded `.res`, COM registration) are general to all Windows
builds, not CE-specific, so they now live in
[windows.md § Delphi build-order dependencies](windows.md#delphi-build-order-dependencies).
They bite harder under CE because you build each project by hand: accept
the IDE prompts in the order `build.sh` issues them and let none get
skipped.

Run `./build.sh build` in each child directory (or at the repo root
to fan out) with `KEYMAN_DELPHI_CE=1` set — the script drives the
order and prompts you at each Delphi step.

**Install (a separate step).** Once built, overlay the dev binaries and
register `kmcomapi.dll` from an **elevated Git Bash**:

```bash
windows/src/engine/build.sh install
windows/src/desktop/build.sh install
```

### Faster iteration: script + IDE Build All (warm-state only)

`delphi_msbuild` under `KEYMAN_DELPHI_CE=1` **auto-skips its prompt**
when the expected output at `bin/<Platform>/Debug/<project>.{exe,dll,bpl}`
is newer than the corresponding `.dpr` / `.dpk` source. On subsequent
builds — where the previous build's `.res` files, codegen `.pas`
files, and vendored `.bpl`s are all still on disk — this enables:

1. From a terminal, kick off the shell build script for a subsystem
   (e.g. `./windows/src/engine/build.sh build`). The first CE prompt
   fires and blocks.
2. Alt-Tab to Delphi 12 CE. Open the relevant project group:
   * `windows/src/engine/engine.groupproj` — engine children (keyman,
     kmcomapi, tsysinfo, tsysinfox64). Post-PR #16044 `insthelper`
     is not in the group and needs to be opened on its own.
   * `windows/src/desktop/desktop.groupproj` — desktop children
     (kmshell, kmbrowserhost, kmconfig, insthelp, setup).
   * `developer/src/developer.groupproj` — TIKE, kmconvert,
     developer/setup.
3. Check Delphi's Config = **Debug** and Platform = **Win32** (or
   Win64 for the 64-bit half of kmcomapi and for tsysinfox64), then
   **Build All**. Every project in the group compiles once.
4. Alt-Tab back to the terminal, press Enter to release the first
   prompt.
5. Each subsequent `delphi_msbuild` call detects its output is fresh
   and skips the prompt automatically. Post-build steps
   (`sentrytool_delphiprep`, `tds2dbg`, staging `cp`) run against the
   pre-built binaries.

**This flow only works when all Delphi inputs already exist on disk.**
Delphi Build All can't fabricate the inputs the shell scripts
generate:

* `MessageIdentifierConsts.pas` (from `devtools -buildmessageconstants`
  in `windows/src/global/delphi/build.sh`)
* `Keyman.Setup.System.Locale.*.pas` (from `devtools -buildsetupstrings`
  in `windows/src/desktop/setup/build.sh`)
* `Keyman.System.Standards.BCP47*.pas` (from `build_standards_data/build.sh`)
* `keyman_components.bpl`, `common_components.bpl`, `CEF4Delphi.bpl`
  (from their respective `build.sh` scripts)
* `tsysinfo_x64.res` (from tsysinfo/build.sh's tsysinfox64 → copy →
  rc.exe chain)
* `kbd_noicon.res` + `kmcomapi.tlb` (from kmcomapi/build.sh do_build)
* Per-project `version.res` / `manifest.res` (from each `build.sh`'s
  pre-build)

**Fresh clone or after `git clean -fdx`**: you must walk the CE
prompts one-by-one at least once. Each project's `build.sh` fires the
pre-build steps that generate the inputs above; then the CE prompt
fires; you Build in Delphi; script proceeds. **On the next build**
(codegen outputs still present) you can switch to the Build All flow.

If a project's output is missing or older than the source (you
edited a `.dpr`, or Delphi built to Release instead of Debug), the
CE prompt still fires for that project — no risk of silent
stale-output failures.

**Caveat on engine.groupproj even for warm builds**: the group
currently orders `tsysinfo` before `tsysinfox64`, so a bare Build All
on the engine group fails on tsysinfo (missing `tsysinfo_x64.res`)
unless `windows/src/engine/tsysinfo/build.sh` was run at least once
first (which produces the `.res`).
[#16192](https://github.com/keymanapp/keyman/issues/16192) proposes
a reorder + pre-build event that would remove this caveat.

If you prefer per-project prompts (e.g. debugging a single project
in isolation, or a truly fresh clone), skip step 3 above — the
script will prompt for each `.dproj` in dep order and you can Build
them one at a time.

### Verify Configuration + Platform before every IDE build

Delphi's active Configuration (top-of-IDE dropdown) and Platform
persist across projects — if you switched to Release or Win64 while
inspecting an earlier `.dproj`, Delphi opens the next one with those
same settings. The `.dproj`'s `DCC_ExeOutput` interpolates
`$(Platform)` and `$(Config)`, so a Release/Win32 build lands at
`bin/Win32/Release/<project>.exe` — not the `bin/Win32/Debug/`
location the `build.sh` post-build step expects.

Symptom: Delphi's Messages pane reports `Build succeeded`, but the
`build.sh` fails with
`EFOpenError: Cannot open file "...\bin\Win32\Debug\<project>.exe".
The system cannot find the path specified.`

Before Building each project via the CE prompt, confirm the top-of-IDE
dropdown reads **Debug** and **Win32** (or **Win64** for tsysinfox64
and the Win64 half of kmcomapi).

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
Pro.

* **DLLs (kmcomapi, keymanhp):** Run → Parameters → **Host
  Application** = `<Keyman install path>\kmshell.exe`.
* **Long-running processes (kmshell, TSF text service):** Run →
  **Attach to Process**.
* **C++ engine pieces (keyman32, kmtip, mcompile):** Visual Studio →
  Attach to Process; load `.pdb` from `windows/bin/`. Stepping
  between `keyman.exe` (Delphi) and `keyman32.dll` (C++) requires
  `windbg` — out of scope here.

## 7. Troubleshooting

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

`kmcomapi.dll` not registered. From an elevated Git
Bash: `windows/src/engine/kmcomapi/build.sh install`.

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

### `EFOpenError: Cannot open file "...\bin\Win32\Debug\<project>.exe". The system cannot find the path specified.`

Delphi's Config dropdown was on **Release** (or the Platform on
**Win64**) when you Built, so the output landed at
`bin/Win32/Release/<project>.exe` and `sentrytool_delphiprep`
can't find the Debug path. Switch Config to **Debug**, Platform to
**Win32**, Clean + Build. See §4 "Verify Configuration + Platform".

### `mv: cannot stat 'bin/Win64/Debug/<lib>.dll'`

You built kmcomapi (or another multi-platform Delphi package) with
Delphi's Platform dropdown set to Win32 for both CE prompts. The
second prompt is for Win64 — toggle the dropdown between the two
prompts. See §4 "Multi-platform Delphi packages".
