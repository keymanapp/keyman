# Build Keyman on Windows with Delphi 12 Community Edition

> [!WARNING]
> This is a workaround guide for developers using **Delphi 12 Athens Community
> Edition (CE)**. Delphi CE 11+ deliberately blocks command-line `dcc32`
> compilation:
>
> ```
> This version of the product does not support command line compiling
> ```
>
> Because of that, the repo's `build.sh` chain cannot drive Delphi builds
> end-to-end on CE. Everything Delphi must be opened and built in the IDE by
> hand, and the codegen / resource-compilation steps that `build.sh` would
> normally perform must be run manually first. This guide documents that
> workflow plus the local source patches needed to compile against Delphi 12.
>
> If you have a Delphi 10.3 / 10.4 Professional or Enterprise license, follow
> the canonical [windows.md](windows.md) instead; none of the workarounds in
> this file are needed.

## Scope

This guide covers:

1. Prerequisites (Windows + Delphi 12 CE specific notes)
2. One-time environment setup
3. Local-only source patches required for Delphi 12
4. Build order and the implicit cross-project dependencies
5. Helper scripts that paper over what `build.sh` would normally do
6. Running and debugging the dev build alongside an installed Keyman 19
7. Troubleshooting common error messages

The canonical reference for the standard (non-CE) flow is
[windows.md](windows.md). Refer to it for repository layout, the
`KEYMAN_ROOT` / `KEYMAN_CEF4DELPHI_ROOT` / `EMSCRIPTEN_BASE` env vars,
Chocolatey package list, Visual Studio workloads, and Android dependencies.
This document only describes the deltas.

> [!IMPORTANT]
> All patches and registry edits described here are **local-only** and must be
> reverted before opening a pull request. The repo's CI runs against
> Delphi 10.3 and will not accept any of these workarounds upstream.

## 1. Prerequisites

Beyond the [Base](windows.md#base-dependencies) and
[Web](windows.md#web-dependencies) dependencies in `windows.md`, Delphi 12 CE
needs the following:

* **Delphi 12 Athens Community Edition**
  * Download from
    https://www.embarcadero.com/products/delphi/starter/free-download
  * Installs to `C:\Program Files (x86)\Embarcadero\Studio\23.0\` -- note the
    `23.0` version dir, not `20.0` (10.3) or `21.0` (10.4).
  * During the install wizard, select **DUnit Unit Testing Frameworks**
    (required by Keyman test projects).
  * Launch the IDE once after install to complete registration. The first
    launch populates the per-user `BDS\23.0` registry hive used in step 4.
* **Visual Studio 2022 Community** with the C++ native desktop workload, plus
  Windows 10 SDK (10.0.19041.0) and Windows 11 SDK (10.0.26100). See
  [Windows Platform Dependencies](windows.md#windows-platform-dependencies)
  for the full winget command.
* **Keyman 19 (official release)** installed from
  https://keyman.com/desktop. This is *not* optional for Delphi 12 CE
  developers -- the dev kmshell.exe runtime-discovers its install path via
  `TKeymanPaths.KeymanDesktopInstallPath()`, which is hardcoded to
  `C:\Program Files (x86)\Keyman\Keyman Desktop\`. Without the install,
  kmshell crashes at startup with `SKApplicationTitle has had a fatal
  error...` before its main form appears. See section 7 (Running).
* **Keyman Developer (official release)** is recommended in parallel for TIKE
  overlay debugging -- TIKE loads templates and language data from the
  installed Developer support files.
* **7-Zip** (`choco install 7zip`) for extracting the CEF libcef payload.

## 2. One-time setup

### 2.1 Clone the repos

Use the layout from [windows.md](windows.md#repository-paths). This guide
assumes the keyman checkout is at `C:\Projects\keyman\keyman\` (matches the local
machine). Use the same parent directory for the CEF and emsdk checkouts so
the `KEYMAN_CEF4DELPHI_ROOT` / `EMSCRIPTEN_BASE` paths below line up.

```bash
# Git Bash -- use forward slashes via msys
mkdir -p /c/Projects/keyman
cd /c/Projects/keyman
git clone https://github.com/keymanapp/keyman
git clone https://github.com/keymanapp/CEF4Delphi_Binary
git clone https://github.com/emscripten-core/emsdk
```

### 2.2 Environment variables

```cmd
:: Standard Windows cmd / PowerShell -- backslash paths
SETX KEYMAN_ROOT "C:\Projects\keyman\keyman"
SETX KEYMAN_CEF4DELPHI_ROOT "C:\Projects\keyman\CEF4Delphi_Binary"
SETX EMSCRIPTEN_BASE "C:\Projects\keyman\emsdk\upstream\emscripten"
```

`SETX` is persistent but does not affect the current shell -- open a fresh
shell to pick them up. Also add `<keyman-root>\windows\lib` to `PATH` so
Delphi's design-time packages can resolve.

### 2.3 Emscripten

```bash
# Git Bash
cd /c/Projects/keyman/emsdk
emsdk install 3.1.58
emsdk activate 3.1.58
cd upstream/emscripten
npm install
```

### 2.4 node.js via nvm-windows

```cmd
nvm install 20.16.0
nvm use 20.16.0
```

### 2.5 CEF4Delphi_Binary -- switch to the pinned tag and extract libcef

`common/windows/cef-checkout.sh` normally does this, but if you're driving
Delphi from the IDE you'll want to do it once by hand. Some payload files
exceed GitHub's 100 MB limit and ship as `.zip` files inside the repo; they
must be extracted in place.

```powershell
$cefVer = (Get-Content C:\Projects\keyman\keyman\common\windows\CEF_VERSION.md).Trim()
# e.g. 89.0.18

Push-Location $env:KEYMAN_CEF4DELPHI_ROOT
git reset --hard
git clean -fd
git fetch origin "v$cefVer"
git switch "v$cefVer"

if (-not (Test-Path .\libcef.dll)) {
  Get-ChildItem *.zip | ForEach-Object {
    & 'C:\Program Files\7-Zip\7z.exe' x -y $_.FullName
    Remove-Item $_.FullName
  }
}
Pop-Location
```

### 2.6 Install Keyman 19 official

Install Keyman 19.0 from https://keyman.com/desktop. After install verify
`C:\Program Files (x86)\Keyman\Keyman Desktop\kmshell.exe` exists and that
`HKLM\SOFTWARE\WOW6432Node\Keyman\Keyman Engine` is populated. The overlay
workflow in section 7 depends on these.

### 2.7 Delphi 12 IDE Library Search Paths

`build.sh` injects `dcc32 -I/-U/-R` flags that the IDE does not see. Without
them, opening any of the Keyman .dproj files in Delphi triggers `F1026 File
not found: jvcl.inc` / `jedi.inc` / `jcl.inc` / etc.

These paths live in the per-user registry under
`HKCU\Software\Embarcadero\BDS\23.0\Library\Win32` (and `...\Win64`), in the
`Search Path` value, semicolon-separated. **Close Delphi first**; the IDE
caches the value at startup.

```powershell
# Elevated PowerShell -- back up first
reg export 'HKCU\Software\Embarcadero\BDS\23.0\Library' `
  C:\Projects\keyman\keyman\delphi-library-paths.backup.reg /y

$key32 = 'HKCU:\Software\Embarcadero\BDS\23.0\Library\Win32'
$key64 = 'HKCU:\Software\Embarcadero\BDS\23.0\Library\Win64'

$paths = @(
  # Keyman common includes (mirrors DELPHIINCLUDES in delphi_flags.inc.sh)
  'C:\Projects\keyman\keyman\common\windows\delphi\ext\cef4delphi\source',
  'C:\Projects\keyman\keyman\common\windows\delphi\ext\dcpcrypt',
  'C:\Projects\keyman\keyman\common\windows\delphi\ext\jwa\Win32API',
  'C:\Projects\keyman\keyman\common\windows\delphi\ext\sentry',
  'C:\Projects\keyman\keyman\developer\src\ext\mbcolor',
  'C:\Projects\keyman\keyman\developer\src\ext\scfontcombobox',
  # JCL / JVCL source roots (needed for TIKE)
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\common',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\prototypes',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\vcl',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\windows',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jcl\jcl\source\include',   # jcl.inc
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jvcl\jvcl\design',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jvcl\jvcl\run',
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jvcl\jvcl\common',         # jvcl.inc
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jvcl\jvcl\resources',      # JvConsts.res
  'C:\Projects\keyman\keyman\developer\src\ext\jedi\jedi',                     # jedi.inc
  'C:\Projects\keyman\keyman\developer\src\ext\jedi'                           # parent for {$I jedi\jedi.inc}
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

Restore via `reg import C:\Projects\keyman\keyman\delphi-library-paths.backup.reg`.

Note: only the **Search Path** value was set, not Browsing Path or Debug DCU
Path. Ctrl+click navigation to JCL/JVCL source and step-into-source debugging
may need the same paths added there manually via the IDE
(Tools > Options > Language > Delphi > Library).

## 3. Local-only source patches

These patches are required to compile against Delphi 12 (VER360) but are
**not yet upstreamable** -- they coexist with Delphi 10.3 (VER340) by
extending IFDEF chains rather than replacing the older guard. The patches
must be reverted before any PR.

> [!NOTE]
> In some branches `windows/src/engine/engine.groupproj` carries a stale
> reference to `inst\insthelper\insthelper.dproj` -- a path that doesn't
> resolve because the real file is at
> `windows\src\engine\insthelper\insthelper.dproj` (no `inst\` parent).
> If your tree has the broken reference, either patch `engine.groupproj`
> to drop the `inst\` segment or move the file. See section 3.3 and the
> `[[delphi-12-local-patches]]` memory.

### 3.1 Build-script: select Delphi version and enable CE interactive mode

Set both environment variables for Delphi 12 CE:

```powershell
# PowerShell (persistent for your user)
[Environment]::SetEnvironmentVariable('KEYMAN_DELPHI_VERSION', '23.0', 'User')
[Environment]::SetEnvironmentVariable('KEYMAN_DELPHI_CE', '1', 'User')
```

```bash
# Git Bash (per-shell)
export KEYMAN_DELPHI_VERSION=23.0
export KEYMAN_DELPHI_CE=1
```

`KEYMAN_DELPHI_VERSION` selects which Delphi installation `build.sh` looks for
(defaults to `20.0` / Delphi 10.3). Without this override, `build.sh` looks for
`C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\rsvars.bat`, which does not
exist on a Delphi-12-only machine.

`KEYMAN_DELPHI_CE=1` switches `delphi_msbuild` into interactive mode. Instead of
invoking `msbuild.exe`, the script pauses and prompts:

```
Delphi CE: CLI compilation is not available.
Please build <project.dproj> in the Delphi IDE now, then press Enter to continue.
```

This means you use the same `build.sh` commands as a Professional Delphi user; the
scripts orchestrate pre-build steps (rc compilation, codegen) and post-build steps
(binary copying) around your IDE builds automatically.

Both variables are backwards-compatible: when unset or `KEYMAN_DELPHI_CE=0`, all
behavior is identical to the historical Delphi 10.3 CI defaults.

### 3.2 keyman.exe uiAccess strip (LOCAL ONLY, not committed)

This patch is **deliberately not committed** to this branch — it would
change release-signed-binary semantics in a way upstream CI doesn't want.
Apply it manually as a working-tree-only edit if you need to overlay an
unsigned dev `keyman.exe`. Run after a fresh checkout, before building
keyman.dproj:

```powershell
$f = 'C:\Projects\keyman\keyman\windows\src\engine\keyman\manifest.in'
(Get-Content $f) -replace 'uiAccess="true"', 'uiAccess="false"' | Set-Content $f -Encoding UTF8
```

Then re-run `build.sh build` for `windows/src/engine/keyman` (the pre-build
`build_manifest.res` step regenerates manifest.xml / manifest.res before prompting
for the IDE build), do a **Clean + Build** of keyman.dproj in Delphi IDE
(see [[delphi-incremental-build-res-cache]] — Build alone won't re-link the
new manifest.res), and run `windows/src/engine/build.sh install` elevated (section 5.3).

**Why:** Windows refuses to launch any unsigned binary that declares
`uiAccess="true"`, returning `Access is denied` (error 8235). The overlay
workflow (section 7) replaces the signed installed keyman.exe with an
unsigned dev build, so uiAccess must be turned off for the overlaid dev
binary to launch. Trade-off: keyboard injection into elevated apps stops
working until uiAccess is restored and the binary is signed via Keyman's
test-cert pipeline.

To revert (keep your tree CI-clean): `git checkout -- windows/src/engine/keyman/manifest.in`.

### 3.3 engine.groupproj / insthelper path ambiguity

There is a real path ambiguity in the engine tree:

* The actual `insthelper.dproj` lives at
  `windows\src\engine\insthelper\insthelper.dproj`.
* Historical `build.sh` plumbing (and at least one variant of
  `engine.groupproj` carried in some branches) refers to it as
  `inst\insthelper\insthelper.dproj` -- i.e. with an extra `inst\`
  segment that does not resolve.

In the current tree, `engine.groupproj` does not include an `insthelper`
entry at all (the group only builds keyman, kmcomapi, tsysinfo,
tsysinfox64), so "Build All Projects" on the group works -- but
`insthelper.dproj` must then be opened and built on its own. If you encounter
a tree where the groupproj does carry the broken `inst\insthelper\...` path,
the two clean workarounds are:

(a) Patch `engine.groupproj` to drop the `inst\` segment -- change every
    `inst\insthelper\insthelper.dproj` to `insthelper\insthelper.dproj` and
    update the `<Target Name="insthelper">` MSBuild attribute the same way.

(b) Move the directory: `git mv windows/src/engine/insthelper
    windows/src/engine/inst/insthelper` so the existing groupproj path
    resolves. This is invasive and not recommended.

In either case, also add `insthelper` to the `Build` / `Clean` / `Make`
`CallTarget` lists if you want "Build All Projects" to pick it up. See the
GAP note at the top of section 3 about `[[delphi-12-local-patches]]`.

### 3.4 JCL Boolean -> BOOL casts, plus JclWin32 -> Winapi.Windows on CreateMutex

`developer/src/ext/jedi/jcl/jcl/source/common/JclSynch.pas`

Delphi 12 dropped the implicit `Boolean` -> `BOOL` conversion. Every Win32
sync API call must be wrapped:

```pascal
// - Result := CreateEvent(SecAttr, Manual, Signaled, PChar(Name));
// + Result := CreateEvent(SecAttr, BOOL(Manual), BOOL(Signaled), PChar(Name));
```

Call sites that need the cast: `CreateEvent`, `OpenEvent`,
`CreateWaitableTimer`, `OpenWaitableTimer`, `OpenSemaphore`, `CreateMutex`,
`OpenMutex`. Other JCL units likely need the same treatment -- patch
reactively.

The `CreateMutex` site additionally needs a namespace switch -- the BOOL
cast alone is not sufficient. Per `local-delphi-12-patches.patch`:

```pascal
// - FHandle := JclWin32.CreateMutex(SecAttr, InitialOwner, PChar(Name));
// + FHandle := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateMutex(
// +              SecAttr, BOOL(InitialOwner), PChar(Name));
```

The reason is that `JclWin32.CreateMutex` is a `JclWin32`-namespaced
re-declaration that Delphi 12 resolves ambiguously against
`Winapi.Windows.CreateMutex`, producing a "Cardinal vs LongBool" type
mismatch that the BOOL cast can't fix. Routing the call through
`Winapi.Windows.CreateMutex` directly bypasses the ambiguity. The
`{$IFDEF HAS_UNITSCOPE}` guard preserves compatibility with older Delphis
that still use the un-namespaced `Windows` unit.

### 3.5 JVCL `OldCreateOrder` removed in Delphi 11

`developer/src/ext/jedi/jvcl/jvcl/run/JvComponent.pas` (line ~126):

```pascal
// - if OldCreateOrder then
// -   DoCreate;
// + {$IF Defined(VER350) or Defined(VER360)}
// +   DoCreate;
// + {$ELSE}
// +   if OldCreateOrder then DoCreate;
// + {$IFEND}
```

`TCustomForm.OldCreateOrder` was removed in Delphi 11; modern Delphi behaves
as if it were always `True`.

### 3.6 mbcolor: teach it about VER350/VER360

`developer/src/ext/mbcolor/mxs.inc`

Add two new IFDEF blocks mirroring the existing VER340 block:

```pascal
{$ifdef VER350}      // Delphi 11
{$define DELPHI_5_UP}
{$define DELPHI_6_UP}
{$define DELPHI_7_UP}
{$define DELPHI_8_UP}
{$define DELPHI_9_UP}
{$define DELPHI_10_UP}
{$endif}

{$ifdef VER360}      // Delphi 12
{$define DELPHI_5_UP}
{$define DELPHI_6_UP}
{$define DELPHI_7_UP}
{$define DELPHI_8_UP}
{$define DELPHI_9_UP}
{$define DELPHI_10_UP}
{$endif}
```

Without this, mbcolor's `{$IFDEF DELPHI_6_UP}, Variants{$ENDIF}` silently
drops `Variants` from the `uses` clause, causing `E2003 Undeclared
identifier: 'null'` at `HTMLColors.pas:290`.

### 3.7 devtools: map VER350 / VER360 to Studio version

`common/windows/delphi/tools/devtools/SourceRootPath.pas`

The constant is named `DelphiMajorVersion` (not `DelphiVersion`). The existing
baseline (not patched by us) maps VER310->'18.0', VER320->'19.0',
VER330->'20.0', VER340->'21.0'. The patch adds the two newer compilers:

Before (baseline, abridged):

```pascal
{$IFDEF VER310} const DelphiMajorVersion = '18.0';
{$ELSE}{$IFDEF VER320} const DelphiMajorVersion = '19.0';
{$ELSE}{$IFDEF VER330} const DelphiMajorVersion = '20.0';
{$ELSE}{$IFDEF VER340} const DelphiMajorVersion = '21.0';
{$ELSE}
ERROR: must define Delphi version
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
```

After (with VER350 / VER360 inserted before the `ERROR:` fallback):

```pascal
{$IFDEF VER340} const DelphiMajorVersion = '21.0';
{$ELSE}{$IFDEF VER350} const DelphiMajorVersion = '22.0';   // 11  (NEW)
{$ELSE}{$IFDEF VER360} const DelphiMajorVersion = '23.0';   // 12  (NEW)
{$ELSE}
ERROR: must define Delphi version
{$ENDIF}{$ENDIF}{$ENDIF}
```

Note that the VER340->'21.0' baseline reflects upstream's choice to bump the
Studio dir per CE-license rev, not the more conventional 10.3->'20.0'
mapping. Don't "fix" the existing rows -- only append the two new ones.

Without one of these defined, the `$ELSE` branch hits the `ERROR: must
define Delphi version` literal which manifests as `E2029 Declaration
expected`.

### 3.8 Indy UTF-8 URL tripwire

`common/windows/delphi/web/Keyman.System.HttpServer.Base.pas`

Wrap the `ERROR! Check if this is still needed with Delphi update` tripwire
in nested `{$IFNDEF VER340}{$IFNDEF VER350}{$IFNDEF VER360}`. The Indy URL
encoding workaround is still required on 12; just extending the guard
preserves the workaround.

### 3.9 FixedTrackbar tripwire

`common/windows/delphi/components/FixedTrackbar.pas`

Same pattern -- extend the inner `{$MESSAGE ERROR}` guard with
`{$IFNDEF VER350}{$IFNDEF VER360}`.

### 3.10 CleartypeDrawCharacter EnumFontFamiliesEx return type

`common/windows/delphi/general/CleartypeDrawCharacter.pas` (line ~594)

`Winapi.Windows.EnumFontFamiliesEx` returns `Integer` starting with Delphi
10.4. The existing guard only covered VER340; extend it:

```pascal
// - {$IFDEF VER340}
// + {$IF Defined(VER340) or Defined(VER350) or Defined(VER360)}
  if EnumFontFamiliesEx(...) <> 0 then
// - {$ELSE}
// + {$ELSE}
  if EnumFontFamiliesEx(...) then
// - {$ENDIF}
// + {$IFEND}
```

### 3.11 JsonUtil ToChars signature change

`common/windows/delphi/general/JsonUtil.pas` (line ~55)

Delphi 11 added a required `Options: TJSONOutputOptions` parameter to
`System.JSON.TJSONAncestor.ToChars`:

```pascal
{$IF Defined(VER350) or Defined(VER360)}
  obj.ToChars(builder, []);
{$ELSE}
  obj.ToChars(builder);
{$IFEND}
```

### 3.12 Generated `.res` files

The local working tree will also show binary diffs to many `.res` files
under `developer/src/`, `windows/src/desktop/`, `windows/src/engine/`. These
are produced by `rc.exe` during preflight (icons / manifest / version) --
they are *not* patches but should be excluded from any PR. See section 5.

## 4. Build order

Keyman's repo has several cross-project dependencies that are **not**
expressed in `.dproj` or `.groupproj` files -- they're implicit because the
producer is a Delphi tool that emits code consumed by another Delphi
project, or because a Delphi `.res` embeds the output of another Delphi
`.exe`. Doing things in the wrong order produces fatal `dcc32` errors with
non-obvious messages.

### CLI-buildable on Delphi 12 CE

These do not need the IDE:

* **Keyman Core** -- `./core/build.sh` (Rust / C++ via meson+ninja)
* **kmcmplib** -- `./developer/src/kmcmplib/build.sh build` (Rust / C++)
* **TypeScript modules** -- `kmc`, `kmc-kmn`, `kmc-ldml`, `kmc-package`,
  `kmc-keyboard-info`, `kmc-analyze`, `kmc-copy`, `kmc-generate`,
  `kmc-model`, `kmc-model-info` (each has its own `build.sh`)
* **KeymanWeb** -- `./web/build.sh` (TypeScript / Emscripten)
* **C++ engine pieces** -- `windows/src/engine/{keyman32,kmtip,keymanhp,
  kmrefresh,mcompile,testhost}` (msbuild)

Build these via the normal `build.sh` chain. They have no Delphi codegen
dependency.

> [!CAUTION]
> `build.sh` cascades dependencies via `--builder-dep-parent`, so even a
> "non-Delphi" CLI build can pull a Delphi tool in -- which then fails on
> CE with `This version of the product does not support command line
> compiling`. Build the tool by hand in the IDE first, or pass `--no-deps`.

### Delphi-IDE-only on CE

* **All of `windows/src/engine/`** -- `keyman.dproj`, `kmcomapi.dproj`,
  `insthelper.dproj`, `tsysinfo.dproj`, `tsysinfox64.dproj`
  (`engine.groupproj`)
* **All of `windows/src/desktop/`** -- kmshell, kmbrowserhost, kmconfig,
  insthelp, setup (`desktop.groupproj`)
* **All of `developer/src/`** -- TIKE, kmconvert, setup
  (`developer.groupproj`)
* **All Delphi tools in `common/windows/delphi/tools/`** -- devtools,
  build_standards_data, certificates, sentrytool, verify_signatures

### The cross-project dependency graph

The non-obvious dependencies, in the order they must be satisfied:

#### (a) devtools.dproj BEFORE keyman.dproj / setup.dproj / kmshell.dproj

`keyman.dpr` (engine) and `kmshell.dpr` (desktop) both depend on
`MessageIdentifierConsts.pas`, a ~1500-line file generated from
`windows/src/desktop/kmshell/xml/strings.xml` via
`devtools.exe -buildmessageconstants`. The file is `.gitignored` -- it does
not exist in a fresh checkout. Failure mode:

```
F1026 File not found: '...\windows\src\global\delphi\cust\MessageIdentifierConsts.pas'
```

`setup.dpr` depends on ~32 `Keyman.Setup.System.Locale.<bcp47>.pas` files
generated from `windows/src/desktop/setup/locale/*.xml` via
`devtools.exe -buildsetupstrings`.

#### (b) build_standards_data.dproj BEFORE TIKE.dproj

`Keyman.System.LanguageCodeUtils` and friends reference five generated
BCP-47 registry units in `common/windows/delphi/standards/`. The biggest is
`Keyman.System.Standards.LangTagsRegistry.pas` (~2.4 MB). All five are
`.gitignored`. Failure mode:

```
F1026 File not found: '...\Keyman.System.Standards.BCP47SubtagRegistry.pas'
```

#### (c) tsysinfox64.dproj BEFORE tsysinfo.dproj

`tsysinfo.dproj` is Win32 but embeds the Win64 `tsysinfox64.exe` as a
binary resource (`tsysinfo_x64.res`) so it can spawn the 64-bit helper
process at runtime. The default Build All order in `engine.groupproj` is
**keyman, kmcomapi, tsysinfo, tsysinfox64, insthelper** -- so tsysinfo is
attempted before the .exe it embeds exists. Failure mode:

```
F1026 File not found: 'tsysinfo_x64.res'
```

**Right order:**

1. Build `tsysinfox64.dproj` in the IDE (Win64 / Debug).
2. Copy: `Copy-Item windows/src/engine/tsysinfox64/bin/Win64/Debug/tsysinfox64.exe windows/src/engine/tsysinfo/tsysinfox64.bin`
3. From `windows/src/engine/tsysinfo/`: `rc /nologo tsysinfo_x64.rc`
4. Now build `tsysinfo.dproj`.

#### (d) kmcmplib (CLI build) BEFORE TIKE.dproj at runtime

TIKE links to `kmcmplib-19.dll` (the .kmn keyboard compiler). It builds via
`./developer/src/kmcmplib/build.sh build` -- no IDE needed. TIKE compiles
fine without it, but throws "kmcmplib-19.dll not found" at runtime on any
compile action.

#### (e) keyman.dproj BEFORE kmshell.dproj runtime

`kmshell.exe` is the Desktop UI; the actual engine work happens in
`keyman.exe`, which kmshell spawns via the COM `IKeymanController` CLSID.
kmshell compiles without keyman.exe -- the failure is at runtime: the
dev kmshell shows "Keyman failed to start" when enabling a keyboard.

#### (f) regsvr32 kmcomapi.dll BEFORE running kmshell

`kmcomapi.dll` exposes the COM objects kmshell instantiates at startup.
Without registration, `CoCreateInstance` returns `REGDB_E_CLASSNOTREG`
(`Class not registered`) and kmshell dies before its main form appears.
`windows/src/engine/build.sh install` handles this automatically (section 5.3).

### The canonical build order

For a clean checkout, this is the order that actually works:

1. CLI build everything CLI-buildable: `./core/build.sh build`,
   `./developer/src/kmcmplib/build.sh build`, TypeScript modules,
   KeymanWeb. The C++ engine pieces can wait.
2. Build devtools: `cd common/windows/delphi/tools/devtools && ./build.sh build`
   (prompted to build `devtools.dproj` in IDE; press Enter when done).
3. Build build_standards_data: `cd common/windows/delphi/tools/build_standards_data && ./build.sh build`
   (prompted to build `build_standards_data.dproj` in IDE; after Enter, the script
   automatically runs all five codegen invocations).
4. Build engine components: `cd windows/src/engine && ./build.sh build`
   (prompted for each Delphi project in dependency order; `tsysinfo.dproj` is
   prompted after `tsysinfox64.dproj` because `build.sh` handles the dependency).
5. Build desktop components: `cd windows/src/desktop && ./build.sh build`
   (prompted for each desktop Delphi project).
6. Build developer components: `cd developer/src && ./build.sh build`
   (prompted for TIKE, kmconvert, and developer setup).
7. Overlay dev builds (elevated Git Bash): `cd windows/src/engine && ./build.sh install`
   then `cd windows/src/desktop && ./build.sh install` (section 5.3).

## 5. Shell-based CE workflow

With `KEYMAN_DELPHI_CE=1` set (section 3.1), the existing `build.sh` scripts
handle the CE constraint interactively. At each step that would normally invoke
`msbuild` through Delphi, the script pauses and prints:

```
Delphi CE: CLI compilation is not available.
Please build <project.dproj> in the Delphi IDE now, then press Enter to continue (or Ctrl-C to abort).
```

Pre-build steps (rc compilation, manifest generation) and post-build steps
(binary copying, codegen) run automatically around your IDE builds.

### 5.1 Pre-build resource compilation

`build_version.res()` and `build_manifest.res()` use `rc.exe` from the Visual
Studio environment -- they work on CE without modification. They run inside each
project's `build.sh build` before the IDE prompt appears.

> [!IMPORTANT]
> After building in the IDE following a resource change, use **right-click →
> Clean, then Build** in Delphi. An incremental Build silently embeds the stale
> cached `.res`.

### 5.2 Codegen (devtools.exe, build_standards_data.exe)

Build the tools first via their `build.sh` scripts, which prompt for the IDE build
then run codegen automatically:

```bash
# Git Bash (KEYMAN_DELPHI_CE=1 and KEYMAN_DELPHI_VERSION=23.0 must be exported)

# Build devtools: prompted to build devtools.dproj in IDE, then exits
cd "$KEYMAN_ROOT/common/windows/delphi/tools/devtools"
./build.sh build

# Build build_standards_data: prompted for IDE build, then auto-runs all five
# build_standards_data.exe invocations to generate the registry .pas files
cd "$KEYMAN_ROOT/common/windows/delphi/tools/build_standards_data"
./build.sh build
```

The `-buildmessageconstants`, `-buildsetupstrings`, and `-buildlocaleindex` codegen
steps run automatically when you build their dependent projects
(`windows/src/global/delphi`, `windows/src/desktop/setup`, `windows/src/desktop/kmshell`).

All generated `.pas` files are `.gitignored` and must be regenerated after `git clean -fdx`.

### 5.3 Overlay dev builds

From an **elevated** Git Bash (right-click Git Bash → Run as administrator):

```bash
cd "$KEYMAN_ROOT/windows/src/engine"
./build.sh install

cd "$KEYMAN_ROOT/windows/src/desktop"
./build.sh install
```

`windows/src/engine/kmcomapi/build.sh install` re-registers `kmcomapi.dll` via
`regsvr32` automatically.

After overlay, launch Keyman via the Start Menu (or the installed `kmshell.exe`
path) -- not from the dev tree -- so `TKeymanPaths.KeymanDesktopInstallPath()`
finds the support files.

> [!NOTE]
> An alternative workflow -- keep `uiAccess="true"` in `manifest.in` (i.e.
> skip the section 3.2 patch) and skip overlaying `keyman.exe` -- was
> tried and abandoned. With `uiAccess="true"`, the unsigned dev
> `keyman.exe` is refused by Windows (`error=8235`) and the engine never
> starts. The patch + overlay path documented here is the only flow that
> works without a signing cert.

## 6. Running Keyman from the dev tree

Because of `TKeymanPaths.KeymanDesktopInstallPath()`, the dev workflow is:

1. Install Keyman 19 official from keyman.com (once).
2. Build dev binaries via `build.sh build` with `KEYMAN_DELPHI_CE=1` (per section 4).
3. From an elevated Git Bash: `cd windows/src/engine && ./build.sh install`, then
   `cd windows/src/desktop && ./build.sh install` (section 5.3).
4. Launch Keyman from the Start Menu.

If launching keyman.exe (the engine) directly from the dev `bin\` folder
returns `Could not find keyman.exe (error=8235)`, check that section 3.2's
uiAccess patch was applied and that the project was Clean + Built (not just
Built) after patching `manifest.in`.

## 7. Debugging

### 7.1 Run With Host Application

For DLLs (kmcomapi, keymanhp) and projects that don't have a main entry
point: in Delphi, **Run > Parameters > Host Application** =
`C:\Program Files (x86)\Keyman\Keyman Desktop\kmshell.exe` (or
`...keyman.exe`). Set breakpoints in the DLL source, then Run (F9).

### 7.2 Attach to Process

For long-running processes (an already-launched kmshell, the TSF text
service): **Run > Attach to Process**, pick the process, then set
breakpoints. The dev tree's `.dproj` projects emit `.dcu` files with full
debug info into `bin\Win32\Debug\` and `bin\Win64\Debug\`, so as long as
you've overlaid the binary you'll get proper symbols.

### 7.3 What you get -- and don't get -- from a vanilla install

The installed Keyman 19 from keyman.com ships **stripped** PDBs at best;
practically, you'll see only export-table symbols when attaching. The
overlay workflow replaces the installed binaries with dev builds whose
debug info lives next to the source -- after overlay you can step through
Keyman source in the IDE. Without overlay, the IDE will load the source
file when you breakpoint but the line numbers will not match the running
binary.

For C++ pieces (keyman32, kmtip, mcompile), use Visual Studio's Attach to
Process and load the `.pdb` from `windows/bin/`.

## 8. Troubleshooting

### `This version of the product does not support command line compiling`

You're hitting the CE block on `dcc32`. Don't try to drive Delphi from
`build.sh`; build the relevant `.dproj` in the IDE by hand. If `build.sh`
pulled Delphi in as a transitive dep, pass `--no-deps` or build the
upstream Delphi project first.

### `F1026 File not found: 'MessageIdentifierConsts.pas'`

Codegen wasn't run. Build `devtools.dproj` in the IDE, then run section 5.2
step (a).

### `F1026 File not found: 'Keyman.Setup.System.Locale.<bcp47>.pas'` (one of ~32)

Codegen wasn't run. Build `devtools.dproj` then run section 5.2 step (b).
Trailing backslashes on the two args matter.

### `F1026 File not found: 'Keyman.System.Standards.BCP47SubtagRegistry.pas'`

`build_standards_data.exe` wasn't run. Build the `.dproj` then run section
5.2 (BSD block).

### `F1026 File not found: 'tsysinfo_x64.res'`

Group build order issue. Build tsysinfox64 first, copy the exe to
tsysinfo's source dir as `tsysinfox64.bin`, run `rc /nologo tsysinfo_x64.rc`,
then build tsysinfo. See section 4 (c).

### `F1026 File not found: 'version.res'` or `'manifest.res'`

Pre-build resource compilation didn't run. Re-run `build.sh build` for the
project (which calls `build_version.res` / `build_manifest.res` before the IDE
prompt), or invoke `rc.exe` manually from the project directory.

### `F1026 File not found: 'jvcl.inc'` / `'jedi/jedi.inc'` / `'jcl.inc'`

Library Search Paths not in the registry. See section 2.7. Close and
reopen Delphi after editing the registry; the IDE caches the value at
startup.

### `E2010 Incompatible types: 'Cardinal' and 'Boolean'` (JCL)

JCL Boolean -> BOOL casts missing. See section 3.4.

### `E2003 Undeclared identifier: 'OldCreateOrder'` (JVCL)

JvComponent.pas not patched. See section 3.5.

### `E2003 Undeclared identifier: 'null'` (HTMLColors)

mbcolor's `mxs.inc` not patched for VER350/VER360 -- the `Variants` unit
was silently dropped from the `uses` clause. See section 3.6.

### `E2029 Declaration expected` near SourceRootPath.pas

devtools/SourceRootPath.pas hit the `{$ELSE} {$MESSAGE ERROR}` fallback.
See section 3.7.

### `E2012 Type of expression must be BOOLEAN` near EnumFontFamiliesEx

CleartypeDrawCharacter.pas guard not extended for VER350/VER360. See
section 3.10.

### `F2613 Unit 'JvComponentBase' not found`

JVCL `run/` (or `design/`) is missing from Library Search Path. See
section 2.7.

### Delphi pops up an "Unsupported CEF version" dialog

The CEF4Delphi_Binary checkout isn't on the tag in `CEF_VERSION.md`. See
section 2.5. Don't ignore the dialog -- the CEF4Delphi Pascal binding and
the libcef binary must match exactly.

### `SKApplicationTitle has had a fatal error...` on kmshell launch

kmshell can't find its strings.xml / locale files because
`KeymanDesktopInstallPath()` is hardcoded to `Program Files`, and Keyman
19 official isn't installed (or its install support files were removed by
an aborted dev experiment). Install Keyman 19 official from keyman.com.

### `Class not registered ClassID {CF46549D-...}` on kmshell launch

`kmcomapi.dll` isn't registered. From an elevated Git Bash:

```bash
cd "$KEYMAN_ROOT/windows/src/engine/kmcomapi"
./build.sh install
```

This re-registers the DLL automatically. If you want to run `regsvr32` directly:

```powershell
& regsvr32 /s 'C:\Program Files (x86)\Common Files\Keyman\Keyman Engine\kmcomapi.dll'
```

### `Could not find keyman.exe (error=8235)`

Windows blocked launch of an unsigned `uiAccess="true"` binary. Patch
`windows/src/engine/keyman/manifest.in` per section 3.2, Clean + rebuild
`keyman.dproj`, then re-run `windows/src/engine/build.sh install` elevated.

---

## Upstream candidates

If you'd like to PR some of these changes back upstream, the patches split
into two groups based on whether they touch Keyman-owned code or vendored
third-party code.

**Upstreamable (Keyman-owned, additive, CI-safe with `KEYMAN_DELPHI_VERSION`
unset):**

* `resources/build/win/configure_environment.inc.sh` + `delphi_environment.inc.sh`
  -- `KEYMAN_DELPHI_VERSION` env-var override (default preserves 10.3 behavior).
* `resources/build/win/environment.inc.sh` -- `KEYMAN_DELPHI_CE=1` interactive
  mode for `delphi_msbuild` (no-op when unset).
* `common/windows/delphi/tools/devtools/SourceRootPath.pas` -- VER350/VER360
  cases for `DelphiMajorVersion`.
* `common/windows/delphi/web/Keyman.System.HttpServer.Base.pas` -- extend
  VER tripwire to accept VER340/VER350/VER360.
* `common/windows/delphi/components/FixedTrackbar.pas` -- same tripwire
  pattern.
* `common/windows/delphi/general/CleartypeDrawCharacter.pas` -- integer-return
  comparison for `EnumFontFamiliesEx` on VER340+.
* `common/windows/delphi/general/JsonUtil.pas` -- two-arg `ToChars` on
  VER350/VER360 (Delphi 11+ RTL change).
* `windows/src/engine/engine.groupproj` -- drop the broken
  `inst\insthelper\insthelper.dproj` reference.

These could ship as a single "Support compiling under Delphi 11/12" PR
without conflicting with the broader Delphi-removal effort
([keymanapp/keyman#4599](https://github.com/keymanapp/keyman/issues/4599)).

**Local-only (vendored third-party):**

* `developer/src/ext/jedi/jcl/jcl/source/common/JclSynch.pas` -- JCL Boolean
  -&gt; BOOL casts. The right fix is to refresh the bundled JCL from upstream
  (project-jedi.org) when JCL releases a Delphi 12-compatible version.
* `developer/src/ext/jedi/jvcl/jvcl/run/JvComponent.pas` -- JVCL
  OldCreateOrder removal. Same -- pull from upstream JVCL when available.
* `developer/src/ext/mbcolor/mxs.inc` -- third-party mbcolor library.
  Upstream maintenance status unclear.

These should *not* be PR'd upstream-Keyman as-is; the bundled third-party
copies should be refreshed from their respective maintainers instead.

## Reverting before a PR

Before opening a PR, revert all section 3 patches and remove the section
2.7 registry entries. The simplest check:

```bash
git status              # should show only intentional changes
git diff -- '*.res'     # should be empty (preflight .res are local-only)
```

If `git diff` shows any of the files mentioned in section 3, revert them
with `git checkout -- <path>` before pushing.

Restore the original Library Search Path values:

```powershell
reg import C:\Projects\keyman\keyman\delphi-library-paths.backup.reg
```
