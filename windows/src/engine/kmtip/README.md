
## Overview

`kmtip` is an implementation of a Text Service for the Text Services Framework
(TSF). This implementation is named Keyman Text Input Processor (TIP). Architecturally
it sits between the TSF Manager and Keyman.exe.

## Background

### 1. The "Mixed Architecture" Environment

Windows on both amd64(x86-64) and on Arm devices can run applications of differing architectures simultaneously:

On amd64 Devices:
* **Native x64 apps:** (e.g., Windows Explorer, Notepad, Edge).
* **x86 apps (WOW64):** (32-bit legacy apps).

On Arm Devices:
* **Native Arm64 apps:** (e.g., Windows Explorer, Notepad, Edge).
* **Emulated x64 apps:** (e.g., Older desktop applications).
* **Emulated x86 apps:** (32-bit legacy apps).

A Text Input Processor (TIP) is a plugin that gets **loaded directly into the
process space** of the application currently receiving text input. Therefore,
the TIP DLL must match the architecture of the application it is running in.
Setting all this up is different on Windows on Arm and Windows on amd64
devices.


## kmtip.dll and kmtip64.dll - On amd64 Devices

On 64-bit Windows, there is a single registry entry (COM CLSID) for a 64-bit
TIP: **kmtip64.dll**.
`HKLM\SOFTWARE\Classes\CLSID`

For 32-bit applications, there exists a WOW6432Node in the Windows registry
that allows a different DLL to be registered for the same Class ID:
**kmtip.dll**.
`HKLM\SOFTWARE\WOW6432Node\Classes\CLSID`
It is essentially two views of the registry depending on whether
the application is 32-bit or 64-bit.


## kmtiparm64x.dll - Arm64 Devices

### Single Registry Entry Problem for the two 64-bit architectures

On Windows on Arm there is a problem because there is only a single registry
entry for the **64-bit** CLSID. This means it is not possible to register both an
amd64 DLL and an Arm64 DLL, for example **kmtip64.dll** and
**kmtiparm64.dll**. (For 32-bit apps there is still the WOW6432Node that
registers `kmtip.dll`.)

* If we register a standard **Arm64** DLL, x64 apps will crash or fail to load
  it because they cannot load a pure Arm64 binary.
* If we register a standard **x64** DLL, native Arm64 apps will fail to load
  it.

### The Solution: Arm64X

**Arm64X** is a PE (Portable Executable) binary format. It is a dual-architecture
library capable of servicing both native Arm64 applications and emulated x64
applications on Windows on Arm (WoA) devices.

`kmtiparm64x.dll` solves this problem by using the **Arm64X** format. It
contains two complete sets of code within a single file:

1. **Arm64 Code:** Used when the DLL is loaded by a native Arm64 process.
2. **Arm64EC (Emulation Compatible) Code:** Used when the DLL is loaded by an
   x64 process.
   * *Note: Arm64EC code runs at native speed but uses x64 data structures and
     calling conventions, allowing it to interoperate seamlessly with the x64
     host app.*

### Runtime Behavior

When an application loads `kmtiparm64x.dll`:

* The Windows loader automatically detects the process architecture.
* **Automatic Selection:**
  * If the host process is **x64**, Windows exposes the **Arm64EC** section.
    The **Arm64EC** section is built against the **keyman64.dll** Keyman engine,
    causing it to be loaded dynamically.
  * If the host process is **Arm64**, Windows exposes the **Arm64** section. The
    **Arm64** section is built against the **keymanarm64.dll** Keyman engine,
    causing it to be loaded dynamically.

* **Transparency:** The host application is unaware that the binary contains
  code for other platforms; it sees only what it expects

### Build Composition

The `kmtiparm64x.dll` binary is the result of merging two distinct build
targets from the source:

1. **kmtip (arm64)**: The pure native target.
2. **kmtip (arm64EC)**: The hybrid target for x64 compatibility.

### Usage

* **Registration:** This DLL is registered against the single Keyman TIP CLSID
  for 64-bit contexts.
* **x86 Support:** Note that 32-bit (x86) applications still require the
  separate `kmtip.dll` (32-bit x86 build), as they use a completely different
  registry hive (WOW6432Node).
