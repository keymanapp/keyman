This README explains the purpose and architecture of `kmtiparm64x.dll`, for Windows on Arm devices.

***

# kmtiparm64x.dll

## Overview
`kmtiparm64x.dll` is an **Arm64X** PE (Portable Executable) binary. It is a dual-architecture library capable of servicing both native Arm64 applications and emulated x64 applications on Windows on Arm (WoA) devices.

This DLL is needed for the Keyman Text Input Processor (TIP) to function correctly across the different binary architectures of applications running on WoA.

## Background and Problem

### 1. The "Mixed Architecture" Environment
Windows on Arm devices can run applications of differing architectures simultaneously:
* **Native Arm64 apps:** (e.g., Windows Explorer, Notepad, Edge).
* **Emulated x64 apps:** (e.g., Older desktop applications).
* **Emulated x86 apps:** (32-bit legacy apps).

A Text Input Processor (TIP) is a plugin that gets **loaded directly into the process space** of the application currently receiving text input. Therefore, the TIP DLL must match the architecture of the application it is running inside.

### 2. The Single CLSID Problem
On 64-bit Windows, there is typically a single registry entry (COM CLSID) for a 64-bit TIP.
* If we register a standard **Arm64** DLL, x64 apps will crash or fail to load it because they cannot load a pure Arm64 binary.
* If we register a standard **x64** DLL, native Arm64 apps will fail to load it.

We cannot easily register two different DLLs for the same Class ID to handle this "split" 64-bit world.
Note: For 32-bit there exists a WOW6432Node in windows registry that allows different dlls to registerd for the same Class ID.

## The Solution: Arm64X
`kmtiparm64x.dll` solves this by using the **Arm64X** format. It contains two complete sets of code within a single file:

1.  **Arm64 Code:** Used when the DLL is loaded by a native Arm64 process.
2.  **Arm64EC (Emulation Compatible) Code:** Used when the DLL is loaded by an x64 process.
    * *Note: Arm64EC code runs at native speed but uses x64 data structures and calling conventions, allowing it to interoperate seamlessly with the x64 host app.*

### Runtime Behavior
When an application loads `kmtiparm64x.dll`:
* The Windows loader automatically detects the process architecture.
* **Automatic Selection:**
    * If the host process is **x64**, Windows exposes the **Arm64EC** section. The **Arm64EC** section is built against the **keyman64.dll** keyman engine causing it to be loaded dynamically.
    * If the host process is **Arm64**, Windows exposes the **Arm64** section. The **Arm64** section is built against the **keymanarm64.dll** keyman engine causing it to be loaded dynamically.

* **Transparency:** The host application is unaware that the binary contains code for other platforms; it sees only what it expects


## Build Composition
This binary is the result of merging two distinct build targets from the source:
1.  **kmtip (arm64)**: The pure native target.
2.  **kmtip (arm64EC)**: The hybrid target for x64 compatibility.

## Usage
* **Registration:** This DLL is registered against the single Keyman TIP CLSID for 64-bit contexts.
* **x86 Support:** Note that 32-bit (x86) applications still require the separate `kmtip.dll` (32-bit x86 build), as they use a completely different registry hive (Wow6432Node).
