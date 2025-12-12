# Keyman Engine for Windows

There are a number of build artifacts that make up the Keyman Engine. This is a brief description of each artifact's purpose.
For FAQs on the architecture see the [Keyman Wiki](https://github.com/keymanapp/keyman/wiki/Keyman-Windows-FAQs).

## inst - keymanengine.msm

The installation merge module database for the Keyman Engine.

## keyman - keyman.exe

Host application for the Keyman Engine. Shows the Keyman icon, Keyman menu, OSK, and handles language switching etc, as well as running the windows hooks to capture and translate keystrokes.

## keyman32 - keyman32.dll | keyman64.dll | keymanarm64.dll

Main keyboard processing library and hook library (x86, x64, arm64).

## keymanmc - keymanmc.dll

Message library for Windows events.

## keymanhp - keymanhp.x64.exe | keymanhp.arm64.exe

64-bit versions of keyman.exe. All UI is in keyman.exe, so this is a thin wrapper around the keyman32 build. One is for x64 CPU architecture and the other Arm64.

In v18.0 and earlier, this was called keymanx64.exe.

## kmcomapi - kmcomapi.dll | kmcomapi.x64.dll

This is the public COM API library, used by keyman.exe, kmshell.exe, and various other Keyman and 3rd party programs.

## kmrefresh - kmrefresh.x86.exe & kmrefresh.x64.exe

A helper app to refresh Windows language settings, due to a bug in Windows; read ../kmrefresh/kmrefresh.cpp for details.

## kmtip - kmtip.dll | kmtip64.dll | kmtiparm64x.dll

Implementation of a Text Service for the Text Services Framework (TSF). This implementation is named Keyman TIP.
Architecturally it sits between the TSF Manager and Keyman.exe

kmtiparm64x.dll is built as an Arm64X file. It supports both x64 and arm64 on a
Windows on Arm machine. kmtip64.dll is therefore only used on Windows x64 Machines.
See the [kmtip README](kmtip/README.md) for deeper description of kmtip*.dll.

## mcompile - mcompile.exe

A tool to recompile keyboard layouts after install to map to various European Latin script keyboard layouts

## testhost - testhost.exe

Test application to allow simple debugging of keystroke processing (not distributed)

## tsysinfo - tsysinfo.exe & tsysinfox64.exe

Diagnostics
