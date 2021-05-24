# Keyman Engine for Windows

There are a number of build artifacts that make up the keyman engine. This is a brief description of each artifacts purpose.

## inst - keymanengine.msm

The installation merge module database for the Keyman Engine.

## keyman - keyman.exe

Host application for the Keyman Engine. Shows the Keyman icon, Keyman menu, OSK, and handles language switching etc, as well as running the windows hooks to capture and translate keystrokes.

## keymanx64 - keymanx64.exe

64-bit version of keyman.exe. All UI is in keyman.exe, so this is a thin wrapper around the keyman32 build.

## kmcomapi - kmcomapi.dll | kmcomapi.x64.dll [ kmcomapi.dbg ]

This is the COM API dynamic linked library that will be used by keyman.exe.  The *.dbg is debug build.

## kmrefresh - kmrefresh.x86.exe & kmrefresh.x64.exe

A helper app to refresh Windows language settings, due to a bug in Windows; read ../kmrefresh/kmrefresh.cpp for details.

## kmtip - kmtip.dll | kmtip64.dll [ kmtip.pdb | kmtip64.pdb ]

Implementation of a Text Service for the Text Services Framework (TSF). This implementation is named Keyman TIP.
Architecturally it sits between the TSF Manager and Keyman.exe

The *.pdb is the debug build.

## mcompile - mcompile.exe

A tool to recompile keyboard layouts after install to map to various European Latin script keyboard layouts

## tsysinfo - tsysinfo.exe & tsysinfox64.exe

Diagnostics

