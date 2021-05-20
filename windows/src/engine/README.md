# Keyman Engine for Windows

There are a number of processes that make up the keyman engine. This is a brief description of each process’s role.

## keyman - keyman.exe

Host application for Keyman Engine. Shows the Keyman icon, Keyman menu, OSK, and handles language switching etc, as well as running the windows hooks to capture and translate keystrokes.

## keymanx64 - keymanx64.exe

64-bit version of keyman.exe. All UI in keyman.exe, so this is a thin wrapper around the keyman32 build.

## kmrefresh - kmrefresh.x86.exe & kmrefresh.x64.exe

A helper app to refresh Windows language settings, due to a bug in Windows; read ../kmrefresh/kmrefresh.cpp for details.

## mcompile - mcompile.exe

A tool to recompile keyboard layouts after install to map to various European Latin script keyboard layouts

## tsysinfo - tsysinfo.exe & tsysinfox64.exe

Diagnostics

