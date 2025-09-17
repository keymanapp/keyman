---
title: What's New in Keyman 18.0 for Linux
---

Here are some significant changes to Keyman for Linux 18.0:

- Minimum supported Ubuntu version is now 22.04 Jammy LTS.
- Other supported versions are Ubuntu 24.04 Noble LTS and
  Ubuntu 24.10 Oracular. Users that are running an older version of
  Ubuntu will have to continue using an older Keyman version.
- Keyman no longer requires a patched version of ibus as Keyman now uses
  a system service to manage keystroke order (#11535).
- Added support for simulation of AltGr (right Alt) with Ctrl+Alt (#11852)

Keyman for Linux 18.0.241:

- `onboard-keyman` package no longer gets installed automatically when
  installing `keyman`. The reason is that it doesn't work properly with
  Wayland and is not available in the official Debian/Ubuntu repos
  (#14769). It is still possible to install it manually in your package
  manager, or in a terminal window with `sudo apt install onboard-keyman`.
