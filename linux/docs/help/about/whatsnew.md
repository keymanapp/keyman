---
title: What's New in Keyman 19.0 for Linux
---

Here are some of the new features we have added to Keyman for Linux 19.0:

- Keyboard search is now localized for several languages (#15510)
- Generate a diagnostic report for technical support (#15576)
- About [275 other fixes and changes](https://keyman.com/go/app/whatsnew/linux/19.0)

Known issues:

- `onboard-keyman` package no longer gets installed automatically when
  installing `keyman`. The reason is that it doesn't work properly with Wayland
  and is not available in the official Debian/Ubuntu repos
  ([#14769](https://github.com/keymanapp/keyman/issues/14769)). It is still
  possible to install it manually in your package manager, or in a terminal
  window with `sudo apt install onboard-keyman`.