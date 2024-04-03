---
title: What's New
---

Here are some of the new features we have added to Keyman for Linux 18.0:

- Keyman for Linux now fully works with the Wayland compositor
  ([#4273](https://github.com/keymanapp/keyman/issues/4273)).
  Previously the full functionality only worked with the X11 display server.
- Slightly modified the UI of `km-config`. The new _Options_ tab allows
  to disable automatic error reporting to keyman.com.
- Added Mon localization ([#9550](https://github.com/keymanapp/keyman/pull/9550)).
- It is now possible to specify an arbitrary language tag when installing
  a keyboard package from the command line ([#8598](https://github.com/keymanapp/keyman/issues/8598)).
  Previously this only worked for the language tags listed in the
  keyboard package.
- Keyman for Linux now supports the upcoming Ubuntu 24.04 Noble LTS in
  addition to Ubuntu 23.10 Mantic, Ubuntu 22.04 Jammy LTS and
  Ubuntu 20.04 Focal LTS. Note: This will be last version that supports
  Ubuntu 20.04 Focal.
- Keyman Core on Linux supports full normalization for LDML keyboard output
  ([#10390](https://github.com/keymanapp/keyman/issues/10390)).
- Renamed the mysterious `libkmnkbp0-0` package to the clearer
  `libkeymancore1`. Cleanup of the API.
