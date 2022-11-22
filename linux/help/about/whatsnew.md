---
title: What's New
---

Here are some of the new features and major bug fixes we have added to Keyman for Linux 16.0:

* Improve setting context (#7084)
* Improved reordering and backspace behavior in Chrome(ium) and other apps that
  don't support surrounding text (#7079)
    * Note, this currently requires an update to ibus to be installed, pending
      upstream merge and distribution of ibus patches #2440 and 781119be. The
      packages.sil.org and https://launchpad.net/~keymanapp/+archive/ubuntu/keyman
      repos contain the updated ibus versions.
* Now supports Ubuntu 22.10 (Kinetic) (#6975)
* Fix uninstallation when using fcitx5 (#6963)
