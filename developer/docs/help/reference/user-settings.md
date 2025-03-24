---
title: Keyman Developer User Settings
---

Keyman Developer has settings stored in several locations:

`~/.keymandeveloper/options.json` (`%userprofile%\.keymandeveloper\options.json` on Windows)

: User preferences are stored in this file. In version 16 and earlier, these
  options were stored in the Registry under
  `HKCU\Software\Keyman\Keyman Developer\IDE\Options`. Any options found in
  the Registry will be migrated to the `options.json` file on first start
  of the Keyman Developer IDE.

`HKCU\Software\Keyman\Keyman Developer\IDE`

: Stores settings specific to the IDE, including window locations and
  visibility, recently used projects, font preferences.

`%appdata%\Keyman\Keyman Developer`

: Stores files related to the runtime environment of Keyman Developer, including
  cache files.

`%localappdata%\Keyman\Diag`

: Stores files relating to diagnostics for Keyman Developer; this folder is also
  used for Keyman for Windows diagnostics.
