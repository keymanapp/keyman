# Testhost

This project is a debug host for keyman32 (and keyman64). It is setup to run a single-threaded test mode of keyman32 keystroke processing. It depends on having Keyman correctly installed, but not currently running. Keyboards must be installed as normal.

The primary use of testhost is in interactive debugging of Keyman Core, and intermediate Engine code around Core. The final stage of characters reaching the document  will not be identical to Keyman Engine in normal use, and there may be other interactive limitations as well.

Before trying to run, make sure you have built keyman32 in Debug configuration. Testhost will attempt to load keyman32.dll from ../keyman32/bin/Win32/Debug/keyman32.dll, and if that is not present, from ../keyman32/bin/Win32/Release/keyman32.dll, and finally, from a hardcoded "C:\\Program Files (x86)\\Common Files\\Keyman\\Keyman Engine\\keyman32.dll" (which is hacky but probably suffices for now).
