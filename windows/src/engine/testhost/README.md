# Testhost

This project is a debug host for keyman32-ver (and keyman64). It is setup to run a single-threaded test mode of keyman32-ver keystroke processing. It depends on having Keyman correctly installed, but not currently running. Keyboards must be installed as normal.

The primary use of testhost is in interactive debugging of Keyman Core, and intermediate Engine code around Core. The final stage of characters reaching the document  will not be identical to Keyman Engine in normal use, and there may be other interactive limitations as well.

Before trying to run, make sure you have built keyman32-ver in Debug configuration. Testhost will attempt to load keyman32-ver.dll from ../keyman32-ver/bin/Win32/Debug/keyman32-ver.dll, and if that is not present, from ../keyman32-ver/bin/Win32/Release/keyman32-ver.dll, and finally, from a hardcoded "C:\\Program Files (x86)\\Common Files\\Keyman\\Keyman Engine\\keyman32-ver.dll" (which is hacky but probably suffices for now).

The work flow that works well using testhost.
With Visual Studio(VS)
1. Build Keyman32 with Debug Configuration.
2. Build testhost with Debug Configuration.
3. Ensure Keyman is not running and that language associated with keyboard you want to test is not selected in the language bar.
4. Press F5 to start debugging.
5. Now use the language bar to select the language associated with a keyman keyboard.
6. You should now be able to debug both the platform and keyman core code.
7. Once you stop the debug session you will need to go back to step 3 to start a new session. (or step 1 if you are making code changes; you should not need to reboot windows.)
