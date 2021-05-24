MsgList installation:

Command:
MsgListerApp /i

Run from an elevated command prompt. If you don't have a symbol store set up, the install process will create a temporary one in %TMP% for the installation duration. Symbols are required to reliably find the offset of the ValidateHWND function and the gpresUser variable to synchronize access to the message queue. If you do have a symbol store, the symbols won't be deleted as I figured you'd probably have a poke around yourself.

The install process copies the MsgLister.sys file to the \System32\drivers directory and creates a driver service also named MsgLister.


Uninstallation:

Command:
MsgListerApp /u

Removes the service and deletes the previously installed MsgLister.sys.


Active System Hooks:

Command:
MsgListerApp /h

Displays a list of all Win32 hooks that are currently active across all sessions, windowstations, and desktops. Basically, all existant hooks.


Active Windows:

Command:
MsgListerApp /L

Displays a list of active windows with their HWND followed by their title. This is mainly convenience functionality so you don't have to break out Spy++ to get a HWND.


Message Listing:

Command:
MsgListerApp <hwnd>

The hwnd argument is, unsuprisingly, the hwnd of the window of interest.

Checks to see if the version of win32k present on the system has been changed since install time. If so, you must reinstall to continue. Otherwise, it starts the driver service and writes any message information to stdout. The driver is then unloaded and the app exits.