# Windows Acceptance Tests

**TEST_61**

feat(windows): add right modifier included in hotkey optional functionality #12259(TEST_LANGUAGE_HOTKEYS_LEFT_SIDE)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `61`
- Product: `Windows`
- Source files: `61.JSON`, `61.html`

</details>

**Description**

Original TestLodge ID: TC295

<details>
<summary>Setup</summary>

1. Install Keyman's latest stable build from the Keyman.com site.(or state.keyman.com)
2. Install Notepad (64-bit).exe

</details>

<details>
<summary>Action</summary>

1. Open the "Keyman configuration" dialog by pressing the icon from the Windows tray.
2. Verify the "Keyman configuration" dialog appears.
3. Navigate to the "Keyboard Layouts" tab.
4. Installed the Tamil99 and IPA SIL keyboards by clicking the "Download keyboard" button.
5. Verify the keyboards appear in the "Keyboard Layouts" tab.
6. Navigate to the "HotKeys" tab
7. Set the left side of the CTRL+G key to the Tamil keyboard.
8. Set the left side of the CTRL_Shift+I key to the IPA SIL keyboard.
9. Verify the key appears on the assigned keyboard.
10. Open the Notepad application.
11. Press the left side of the CTRL+G key in the Notepad app.
12. Verify the keyboard switch to the Tamil keyboard.
13. Enter some letters using the Tamil keyboard.
14. Verify the Tamil text appears in the notepad app.
15. Press the left side of the CTRL_Shift+I key in the note app.
16. Verify the keyboard switch from Tamil to IPA SIL keyboard.
17. Enter some IPA letters using the IPA SIL keyboard.
18. Verify the IPA text appears in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the search bar (in the taskbar)
2. Verify that the ‘Control Panel’ appears in the menu list.
3. Click the Control Panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_309**

Handle keyboard package not downloaded

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `309`
- Product: `Windows`
- Source files: `309.JSON`, `309.html`

</details>

**Description**

TC - fix(windows): handle keyboard package not downloaded #12948

<details>
<summary>Setup</summary>

1.    Install the latest version of Keyman.
2.    Install the older version of any Keyman keyboard (e.g. Gff_Amharic 3.1.1 version)

</details>

<details>
<summary>Action</summary>

1.    Start the Keyman.
2.    Open the Keyman Configuration dialog.
3.    Navigate to the "Keyboard Layouts tab".
4.    Verified that the installed keyboard shows an older version.
5.    Navigate to the "Update" tab.
6.    Verified that the "Apply update now" button appeared disabled.
7.    Verified that the "Check for new updates" button appeared enabled.
8.    Press the "Check for new updates" button.
9.    Verified that the newer version appeared in the "Updated component" column for GFF's Keyboard and Keyman.
10.    Verified that the "Apply update now" button appeared in the enabled state.
11.    Click the "Apply update now" button to upgrade the Keyman version.
12.    Verified that the "Keyman update" dialog appeared.
13.    Click the "Update now" button on the dialog.
14.    Verified that the "Do you want to allow this app to make changes to your device?" dialog appeared
15.    Click the "Yes" button on that dialog.
16.    Verified that no crash appeared on the Keyman configuration dialog.
17.    Verified that the "Keyman Setup" dialog appeared.
18.    Click the "Yes" button on the "Keyman Setup" dialog.
19.    Verified that the machine restarted.
20.    Login to the machine.
21.    Again, verified that the "Do you want to allow this app to make changes to your device?" dialog appeared.
22.    Click the "Yes" button.
23.    Open the "Keyman Configuration".
24.    Navigate to the "Update" tab.
25.    Press the "Check for new updates" button.
26.    Verified that no update appears for the keyboard.
27.    Navigate to the "Keyboard Layouts tab".
28.    Verified that the Keyman keyboard updated with the latest version.

</details>

<details>
<summary>Cleanup</summary>

1.    Type ‘Control panel’ in the search bar (in the task bar)
2.    Verify that the ‘Control Panel’ appears in the menu list.
3.    Click the Control Panel option.
4.    Verify that the Control Panel dialog appears.
5.    Click the ‘Uninstall a Program’ option.
6.    Verify that the Programs and Features dialog box opens.
7.    Select the installed Keyman version from the list.
8.    Click the Uninstall option.
9.    Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_304**

kmshell switch handling for the installing state_TEST_INSTALL_UPDATE

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `304`
- Product: `Windows`
- Source files: `304.JSON`, `304.html`

</details>

**Description**

TC - feat(windows): kmshell switch handling for the installing state_TEST_INSTALL_UPDATE #12956

<details>
<summary>Setup</summary>

1.    Install the older version of Keyman.
2.    Install the older version of any Keyman keyboard (e.g. Gff_Amharic 3.1.1 version)

</details>

<details>
<summary>Action</summary>

1.    Start the Keyman.
2.    Open the Keyman Configuration dialog.
3.    Navigate to the "Keyboard Layouts tab".
4.    Verified that the installed keyboard shows an older version.
5.    Navigate to the "Update" tab.
6.    Open the "C:\Users"yourusername"\AppData\Local\Keyman\UpdateCache" folder.
7.    Open the regedit window (Computer\HKEY_CURRENT_USER\SOFTWARE\Keyman\Keyman Engine).
8.    WinR type regedit Go the current user key update state
9.    Press F5 to keep it refreshing it.
10.    The state will go to usDownloading to usInstalling at the same time keep checking the UpdateCache folder.
11.    Select the "update state" under the "Keyman Engine" in the regedit window.
12.    In Keyman Configuration: press the "Check for new updates" button.
13.    Verified that the "UpdateCache" folder is updating or adding files
14.    On Regedit window: Verified that the state is changes from usUpdateAvailable--> usDownloading --> usWaitingRestart
15.    Click the "Apply update now" button.
16.    Verified that the "Keyman Update" popup
17.    Click the "Update" button.
18.    Verified that the "UpdateCache" folder has the cache.json and the keyboard gff_amharic. and a Keyman.version.exe if available.
19.    On Regedit window: the state is changing to "usldle"
20.    Restart the machine by clicking the "Yes" button on the "Keyman setup" dialog.
21.    Login to the machine.
22.    Click ˘the "Yes" button on the "Do you want to allow to make the change?" dialog.
23.    Keyman Installation is completed successfully.
24.    Open the configuration window.
25.    On the Keyboard Layouts: Verified that the gff_amharic keyboard is updated.
26.    On the Update Tab: Verified there are no updates available.

</details>

<details>
<summary>Cleanup</summary>

1.    Type ‘Control panel’ in the search bar (in the task bar)
2.    Verify that the ‘Control Panel’ appears in the menu list.
3.    Click the Control Panel option.
4.    Verify that the Control Panel dialog appears.
5.    Click the ‘Uninstall a Program’ option.
6.    Verify that the Programs and Features dialog box opens.
7.    Select the installed Keyman version from the list.
8.    Click the Uninstall option.
9.    Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_46**

KMX_PROCESSOR_COMPLIANT_TEST_FRAME_KEY_RESET_MARKERS_GROUP_WINDOWS: WordPad

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `46`
- Product: `Windows`
- Source files: `46.JSON`, `46.html`

</details>

**Description**

Original TestLodge ID: TC232

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the WordPad App icon from Windows Desktop.
2. Verify that the WordPad Application opens.
3. Press `
4. Verify it produces `
5. Press the "right arrow"
6. Type e
7. Verify it produces `e

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Windows

</details>

---

**TEST_48**

KMX_PROCESSOR_COMPLIANT_TEST_SCROLLLOCK_KEY_NO_RESET_GROUP_WINDOWS: WordPad

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `48`
- Product: `Windows`
- Source files: `48.JSON`, `48.html`

</details>

**Description**

Original TestLodge ID: TC234

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the WordPad App icon from Windows Desktop.
2. Verify that the WordPad Application opens.
3. Press `
4. Verify it produces `
5. Press the "scroll lock"
6. Type e
7. Verify it produces è

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Windows

</details>

---

**TEST_41**

KMX_PROCESSOR_NON_COMPLIANT_TEST_CONTROL_1_GROUP_WINDOWS: TextEditor

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `41`
- Product: `Windows`
- Source files: `41.JSON`, `41.html`

</details>

**Description**

Original TestLodge ID: TC218

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install TextEdit app.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from Windows Desktop.
2. Verify that the TextEdit Application opens.
3. Press `
4. Verify it produces `.
5. Type e
6. Verify it produces è

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Windows
2. Uninstall the TextEdit app.

</details>

---

**TEST_43**

KMX_PROCESSOR_NON_COMPLIANT_TEST_FRAME_KEY_RESET_NOMARKERS_GROUP_WINDOWS: TextEditor

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `43`
- Product: `Windows`
- Source files: `43.JSON`, `43.html`

</details>

**Description**

Original TestLodge ID: TC220

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install TextEdit app.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from Windows Desktop.
2. Verify that the TextEdit Application opens.
3. Press 1 \  \
4. Verify it produces  1 \ \
5. Press the "right arrow"
6. Type 2
7. Verify it produces 1//2

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Windows
2. Uninstall the TextEdit app.

</details>

---

**TEST_56**

LDML_PROCESSOR_COMPLIANT_TEST_FRAME_KEY_RESET_NO_MARKERS_WINDOWS: WordPad

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `56`
- Product: `Windows`
- Source files: `56.JSON`, `56.html`

</details>

**Description**

Original TestLodge ID: TC261

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install WordPad app.

</details>

<details>
<summary>Action</summary>

1. Click on the WordPad App icon from Windows Desktop.
2. Verify that the WordPad Application opens.
3. Press a
4. Verify it produces a
5. Press the "right arrow"(->)
6. Press the SHIFT + 8
7. Verify it produces “Star”

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Windows
2. Uninstall the WordPad app.

</details>

---

**TEST_58**

LDML_PROCESSOR_COMPLIANT_TEST_MODIFIER_TAP_NO_RESET_GROUP_WINDOWS: WordPad

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `58`
- Product: `Windows`
- Source files: `58.JSON`, `58.html`

</details>

**Description**

Original TestLodge ID: TC263

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install WordPad app.

</details>

<details>
<summary>Action</summary>

1. Click on the WordPad App icon from Windows Desktop.
2. Verify that the WordPad Application opens.
3. Press ^
4. Verify it produces ^
5. Press and Release CTRL key.
6. Type e
7. Verify it produces ê

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Windows
2. Uninstall the WordPad app.

</details>

---

**TEST_51**

LDML_PROCESSOR_NON_COMPLIANT_TEST_FRAME_KEY_RESET_NO_MARKERS_GROUP_WINDOWS: TextEditor

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `51`
- Product: `Windows`
- Source files: `51.JSON`, `51.html`

</details>

**Description**

Original TestLodge ID: TC246

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install TextEdit app.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from Windows Desktop.
2. Verify that the TextEdit Application opens.
3. Press a
4. Verify it produces a
5. Press the "right arrow"(->)
6. Press the SHIFT + 8
7. Verify it produces a*

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Windows
2. Uninstall the TextEdit app.

</details>

---

**TEST_53**

LDML_PROCESSOR_NON_COMPLIANT_TEST_MODIFIER_TAP_NO_RESET_GROUP_WINDOWS: TextEditor

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `53`
- Product: `Windows`
- Source files: `53.JSON`, `53.html`

</details>

**Description**

Original TestLodge ID: TC248

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install TextEdit app.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from Windows Desktop.
2. Verify that the TextEdit Application opens.
3. Press ^
4. Verify it produces ^
5. Press and Release CTRL key.
6. Type e
7. Verify it produces ê

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Windows
2. Uninstall the TextEdit app.

</details>

---

**TEST_38**

Test case for (windows): Chrome 86.0 no longer accepts backspace #3698,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `38`
- Product: `Windows`
- Source files: `38.JSON`, `38.html`

</details>

**Description**

Original TestLodge ID: TC205

<details>
<summary>Setup</summary>

1. Install Keyman's latest Stable build from the Keyman.com site.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Open Chrome browser.
2. Set Keyman keyboard.
3. Verify the ‘English - EuroLatin(SIL)’ appeared on the system keyboard.
4. Click the Search bar in the Chrome browser.
5. Type ‘Keyman Testing’ in the Search bar.
6. Press Backspace key.
7. Verify it should delete the character to the left of the insertion point.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Taskbar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the Keyman Uninstallation was completed successfully

</details>

---

**TEST_17**

Test case for Caps (CapsOnly-2),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `17`
- Product: `Windows`
- Source files: `17.JSON`, `17.html`

</details>

**Description**

Original TestLodge ID: TC17

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com Site.
2. Install Shift_frees_caps Keyboard.
3. CapsLock is currently off
4. Currently active keyboard is shift_frees_caps keyboard

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Press CapsLock key.
4. Type 2
5. Verify that the caps lock indicator turned on
6. Verify that the output should show  ‘pass.’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_11**

Test case for Caps Always off (Capsoff-1 ),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `11`
- Product: `Windows`
- Source files: `11.JSON`, `11.html`

</details>

**Description**

Original TestLodge ID: TC11

<details>
<summary>Setup</summary>

1. Install Keyman latest stable build from Keyman.com Site.
2. Install caps_always_off keyboard.
3. CapsLock is currently off

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click ‘capsalwaysoff’ keyboard.
6. Verify that the CapsalwaysOff icon appears in the taskbar.
7. Type ‘a’ in the Notepad.
8. Verify that the result should show ‘ncaps_little_a’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_18**

Test case for Caps doesn’t toggle (CapsOnly-3),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `18`
- Product: `Windows`
- Source files: `18.JSON`, `18.html`

</details>

**Description**

Original TestLodge ID: TC18

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.
2. Install Shift_frees_caps Keyboard.
3. CapsLock is currently off
4. Currently active keyboard is shift_frees_caps keyboard

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop
2. Verify that the Notepad app opens
3. Press CapsLock key.
4. Release CapsLock key.
5. Type 6
6. Verify that the Caps lock indicator is turned ON.
7. Verify that the output should show the result  ‘pass.’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_12**

Test case for Caps lock stays off (capsoff-2),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `12`
- Product: `Windows`
- Source files: `12.JSON`, `12.html`

</details>

**Description**

Original TestLodge ID: TC12

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com Site.
2. Install caps_always_off Keyboard.
3. CapsLock is currently off

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Select Capsalwaysoff keyboard.
6. Verify that the Capsalwaysoff keyboard icon appears in the taskbar.
7. Press CapsLock key.
8. Type ‘a’
9. Verify that the caps lock indicator stays turned off.
10. Verify that the result should show  ‘ncaps_little_a’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_14**

Test case for Caps lock while holding capslock key (capsoff-4),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `14`
- Product: `Windows`
- Source files: `14.JSON`, `14.html`

</details>

**Description**

Original TestLodge ID: TC14

<details>
<summary>Setup</summary>

1. Install Keyman latest  build from Keyman.com Site.
2. Install Caps_always_off Keyboard.
3. CapsLock is currently off

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click Capsalwaysoff keyboard.
6. Verify that the Capsalwaysoff keyboard icon appears in the taskbar.
7. Press ’ CapsLock Key + Shift Key + a ‘  keys.
8. Verify that output should show ‘ncaps_shift_A’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_8**

Test case for Capslock ignored for numbers (capslock-3),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `8`
- Product: `Windows`
- Source files: `8.JSON`, `8.html`

</details>

**Description**

Original TestLodge ID: TC08

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com Site.
2. Install Capslock Keyboard.
3. CapsLock is currently on
4. Currently active keyboard is the CapsLock Keyboard

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Press Shift + 3 keys.
4. Verify that the text ‘pass’ should appear on the Notepad.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_36**

Test case for DEAD_CONTEXT_FRAME_KEY(#10061),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `36`
- Product: `Windows`
- Source files: `36.JSON`, `36.html`

</details>

**Description**

Original TestLodge ID: TC187

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com site.
2. Install store_context Keyboard.
3. Install 045 - deadkey and context Keyboard.
4. Install Text Editor (64-bit).exe
5. Install WordPad

</details>

<details>
<summary>Action</summary>

1. Open the TextEditor (64-bit) on the desktop.
2. Verify that the TextEditor (64-bit) app opens
3. Click 045 - deadkey and context keyboard from the Keyman menu.
4. Verify that the 045 - deadkey and context keyboard icon appears in the task bar.
5. Type y z
6. Press
7. Press
8. Press
9. Verify the output results   "?" should appear in the TextEditor app.
10. Open the WordPad on the desktop.
11. Verify that the WordPad app opens
12. Click 045 - deadkey and context keyboard from the Keyman menu.
13. Verify that the 045 - deadkey and context keyboard icon appears in the task bar.
14. Type ‘y z’
15. Press
16. Press
17. Press
18. Verify the output results "?" should appear in the WordPad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_34**

Test case for DEADKEY_AND_CONTEXT, (#10061),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `34`
- Product: `Windows`
- Source files: `34.JSON`, `34.html`

</details>

**Description**

Original TestLodge ID: TC34

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com site.
2. Install store_context Keyboard.
3. Install 045 - deadkey and context Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click 045 - deadkey and context keyboard from the Keyman menu.
6. Verify that the 045 - deadkey and context keyboard icon appears in the task bar.
7. Type y z Shift  +   /
8. Verify the output results  "correct" should appear in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_21**

Test case for Double_Processing_Firefox(TSF),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `21`
- Product: `Windows`
- Source files: `21.JSON`, `21.html`

</details>

**Description**

Original TestLodge ID: TC21

<details>
<summary>Setup</summary>

1. Install Keyman Developerlatest  Stable build from Keyman.com Site.
2. Install Firefox browser.
3. Download and install  EuroLatin (SIL) Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double click on the Keyman Developer icon from the Desktop.
2. Verify the Keyman Developer window opens.
3. Click Project from the menu bar.
4. Verify that the project Submenu opens.
5. Click the ‘Open Project…’ option.
6. Verify that the Open dialog box opens.
7. Select ‘gff_amharic’ Source file.
8. Click the Open button.
9. Verify that the gff_amharic project opens in the Keyman Developer window.
10. Click the Keyboards tab.
11. Click the EuroLatin(SIL) blue link button.
12. Verify that EuroLatin(SIL) .kmn dialog opens.
13. Click the build tab.
14. Verify that there is a panel ‘Web and Mobile Targets’ appearing on the screen.
15. Click the ‘Test Keyboard on Web’ button.
16. Verify that there should be some web addresses that include ‘http://localhost:8008’ appears in the web address pane.
17. Click ‘http://localhost:8008’ from the pane.
18. Click the ‘Open in browser’ button.
19. Verify the Keyman Developer Keyboard Test Site opens in the Firefox browser.
20. Click the Keyboard dropdown list.
21. Select the EuroLatin Keyboard
22. Verify that the Keyboard has been changed to the EuroLatin Keyboard.
23. Type “a b c d Backspace” in the text area.
24. Verify the output should show abc in the text area.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version  from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_23**

Test case for Double_Processing_Notepad (TSF),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `23`
- Product: `Windows`
- Source files: `23.JSON`, `23.html`

</details>

**Description**

Original TestLodge ID: TC23

<details>
<summary>Setup</summary>

1. Install Keyman Developer latest  Stable build from Keyman.com Site.
2. Install Keyman latest build.
3. Download and install  EuroLatin (SIL) Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Select the EuroLatin Keyboard
6. Type a b c d in the Notepad.
7. Verify the output should show  ‘abcd’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_22**

Test case for Double_Processing_Searchbar(TSF),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `22`
- Product: `Windows`
- Source files: `22.JSON`, `22.html`

</details>

**Description**

Original TestLodge ID: TC22

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com Site.
2. Install Firefox browser.
3. Download and install  EuroLatin (SIL) Keyboard.

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the taskbar.
2. Verify that the Keyman menu opens.
3. Click EuroLatin (SIL) Keyboard.
4. Verify that the EuroLatin Keyboard icon appears in the taskbar.
5. Click the Windows Search bar.
6. Type a b c d backspace in the windows search bar.
7. Verify that the output should show ‘abc’ in the windows search bar.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_25**

Test case for IMSAMPLE_BACKSPACE:

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `25`
- Product: `Windows`
- Source files: `25.JSON`, `25.html`

</details>

**Description**

Original TestLodge ID: TC25

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com Site.
2. Install imsample keyboard.
3. In the Computer\HKEY_CURRENT_USER\SOFTWARE\Keyman\Keyman Engine\Active Keyboards\imsample, create New > DWORD(32-bit) key.
4. Rename ‘ShowIMWindow’ to the new DWORD key.
5. Set the Key value to 0.
6. Create New>DWORD(32-bit) key.
7. Rename ‘ShowIMWindowAlways’ to the new DWORD key.
8. Set the Key value to 0
9. Open Notepad.
10. Type ‘he’ in the text area.
11. Verify the Output should show h[1ɛ 2ɜ 3ə 4e 5ɘ].
12. Type number 3.
13. Verify that the displayed output should show ‘hə’

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click IMTest Keyboard from the Keyman menu.
6. Verify that the Keyboard icon appears in the task bar.
7. Type f
8. Type e
9. Verify that  f[1ɛ 2ɜ 3ə 4e 5ɘ] should be displayed in the Notepad app.
10. Enter Backspace.
11. Verify that the whole option menu including the square brackets [ ] should be deleted.
12. Verify the output results should show ‘ f ‘  in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_24**

Test case for IMSAMPLE_INPUT,CONT,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `24`
- Product: `Windows`
- Source files: `24.JSON`, `24.html`

</details>

**Description**

Original TestLodge ID: TC24

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com Site.
2. Install imsample keyboard.
3. In the Computer\HKEY_CURRENT_USER\SOFTWARE\Keyman\Keyman Engine\Active Keyboards\imsample, create New > DWORD(32-bit) key.
4. Rename ‘ShowIMWindow’ to the new DWORD key.
5. Set the Key value to 0.
6. Create New>DWORD(32-bit) key.
7. Rename ‘ShowIMWindowAlways’ to the new DWORD key.
8. Set the Key value to 0
9. Open Notepad.
10. Type ‘he’ in the text area.
11. Verify the Output should show h[1ɛ 2ɜ 3ə 4e 5ɘ].
12. Type number 3.
13. Verify that the displayed output should show ‘hə’

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click IMTest Keyboard from the Keyman menu.
6. Verify that the Keyboard icon appears in the task bar.
7. Type f
8. Type e
9. Verify that  f[1ɛ 2ɜ 3ə 4e 5ɘ] should be displayed in the Notepad app.
10. Enter Backspace.
11. Verify that the whole option menu including the square brackets [ ] should be deleted.
12. Verify the output results should show ‘ f ‘  in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_26**

Test case for IMSAMPLE_KEYBOARD_IM_WINDOW,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `26`
- Product: `Windows`
- Source files: `26.JSON`, `26.html`

</details>

**Description**

Original TestLodge ID: TC26

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com Site.
2. Install imsample keyboard.
3. In the Computer\HKEY_CURRENT_USER\SOFTWARE\Keyman\Keyman Engine\Active Keyboards\imsample, create New > DWORD(32-bit) key.
4. Rename ‘ShowIMWindow’ to the new DWORD key.
5. Set the Key value to 1.
6. Create New>DWORD(32-bit) key.
7. Rename ‘ShowIMWindowAlways’ to the new DWORD key.
8. Set the Key value to1
9.This keyboard uses the letters aeom to allow IMX input. This time a IM window should display.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click IMTest Keyboard from the Keyman menu.
6. Verify that the Keyboard icon appears in the task bar.
7. Type t r y
8. Enter  key.
9. Verify that the option menu appears on the screen.
10. Click the 1st option.
11. Verify that the output results show show ‘ try æ ‘
12. Type 'm'
13. Verify that the option menu appears on the screen.
14. Select the 3rd ### option.
15. Verify that the output results should show ‘ try æ### ‘

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_1**

Test case for INSTALL Keyman

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `1`
- Product: `Windows`
- Source files: `1.JSON`, `1.html`

</details>

**Description**

Original TestLodge ID: TC01

<details>
<summary>Setup</summary>

1. Download Keyman latest Stable build from Keyman.com Site

</details>

<details>
<summary>Action</summary>

1. Double click on Keyman shortcut icon from Desktop
2. Verify that 'Do you want to allow this app to make changes to your device?' warning message with Yes or No button.
3. Click the 'Yes' button.
4. Verify the 'Keyman Setup' dialog appears on the screen.
5. Click the "Install Options" blue link button.
6. Verify that the Keyman Setup window appears with the ‘Install’ button.
7. Verify that the ‘Install Options’ blue link button appears at the bottom of the dialog.
8. Click on the ‘Install Options’ blue link button.
9. Verify that the ‘Install Options’ dialog box appears on the Screen.
10. Verify that the dialog box appears with the ‘OK’ and ‘Cancel’ button.
11. Click the OK button.
12.  Verify that the Keyman Setup dialog box appears.
13. Click the UI drop down list box.
14. Click ‘Bura-Pabir’ on the list box.
15. Verify that the UI language of Keyman Setup window has been changed into ‘Bura-Pabir’.
16. Click the ‘Install Options’ blue link button.
17. Verify that the UI language menu has been changed into ‘Bura-Pabir’.
18. Click the Install button.
19. Verify that the In-progress window with the message Downloading Keyman desktop.msi appears on the Screen.
20. Verify that the Keyman Welcome Windows appears on the Screen.
21. Verify that the Start Keyman, Configuration and Exit button appears on the dialog.
22. Verify that the Open Configuration blue link button appears on the dialog.
23. Click the ‘Start Keyman’ button.
24. Verify that the Keyman is started and visible in the Notification area (in the task bar).

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version  from the list.
8. Click the Uninstall option.
9. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_3**

Test case for INSTALL_PKG_DISK,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `3`
- Product: `Windows`
- Source files: `3.JSON`, `3.html`

</details>

**Description**

Original TestLodge ID: TC03

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.
2. Download Khmer Angkor.kmp file on the Desktop

</details>

<details>
<summary>Action</summary>

1. Double click on the Khmer Angkor.kmp file.
2. Verify that the Install Keyboard / Package dialog box appears.
3. Verify that Details and Readme tabs appear on this dialog.
4. Click the Readme tab.
5. Verify that the readme file opens.
6. Click the Cancel button.
7. Verify that the Install Keyboard / Package dialog box Closes.
8. Click the Install button.
9. Verify that  a Warning message appears “Do you want to allow this app to make changes to your device?” with a Yes or No button.
10. Click the Yes button.
11. Verify that the Welcome dialog appears on the Screen on the OK button.
12. Click the OK button on the Welcome dialog.
13. Click the Okay button to close the Keyman Configuration dialog.
14. Click the Keyman icon from the Taskbar.
15. Verify that the Keyman menu appears.
16. Click the ‘Configuration’ option from the menu.
17. Verify the Keyman Configuration dialog box opens.
18. Click the ‘Keyboard Layouts’ tab.
19. Verify that the ‘Khmer Angkor’ keyboard is successfully installed.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_35**

Test case for installing offline build (PR #10000):

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `35`
- Product: `Windows`
- Source files: `35.JSON`, `35.html`

</details>

**Description**

Original TestLodge ID: TC186

<details>
<summary>Setup</summary>

1. Download the latest alpha build from the link “https://downloads.keyman.com/windows/”

</details>

<details>
<summary>Action</summary>

1. Open the Command prompt on the Windows desktop via the “cmd” command.
2. Change the directory path to where we downloaded the Keyman Alpha Build.
3. Type the command : keyman latest version number.exe -o (say “17.0.210.exe -o”).
4. Verify that the self extract process happened without any errors.
5.) Verify the build was installed successfully without showing any error messages.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Click the Control Panel option.
3. Click the ‘Uninstall a Program’ option.
4. Select the installed Keyman version from the list.
5. Click the Uninstall option.
6. Verify that the keyman uninstallation was completed successfully.

</details>

---

**TEST_2**

Test case for KEYBOARD_INSTALLATION_REMOTE,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `2`
- Product: `Windows`
- Source files: `2.JSON`, `2.html`

</details>

**Description**

Original TestLodge ID: TC02

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.

</details>

<details>
<summary>Action</summary>

1. Double click on the Keyman icon from the desktop.
2. Verify that the Keyman Welcome dialog appears on the Screen.
3. Click the ‘Configuration’ button.
4. Verify that the Keyman configuration dialog opens.
5. Click the Download Keyboard button.
6. Verify that the ‘Download keyboard from..’ dialog box opens.
7. Type ‘Khmer Angkor’ in the Search box.
8. Verify that the Khmer Angkor Keyboard appears under ‘Results’.
9. Click the ‘Khmer Angkor’ link.
10. Verify that the ‘Install Keyboard’ green button appears.
11. Click the ‘Install Keyboard’ green button.
12. Verify that the ‘Downloading File’ dialog box appears.
13. Verify that the Install Keyboard / Package dialog box appears.
14. Verify that Details and Readme tabs appear on this dialog.
15. Click the Readme tab.
16. Verify that the readme file opens.
17. Click the Cancel button.
18. Verify that the Install Keyboard / Package dialog box Closes.
19. Click the Install button.
20. Verify that  a Warning message appears on the screen stating whether we are allowed to install the Keyman in our Desktop.
21. Click the Okay button.
22. Verify that the Welcome dialog appears on the Screen.
23. Click the OK button on the Welcome dialog.
24. Click the Okay button to close the Keyman Configuration dialog.
25. Click the Keyman icon from the Taskbar.
26. Verify that the Keyman menu appears.
27. Click the ‘Khmer - Khmer Angkor’ option.
28. Open Notepad.
29. Verify that the Khmer Keyboard icon appears instead of the Keyman icon.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_4**

Test Case for KEYBOARD_OUTPUT,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `4`
- Product: `Windows`
- Source files: `4.JSON`, `4.html`

</details>

**Description**

Original TestLodge ID: TC04

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.
2. Install Khmer Angkor Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click on the Keyman icon from the Taskbar.
4. Verify that the Keyman menu opens.
5. Click Khmer - Khmer Angkor keyboard.
6. Verify that the Keyman icon changes in the Task bar.
7. Type ‘xEjmr’ on the Notepad.
8. Verify that the output results  ខ្មែរ
9. Double Click on LibreOffice Writer app
10. Verify that the LibreOffice Document opens.
11. Click on the Keyman icon from the Taskbar.
12. Verify that the Keyman menu opens.
13. Click Khmer - Khmer Angkor keyboard.
14. Verify that the Keyman icon changes in the Task bar.
15. Type ‘xEjmr’ on the Document.
16. Verify that the output results  ខ្មែរ

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_10**

Test case for Lowercase (Capslock-5),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `10`
- Product: `Windows`
- Source files: `10.JSON`, `10.html`

</details>

**Description**

Original TestLodge ID: TC10

<details>
<summary>Setup</summary>

1. Install Keymanlatest Stable build from Keyman.com Site.
2. Install Capslock keyboard.
3. CapsLock is currently on
4. Currently active keyboard is the capslock.kmp keyboard

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Press Shift + d keys.
4. Verify that the text ‘pass’ should appear on the Notepad.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_7**

Test case for Lowercase with Virtual Key (Capslock-2),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `7`
- Product: `Windows`
- Source files: `7.JSON`, `7.html`

</details>

**Description**

Original TestLodge ID: TC07

<details>
<summary>Setup</summary>

1.Install Keyman latest  Stable build from Keyman.com Site.
2.Install CapsLock Keyboard.
3. CapsLock is currently on
4.Currently active keyboard is the CapsLock Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Press Shift + b keys.
4. Verify that the word ‘pass’ should appear on the Notepad.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_16**

Test case for No Caps (CapsOnly-1),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `16`
- Product: `Windows`
- Source files: `16.JSON`, `16.html`

</details>

**Description**

Original TestLodge ID: TC16

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.
2. Install Shift_frees_caps Keyboard.
3. CapsLock is currently off
4. Currently active keyboard is shift_frees_caps keyboard

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Type 1
4. Verify the output results should show ‘pass.’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_13**

Test case for No Caps lock while holding capslock key (capsoff-3),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `13`
- Product: `Windows`
- Source files: `13.JSON`, `13.html`

</details>

**Description**

Original TestLodge ID: TC13

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.
2. Install Caps_always_off Keyboard.
3. CapsLock is currently off

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click Capsalwaysoff keyboard.
6. Verify that the Capsalwaysoff keyboard icon appears in the taskbar.
7. Hold the CapsLock key
8. Type ‘a’
9. Release CapsLock key
10. Verify that the results should show  ‘ncaps_little_a’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_5**

Test Case for ON_SCREEN_KEYBOARD,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `5`
- Product: `Windows`
- Source files: `5.JSON`, `5.html`

</details>

**Description**

Original TestLodge ID: TC05

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.
2. Install Khmer Angkor Keyboard.

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the taskbar.
2. Verify that the Keyman menu opens.
3. Click the Khmer - Khmer Angkor keyboard option.
4. Verify that the Keyman icon changes in the task bar.
5. Click On Screen Keyboard option from the menu.
6. Verify that the On Screen Keyboard
7. Open Notepad app
8. Type ស ដ​ថ​ from the OSK.
9. Verify the output results of the same words.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.yman 16.0’  from the list.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_31**

Test case for OPTION_STORE,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `31`
- Product: `Windows`
- Source files: `31.JSON`, `31.html`

</details>

**Description**

Original TestLodge ID: TC31

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com site.
2. Install store_context Keyboard.
3. Install 021 - options Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click 021-options  Keyboard from the Keyman menu.
6. Verify that the 021-options keyboard icon appears in the task bar.
7. Type a1a0a
8. Verify the output results  "no foo.foo.no foo" should appear in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_32**

Test case for OUTPUT_KEYSTROKE (#10065)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `32`
- Product: `Windows`
- Source files: `32.JSON`, `32.html`

</details>

**Description**

Original TestLodge ID: TC32

<details>
<summary>Setup</summary>

1. Install Keymanlatest  Stable build from Keyman.com site.
2. Install store_context Keyboard.
3. Install 043 - output and keystroke Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click 043 - output and keystroke Keyboard from the Keyman menu.
6. Verify that the 043 - output and keystroke Keyboard icon appears in the task bar.
7. Type 123
8. Verify the output results  "abd3" should appear in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_33**

Test case for OUTPUT_KEYSTROKE_INVALID_CONTEXT,(#10061),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `33`
- Product: `Windows`
- Source files: `33.JSON`, `33.html`

</details>

**Description**

Original TestLodge ID: TC33

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com site.
2. Install store_context Keyboard.
3. Install 043 - output and keystroke Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click 043 - output and keystroke Keyboard from the Keyman menu.
6. Verify that the 043 - output and keystroke Keyboard icon appears in the task bar.
7. Type 1  2
8. Type
9. Type  3
10. Verify the output results
"abc
3"

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_20**

Test case for Shift by itself turns off (CapsOnly-5),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `20`
- Product: `Windows`
- Source files: `20.JSON`, `20.html`

</details>

**Description**

Original TestLodge ID: TC20

<details>
<summary>Setup</summary>

1. Install Keyman latest  Stable build from Keyman.com Site.
2. Install Shift_frees_caps Keyboard.
3. CapsLock is currently off
4. Currently active keyboard is shift_frees_caps keyboard
(Be aware of limitations when testing this on virtual machines as noted above)

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Enable CapsLock key.
4. Press and release the Shift key.
5. Verify that the caps lock indicator turned OFF
6. Verify there was no output should appear in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_19**

Test case for Shift turns off (CapsOnly-4),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `19`
- Product: `Windows`
- Source files: `19.JSON`, `19.html`

</details>

**Description**

Original TestLodge ID: TC19

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.
2. Install Shift_frees_caps Keyboard.
3. CapsLock is currently off
4. Currently active keyboard is shift_frees_caps keyboard

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Enable CapsLock Key.
4. Press Shift + 3 Key.
5. Verify that the Caps lock indicator turned OFF
6. Verify that the output results show  ‘pass.’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_29**

Test case for SIMPLIFIED_CHINESE_BACKSPACE_1:

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `29`
- Product: `Windows`
- Source files: `29.JSON`, `29.html`

</details>

**Description**

Original TestLodge ID: TC29

<details>
<summary>Setup</summary>

1. Install Keyman latest  build from Keyman.com site.
2. Install the Simplified Chinese Keyboard from Keyman site.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click Simplified Chinese  Keyboard from the Keyman menu.
6. Verify that the Keyboard icon appears in the task bar.
7. Type h a n z i
8. Verify that the IMX window will appear and in the top left the letters hanzi should be present
9. Enter  twice
10. Verify that the IMX window should now have the top left letters of han in the Notepad app.
11. Click the 5th option
12. Verify the output results  汗 should appear in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_30**

Test case for SIMPLIFIED_CHINESE_BACKSPACE_2:

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `30`
- Product: `Windows`
- Source files: `30.JSON`, `30.html`

</details>

**Description**

Original TestLodge ID: TC30

<details>
<summary>Setup</summary>

1. Install Keyman latest  build from Keyman.com site.
2. Install the Simplified Chinese Keyboard from Keyman site.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click Simplified Chinese  Keyboard from the Keyman menu.
6. Verify that the Keyboard icon appears in the task bar.
7. Type h a n z i
8. Verify that he IMX window will appear and in the top left the letters hanzi  should be present.
9. Click the 5th option
10. Verify the output results  汉字变换 should appear in the Notepad app.
11. Enter  twice.
12. Verify the output results  汉字 should appear in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_28**

Test case for SIMPLIFIED_CHINESE_MULTIPLE:

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `28`
- Product: `Windows`
- Source files: `28.JSON`, `28.html`

</details>

**Description**

Original TestLodge ID: TC28

<details>
<summary>Setup</summary>

1. Install Keymanlatest  build from Keyman.com site.
2. Install the Simplified Chinese Keyboard from Keyman site.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click Simplified Chinese  Keyboard from the Keyman menu.
6. Verify that the Keyboard icon appears in the task bar.
7. Type h a n z i -
8. Verify that the IMX window will appear and in the top left the letters hanzi should be present
9. Click the 5th option
10. Verify the output results 汉字变换 should appear in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_27**

Test case for SIMPLIFIED_CHINESE_SINGLE,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `27`
- Product: `Windows`
- Source files: `27.JSON`, `27.html`

</details>

**Description**

Original TestLodge ID: TC27

<details>
<summary>Setup</summary>

1. Install Keymanlatest build from Keyman.com site.
2. Install the Simplified Chinese Keyboard

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Click the Keyman icon from the taskbar.
4. Verify that the Keyman menu opens.
5. Click Simplified Chinese  Keyboard from the Keyman menu.
6. Verify that the Keyboard icon appears in the task bar.
7. Type o -
8. Verify that the IMX window should appear on the Screen.
9. Click the 4th option
10. Verify the output results  喔 should appear in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_15**

Test case for switching turns off caps lock (capsoff-5),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `15`
- Product: `Windows`
- Source files: `15.JSON`, `15.html`

</details>

**Description**

Original TestLodge ID: TC15

<details>
<summary>Setup</summary>

• Install Keyman latest  Stable build from Keyman.com Site.
• Install caps_always_off.kmp.
• CapsLock is currently off

</details>

<details>
<summary>Action</summary>

1. Turn on caps lock
2. Double Click on Notepad icon from the desktop.
3. Verify that the Notepad app opens
4. Click the Keyman icon from the taskbar.
5. Verify that the Keyman menu opens.
6. Click Capsalwaysoff keyboard.
7. Type ‘a’
8. Verify that the caps lock indicator turned off
9. Verify that output should show  ‘ncaps_little_a’ in the Notepad app.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_9**

Test case for UpperCase (Capslock-4),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `9`
- Product: `Windows`
- Source files: `9.JSON`, `9.html`

</details>

**Description**

Original TestLodge ID: TC09

<details>
<summary>Setup</summary>

1. Install Keyman latest Stable build from Keyman.com Site.
2. Install Capslock Keyboard.
3. CapsLock is currently on
4. Currently active keyboard is the CapsLock Keyboard

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Type ‘c’
4. Verify that the text ‘pass’ should appear on the Notepad.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_6**

Test case for Uppercase with Virtual key (Capslock-1) ,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `6`
- Product: `Windows`
- Source files: `6.JSON`, `6.html`

</details>

**Description**

Original TestLodge ID: TC06

<details>
<summary>Setup</summary>

1.Install Keyman latest  Stable build from Keyman.com site.
2.Install CapsLock Keyboard.
3.CapsLock is currently on
4. Currently active keyboard is the CapsLock Keyboard.

</details>

<details>
<summary>Action</summary>

1. Double Click on Notepad icon from the desktop.
2. Verify that the Notepad app opens
3. Type ‘a’
4.  Verify that the word ‘pass’ appears on the Notepad.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appears in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_324**

UI layout for update tab_TEST_UI_UPDATES_KBD

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `324`
- Product: `Windows`
- Source files: `324.JSON`, `324.html`

</details>

**Description**

TC - feat(windows): UI layout for update tab_TEST_UI_UPDATES_KBD #12840

<details>
<summary>Setup</summary>

1.    Install the latest version of Keyman.
2.    Install the SIL_Euro_Latin 3.0.2 version keyboard. (older version)
3.    Installed the Gff_Amharic 3.1.1 version keyboard. (older version)

</details>

<details>
<summary>Action</summary>

1.    Start the Keyman.
2.    Open the Keyman Configuration dialog.
3.    Navigate to the "Keyboard Layouts tab".
4.    Verify that the installed keyboard shows an older version.
5.    Navigate to the "Update" tab.
6.    Verify that the "Apply update now" button appears disabled.
7.    Verify that the "Check for new updates" button appears enabled.
8.    Press the "Check for new updates" button.
9.    Verify that the "Update Component", "Old version" and "Size" columns appeared.
10.    Verify that the newer version appears in the "Updated component" column for GFF's  and SIL_Euro_Latin keyboard and Keyman.
11.    Verify that the "Apply update now" button appears in the enabled state.
12.    Verify that the "Updates are available which will be applied when Windows is next restarted." This message appears above the columns.

</details>

<details>
<summary>Cleanup</summary>

1.    Type ‘Control panel’ in the search bar (in the task bar)
2.    Verify that the ‘Control Panel’ appears in the menu list.
3.    Click the Control Panel option.
4.    Verify that the Control Panel dialog appears.
5.    Click the ‘Uninstall a Program’ option.
6.    Verify that the Programs and Features dialog box opens.
7.    Select the installed Keyman version from the list.
8.    Click the Uninstall option.
9.    Verify that the Keyman Uninstallation was completed successfully.

</details>
