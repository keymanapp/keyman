# Linux Acceptance Tests

**TEST_63**

Test case for installing Keyman and onboard.

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `63`
- Product: `Linux`
- Source files: `63.JSON`, `63.html`

</details>

**Description**

Original TestLodge ID: TC35

<details>
<summary>Setup</summary>

Install the latest updates on the system:
1. Open Terminal Window.
2. Run "sudo apt update"
3. Run "sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0"
4. Run "rm -rf ~/.local/share/keyman/"
5. Run "sudo rm -rf /usr/local/share/keyman/"
6. Run "sudo add-apt-repository ppa:keymanapp/keyman-alpha"
7. Run "sudo apt update"
(for pre-beta tests, replace keyman-alpha with keyman-beta)
8. Reboot

</details>

<details>
<summary>Action</summary>

1. Click the terminal icon on the Desktop.
2. Verify the terminal window opens.
3. Run "sudo apt update"
4. Verify the system is up to date.
5. Run "sudo apt install keyman onboard-keyman"
6. Verify that this works without showing any error.

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>

---

**TEST_66**

Test case for the installed keyboard appears in the keyboard dropdown

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `66`
- Product: `Linux`
- Source files: `66.JSON`, `66.html`

</details>

**Description**

Original TestLodge ID: TC38

<details>
<summary>Setup</summary>

1. Run to install the latest Keyman in the system.
2. Install Khmer Angkor Keyboard.

</details>

<details>
<summary>Action</summary>

1. Open a terminal window.
2. Run the “km-config”
3. Verify the Keyman Configuration dialog opens.
4. Verify the Khmer Angkor keyboard appears in the Configuration window.
5. Verify that the language tag for the current keyboard appears in the keyboard.
6. Verify the Keyboard language dropdown list appears in the taskbar (with the language name (Khmer) )
7. Click the launcher icon on the Desktop.
8. Type ‘Settings’ in the Search box.
9. Verify that the Settings icon appears in the Desktop.
10. Click Settings.
11. Verify the Region & Language dialog opens.
12. Verify the keyboard appears in the “Input Sources” list with the language name (eg., Khmer Angkor)

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>

---

**TEST_68**

Test case for adding a keyboard for an additional language (Eurolatin (SIL)),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `68`
- Product: `Linux`
- Source files: `68.JSON`, `68.html`

</details>

**Description**

Original TestLodge ID: TC40

<details>
<summary>Setup</summary>

1. Run to install the latest Keyman in the system.

</details>

<details>
<summary>Action</summary>

1. Click the launcher icon on the desktop.
2. Type “Keyman Configuration” in the search bar.
3. Verify the “Keyman Configuration” dialog appears on the screen.
4. Click the Download button.
5. Type “Icelandic” in the Search box.
6. Click the “EuroLatin (SIL) (Icelandic language)” keyboard from the menu list.
7. Click “Install Keyboard”
8. Verify that the Readme file appears in the install window.
9. Click the “Install” button.
10. Verify that the Welcome file appears after installation.
11. Verify that the new keyboard, Eurolatin (SIL)  appears in the “Keyman Configuration” dialog.
12. Verify the new Keyboard appears on the top right of the screen.

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>

---

**TEST_71**

Test case for UI_About keyboard,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `71`
- Product: `Linux`
- Source files: `71.JSON`, `71.html`

</details>

**Description**

Original TestLodge ID: TC43

<details>
<summary>Setup</summary>

1. Install the latest Keyman in the system.
2. Khmer Angkor Keyboard should be installed before following the steps.

</details>

<details>
<summary>Action</summary>

1. Open Terminal Window.
2. Type a “km-config” command
3. Press the “Enter” key.
4. Verify that the Keyman Configuration dialog opens.
5. Verify that the installed “Khmer Angkor” keyboard appears on the list.
6. Select Khmer Angkor keyboard.
7. Click the About button.
8. Verify package and keyboard information are displayed.
9. Verify that a QR Code link is displayed under Keyboard Information.

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>

---

**TEST_73**

Test case for UI_Options from the Configuration dialog,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `73`
- Product: `Linux`
- Source files: `73.JSON`, `73.html`

</details>

**Description**

Original TestLodge ID: TC45

<details>
<summary>Setup</summary>

1. Run  to install the latest Keyman in the system.
2. EuroLatin (SIL) Keyboard should be installed and ready for testing.
3. Tamil99 Keyboard should be installed and ready for test.

</details>

<details>
<summary>Action</summary>

1. Type ‘Keyman’ from launcher.
2. Verify that the Keyman Configuration dialog box opens.
3. Verify that the installed keyboards appear in the Keyman Configuration dialog.
4. Select EuroLatin (SIL) Keyboard.
5. Verify that the Options button is enabled.
6. Click the Options button.
7. Verify that the Options help page opens.
8. Click the Close button.
9. Verify that the Options help page closes.
10. Select Tamil99  keyboard.
11. Verify that the Options button is disabled.

</details>

<details>
<summary>Cleanup</summary>

1. Open Terminal window.
2. Run
3. Run  to remove left-over artifacts
4. Run

</details>

---

**TEST_76**

Test case for km-package-install using Command line tools,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `76`
- Product: `Linux`
- Source files: `76.JSON`, `76.html`

</details>

**Description**

Original TestLodge ID: TC48

<details>
<summary>Setup</summary>

1. Install the latest Keyman in the system.

</details>

<details>
<summary>Action</summary>

1. Open Terminal Window
2. Run "km-package-install"
3. Verify that you get an error message.
4. Run "km-package-install -p sil_korda_jamo"
5. Verify that this adds the “Korean KORDA Jamo (SIL)” keyboard to the keyboard dropdown.
6. Run "sudo km-package-install -s -f ~/.cache/keyman/hieroglyphic.kmp"
7. Verify that this adds the “Hieroglyphic” keyboard for ancient Egyptian to the keyboard dropdown.
8. Type km-package-install -p gff_amh
9. Press the TAB key.
10. Verify that this completes the command to "km-package-install -p gff_amharic"
11. Run "man km-package-install"
12. Verify it shows the manual page explaining the parameters.
13. Open Chrome browser.
14. Type “https://help.keyman.com/products/linux/15.0/reference/km-package-install “ in the Search box.
15. Verify that the same information is in the manual page.

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>

---

**TEST_78**

Test case for Keyman KVK2LDML file using Command line tools,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `78`
- Product: `Linux`
- Source files: `78.JSON`, `78.html`

</details>

**Description**

Original TestLodge ID: TC50

<details>
<summary>Setup</summary>

1. Run to install the latest Keyman in the system.

</details>

<details>
<summary>Action</summary>

1. Open a Terminal window.
2. Run "km-kvk2ldml"
3. Verify that an error message appears in the terminal window.
4. Run "km-kvk2ldml -o /tmp/test.ldml ~/.local/share/keyman/khmer_angkor/khmer_angkor.kvk"
5. Verify that the test.ldml file is created in the /tmp folder.
6. Run "km-kvk2ldml -p ~/.local/share/keyman/khmer_angkor/khmer_angkor.kvk"
7. Verify that this prints information about the keyboard.
8. Run "km-kvk2ldml -k -p ~/.local/share/keyman/khmer_angkor/khmer_angkor.kvk"
9. Verify that, in addition to the information about the keyboard, it prints the keys contained in the keyboard.
10. Run "man km-kvk2ldml"
11. Verify it shows the manual page explaining the parameters.
12. Open Chrome browser.
13. Type https://help.keyman.com/products/linux/15.0/reference/km-kvk2ldml in the search bar.
14. Verify that the same information appears in the manual page too.
(Note: Replace 15.0 with the Keyman version you're testing)

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>

---

**TEST_81**

Test case for verifying specific keyboards,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `81`
- Product: `Linux`
- Source files: `81.JSON`, `81.html`

</details>

**Description**

Original TestLodge ID: TC53

<details>
<summary>Setup</summary>

1. Install the latest Keyman in the system.
2. Install IPA (SIL) keyboard.
3. Install the Korean KORDA Jamo (SIL) keyboard.
4. Install Khmer Angkor keyboard.

</details>

<details>
<summary>Action</summary>

1. Click LibreOffice Writer.
2. Verify that the LibreOffice Writer opens.
3. Click the Keyman icon from the taskbar.
4. Verify that the IPA (SIL) keyboard appears in the Keyman menu.
5. Select IPA (SIL) keyboard.
6. Type n > in the document.
7. Verify that it shows ŋ in the document.
8. Type ‘gedit’ from the launcher.
9. Verify that the text editor opens.
10. Type n > in the document.
11. Verify that it shows ŋ in the document.
12. Open LibreOffice document.
13. Click the Keyman icon from the taskbar.
14. Verify that the “Korean KORDA Jamo (SIL)” keyboard.
15. Type hangeul.
16. Verify that the result is “한글”
17. Type ‘gedit’ from the launcher.
18. Verify that the text editor opens.
19. Type hangeul in the document.
20. Verify that it shows “한글” in the document.
21. Open LibreOffice document.
22. Click the Keyman icon from the taskbar.
23. Verify that the “Khmer Angkor” keyboard appears.
24. Type xEjmr.
25. Verify that it shows the result “ខ្មែរ”
26. Type ‘gedit’ from the launcher.
27. Verify that the text editor opens.
28. Type xEjmr in the document.
29. Verify that it shows “ខ្មែរ” in the document.
30. Type ‘gedit’ from the launcher.
31. Verify that the text editor opens.
32. Click the Keyman icon from the taskbar.
33. Verify that the “IPA (SIL)r” keyboard appears.
34. Type
an
m
35. Click after n
36. Type >
37. Verify that the output shows
aŋ
m

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>
**TEST_83**

Test case for update an existing installation,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `83`
- Product: `Linux`
- Source files: `83.JSON`, `83.html`

</details>

**Description**

Original TestLodge ID: TC55

<details>
<summary>Setup</summary>

1. Install the latest Keyman in the system.
2. Install Khmer Angkor Keyboard.
3. Install IPA (SIL) Keyboard.

</details>

<details>
<summary>Action</summary>

1. Open Terminal Window.
2. Run "sudo apt --autoremove remove keyman"
3. Run "sudo rm /etc/apt/sources.list.d/keymanapp-ubuntu-keyman*"
4. Run "sudo add-apt-repository ppa:keymanapp/keyman"
5. Run "sudo apt update"
6. Run "sudo apt install keyman"
7. Verify that this has installed the latest stable version.
8. Click the Keyman icon, which appears in the taskbar.
9. Verify that the Keyman keyboards still show up in the keyboards dropdown.
10. Select Khmer Angkor Keyboard.
11. Verify that the Khmer Angkor Keyboard icon appears in the taskbar instead of the Keyman icon.
12. Open Text editor.
13. Type xEjmr.
14. Verify that the output results “ខ្មែរ” in the text editor.

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>

---

**TEST_86**

Test case for bug(linux): backspace doesn't work in "Download Keyman keyboards" dialog #7971,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `86`
- Product: `Linux`
- Source files: `86.JSON`, `86.html`

</details>

**Description**

Original TestLodge ID: TC214

<details>
<summary>Setup</summary>

1. Install Keyman's latest Stable build from the Keyman.com site.
2. Install  Vietnamese_telex  keyboard.

</details>

<details>
<summary>Action</summary>

1. Open the Keyman Configuration dialog box.
2. Switch to Vietnamese_telex keyboard.
3. Click on the Keyboard Search bar.
4. Type ‘gox tieesn Vieejt’
5. Verify it outputs gõ tiếng Việt

</details>

<details>
<summary>Cleanup</summary>

1. Open a Terminal window.
2. Type “sudo apt --autoremove remove keyman ibus-keyman python3-keyman-config libkmnkbp0-0”.
3. Run the command below to remove leftover artifacts.
4. rm -rf ~/.local/share/keyman/
5. sudo rm -rf /usr/local/share/keyman/

</details>

---

**TEST_88**

KMX_PROCESSOR_NON_COMPLIANT_TEST_FRAME_KEY_RESET_MARKERS_GROUP_LINUX: Chrome Browser

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `88`
- Product: `Linux`
- Source files: `88.JSON`, `88.html`

</details>

**Description**

Original TestLodge ID: TC228

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from Linux Desktop.
2. Verify that the Chrome browser opens.
3. Navigate to the "https://www.editpad.org/" page.
4. Click in the edit box.
5. Press `
6. Verify it produces `
7. Press the "right arrow"
8. Type e
9. Verify it produces `e

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux.
2. Uninstall Chrome browser.

</details>

---

**TEST_90**

KMX_PROCESSOR_NON_COMPLIANT_TEST_SCROLLLOCK_KEY_NO_RESET_GROUP_LINUX: Chrome Browser

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `90`
- Product: `Linux`
- Source files: `90.JSON`, `90.html`

</details>

**Description**

Original TestLodge ID: TC230

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install TextEdit app.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from Linux Desktop.
2. Verify that the Chrome browser opens.
3. Navigate to the "https://www.editpad.org/" page.
4. Click in the edit box.
5. Press `
6. Verify it produces `
7. Press the "scroll lock" key
8. Type e
9. Verify it produces è

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux.
2. Uninstall Chrome browser.

</details>

---

**TEST_91**

KMX_PROCESSOR_COMPLIANT_TEST_CONTROL_1_GROUP_LINUX: gedit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `91`
- Product: `Linux`
- Source files: `91.JSON`, `91.html`

</details>

**Description**

Original TestLodge ID: TC240

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the “gedit” App icon from Linux Desktop.
2. Verify that the “gedit” Application opens.
3. Type `
4. Verify it produces e.
5. Verify it produces è

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux

</details>

---

**TEST_93**

KMX_PROCESSOR_COMPLIANT_TEST_FRAME_KEY_RESET_NOMARKERS_GROUP_LINUX: gedit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `93`
- Product: `Linux`
- Source files: `93.JSON`, `93.html`

</details>

**Description**

Original TestLodge ID: TC242

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the “gedit” App icon from Linux Desktop.
2. Verify that the “gedit” Application opens.
3. Press 1 \  \
4. Verify it produces  1 \ \
5. Press the "right arrow"
6. Type 2
7. Verify it produces 1/2

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux

</details>

---

**TEST_94**

KMX_PROCESSOR_COMPLIANT_TEST_SCROLLLOCK_KEY_NO_RESET_GROUP_LINUX: gedit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `94`
- Product: `Linux`
- Source files: `94.JSON`, `94.html`

</details>

**Description**

Original TestLodge ID: TC243

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the “gedit” App icon from Linux Desktop.
2. Verify that the “gedit” Application opens.
3. Press `
4. Verify it produces `
5. Press the "Scroll Lock"
6. Type e
7. Verify it produces è

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux

</details>

---

**TEST_96**

LDML_PROCESSOR_NON_COMPLIANT_TEST_FRAME_KEY_RESET_MARKERS_GROUP_LINUX: Chrome Browser

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `96`
- Product: `Linux`
- Source files: `96.JSON`, `96.html`

</details>

**Description**

Original TestLodge ID: TC255

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from Linux Desktop.
2. Verify that the Chrome browser opens.
3. Navigate to the "www.google.com" page.
4. Click in the search box.
5. Type ^
6. Verify it produces ^.
7. Press the “right arrow”(->)
8. Verify it produces e

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux.
2. Uninstall Chrome browser.

</details>

---

**TEST_97**

LDML_PROCESSOR_NON_COMPLIANT_TEST_FRAME_KEY_RESET_NO_MARKERS_GROUP_LINUX: Chrome Browser

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `97`
- Product: `Linux`
- Source files: `97.JSON`, `97.html`

</details>

**Description**

Original TestLodge ID: TC256

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from Linux Desktop.
2. Verify that the Chrome browser opens.
3. Navigate to the "www.google.com" page.
4. Click in the search box.
5. Press a
6. Verify it produces a
7. Press the "right arrow"(->)
8. Press the SHIFT + 8
9. Verify it produces a*

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux.
2. Uninstall Chrome browser.

</details>

---

**TEST_98**

LDML_PROCESSOR_NON_COMPLIANT_TEST_FRAME_SCROLL_LOCK_NO_RESET_GROUP_LINUX: Chrome Browser

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `98`
- Product: `Linux`
- Source files: `98.JSON`, `98.html`

</details>

**Description**

Original TestLodge ID: TC257

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from Linux Desktop.
2. Verify that the Chrome browser opens.
3. Navigate to the "www.google.com" page.
4. Click in the search box.
5. Press ^
6. Verify it produces ^
7. Press the "Scroll Lock"
8. Press the e
9. Verify it produces  ê

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux.
2. Uninstall Chrome browser.

</details>

---

**TEST_100**

LDML_PROCESSOR_COMPLIANT_TEST_CONTROL_1_LINUX: gedit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `100`
- Product: `Linux`
- Source files: `100.JSON`, `100.html`

</details>

**Description**

Original TestLodge ID: TC269

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install gedit app.

</details>

<details>
<summary>Action</summary>

1. Click on the gedit App icon from Linux Desktop.
2. Verify that the gedit Application opens.
3. Press ^
4. Verify it produces ^
5. Type e
6. Verify it produces ê

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux
2. Uninstall the gedit app.

</details>

---

**TEST_101**

LDML_PROCESSOR_COMPLIANT_TEST_FRAME_KEY_RESET_MARKERS_LINUX: gedit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `101`
- Product: `Linux`
- Source files: `101.JSON`, `101.html`

</details>

**Description**

Original TestLodge ID: TC270

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install gedit app.

</details>

<details>
<summary>Action</summary>

1. Click on the gedit App icon from Linux Desktop.
2. Verify that the gedit Application opens.
3. Press ^
4. Verify it produces ^
5. Press the "right arrow(->)"
6. Type e
7. Verify it produces  e

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux.
2. Uninstall the gedit app.

</details>

---

**TEST_103**

LDML_PROCESSOR_COMPLIANT_TEST_FRAME_SCROLL_LOCK_NO_RESET_LINUX: gedit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `103`
- Product: `Linux`
- Source files: `103.JSON`, `103.html`

</details>

**Description**

Original TestLodge ID: TC272

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install gedit app.

</details>

<details>
<summary>Action</summary>

1. Click on the gedit App icon from Linux Desktop.
2. Verify that the gedit Application opens.
3. Press ^
4. Verify it produces ^
5. Press the "Scroll Lock" key
6. Type e
7. Verify it produces ê

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux.
2. Uninstall the gedit app.

</details>

---

**TEST_104**

LDML_PROCESSOR_COMPLIANT_TEST_MODIFIER_TAP_NO_RESET_GROUP_LINUX: gedit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `104`
- Product: `Linux`
- Source files: `104.JSON`, `104.html`

</details>

**Description**

Original TestLodge ID: TC273

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install gedit app.

</details>

<details>
<summary>Action</summary>

1. Click on the gedit App icon from Linux Desktop.
2. Verify that the gedit Application opens.
3. Press ^
4. Verify it produces ^
5. Press and Release CTRL key.
6. Type e
7. Verify it produces ê

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the Linux
2. Uninstall the gedit app.

</details>

---

