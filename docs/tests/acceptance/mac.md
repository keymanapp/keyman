# MAC Acceptance Tests

Generated from the numbered HTML/JSON regression-test export.

**TEST_233**

change(mac): remove 'Always show OSK' option #12355 (TEST_ALWAYS_SHOW_OPTION_IS_GONE)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `233`
- Product: `MAC`
- Source files: `233.JSON`, `233.html`

</details>

**Description**

Original TestLodge ID: TC288

<details>
<summary>Setup</summary>

1. Download the MacOS installer build from "state.keyman.com"
2. Install Keyman build on the Sonoma or the latest macOS.

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the taskbar.
2. Verify the Keyman menu opens.
3. Select the "Configuration"
4. Verify the "Keyman Configuration" dialog appears.
5. Installed the "Khmer Angkor" keyboard by clicking the "Download Keyboard" button.
6. Verify the "Khmer Angkor" keyboard appears on the "Keyman Configuration" dialog.
7. Click on the "Options" tab.
8. Verify the "Always show on-screen keyboard" checkbox is gone.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall the Keyman build on the macOS.

</details>

---

**TEST_238**

change(mac): remove 'Always show OSK' option #12355 (TEST_OSK_REMEMBERS_SIZE_AND_LOCATION_AFTER_RESTART)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `238`
- Product: `MAC`
- Source files: `238.JSON`, `238.html`

</details>

**Description**

Original TestLodge ID: TC293

<details>
<summary>Setup</summary>

1. Download the MacOS installer build from "state.keyman.com"
2. Install Keyman build on the Sonoma or the latest macOS.

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the taskbar.
2. Verify the Keyman menu opens.
3. Select the "On-Screen Keyboard"
4. Verify the OSK appears.
5. Change the OSK size and move the OSK to 2nd monitor screen.
6. Verify the OSK appears in a bigger size.
7. Restart the machine.
8. Login as a user.
9. Switch to the keyman keyboard.
10. Verify the OSK keyboard is change.
11. Verify the OSK appears in the same bigger size and the OSK position appears in the same place earlier too.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall the Keyman build on the macOS.

</details>

---

**TEST_236**

change(mac): remove 'Always show OSK' option #12355(TEST_OSK_REMAINS_HIDDEN_WITH_KEYMAN_INACTIVE)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `236`
- Product: `MAC`
- Source files: `236.JSON`, `236.html`

</details>

**Description**

Original TestLodge ID: TC291

<details>
<summary>Setup</summary>

1. Download the MacOS installer build from "state.keyman.com"
2. Install Keyman build on the Sonoma or the latest macOS.

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the taskbar.
2. Verify the Keyman menu opens.
3. Select the "On-Screen Keyboard"
4. Verify the OSK appears.
5. Change the keyboard from Keyman to Apple(U.S. Keyboard)
6. Verify that the OSK appears.
7. Open the "Text Editor" application.
8. Type some text in the "Text Editor" application.
9. Verify the text reflect it.
10. Switch the application from "Text Editor" to "Stickes."
11. Click on the app's menu and items.
12. Verify the OSK does not flash when it is not inactive state.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall the Keyman build on the macOS.

</details>

---

**TEST_231**

feat(mac): both option keys generate right alt if no left alt mapping #12458(TEST_RALT_KEYBOARD_WITH_OPTION_KEY_COMBOS)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `231`
- Product: `MAC`
- Source files: `231.JSON`, `231.html`

</details>

**Description**

Original TestLodge ID: TC286

<details>
<summary>Setup</summary>

1. Download the MacOS installer build from "state.keyman.com"
2. Install Keyman build on the Sonoma or the latest macOS.

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the taskbar.
2. Verify the Keyman menu opens.
3. Select the "Configuration"
4. Verify the "Keyman Configuration" dialog appears.
5. Installed the "Armenian Mnemonic" keyboard by clicking the "Download Keyboard" button.
6. Verify the "Armenian Mnemonic" keyboard appears on the "Keyman Configuration" dialog.
7. Switch to the "Armenian Mnemonic" keyboard.
8. Press the ALT + 9 on the left side.
9. Verify this generates the subscript character '₉'
10. Press the ALT + SHIFT + 9 on the left side.
11. Verify that this generates the superscript character '⁹'
12. Press the ALT + SHIFT + 9 on the right side.
13. Verify that this generates the same superscript character '⁹'

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall the Keyman build on the macOS.

</details>

---

**TEST_341**

Handle PackageInfo section in kmp.inf file

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `341`
- Product: `MAC`
- Source files: `341.JSON`, `341.html`

</details>

**Description**

TC - fix(mac): handle PackageInfo section in kmp.inf file #13876

<details>
<summary>Setup</summary>

1.    Download the MacOS installer build from "state.keyman.com"(Alpha version) or keyman.com (stable version)
2.    Install Keyman build on the Sonoma or the latest macOS.

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Open the Configuration dialog by clicking Keyman icon --> Configuration.
3.    Click the "Download Keyboard" button to open the "Keyman: Download Keyboard" dialog
4.    Verify that the "Keyman: Download Keyboard" dialog appears.
5.    Search for the “Sanskrit Unicode” keyboard in the search box.
6.    Verify that the “Sanskrit Unicode” results appeared.
7.    Install the “Sanskrit Unicode” keyboard.
8.    Verify that the “Download complete” pop-up window
9.    Click the "Done" button on the pop-up window.
10.    Verify that the "Package Information" dialog appears.
11.    Press “x” to exit "Package Information."
12.    Open the "Notes" app.
13.    Open a blank note.
14.    Select the “Sanskrit Unicode” keyboard by clicking the Keyman icon --> Sanskrit Unicode
15.    Enter some text in the note app using the "Sanskrit Unicode" keyboard.
16.    Verify that the "Sanskrit Unicode" keyboard appears in the configuration dialog.
17.    Verify that the "Sanskrit Unicode" letter appears in the note app.
18.    Close the keyman's configuration dialog.

</details>

<details>
<summary>Cleanup</summary>

1.    Uninstall the Keyman build on macOS.

</details>

---

**TEST_215**

KMX_PROCESSOR_COMPLIANT_TEST_CONTROL_1_GROUP_MAC: TextEdit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `215`
- Product: `MAC`
- Source files: `215.JSON`, `215.html`

</details>

**Description**

Original TestLodge ID: TC235

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from MAC.
2. Verify that the TextEdit Application opens.
3. Type `
4. Verify it produces e.
5. Verify it produces è

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the MAC

</details>

---

**TEST_218**

KMX_PROCESSOR_COMPLIANT_TEST_SCROLLLOCK_KEY_NO_RESET_GROUP_MAC: TextEdit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `218`
- Product: `MAC`
- Source files: `218.JSON`, `218.html`

</details>

**Description**

Original TestLodge ID: TC238

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from MAC.
2. Verify that the TextEdit Application opens.
3. Press `
4. Verify it produces `
5. Press the "fn"
6. Type e
7. Verify it produces è

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the MAC
SUITE_KMX_PROCESSOR_COMPLIANT_GROUP_LINUX:

</details>

---

**TEST_210**

KMX_PROCESSOR_NON_COMPLIANT_TEST_CONTROL_1_GROUP_MAC: Chrome Browser

Status: `Deprecated`

<details>
<summary>Metadata</summary>

- Source ID: `210`
- Product: `MAC`
- Source files: `210.JSON`, `210.html`

</details>

**Description**

Original TestLodge ID: TC222

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from macOS Desktop.
2. Verify that the Chrome browser opens.
3. Navigate to the "www.google.com" page.
4. Click in the search box.
5. Type `
6. Verify it produces e.
7. Verify it produces è

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall Chrome browser.

</details>

---

**TEST_213**

KMX_PROCESSOR_NON_COMPLIANT_TEST_FRAME_KEY_RESET_NOMARKERS_GROUP_MAC: Chrome Browser

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `213`
- Product: `MAC`
- Source files: `213.JSON`, `213.html`

</details>

**Description**

Original TestLodge ID: TC225

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from macOS Desktop.
2. Verify that the Chrome browser opens.
3. Navigate to the "www.google.com" page.
4. Click in the search box.
5. Press 1 / /
6. Verify it produces 1 / /
7. Press the "right arrow"
8. Type 2
9. Verify it produces 1//2

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall Chrome browser.

</details>

---

**TEST_226**

LDML_PROCESSOR_COMPLIANT_TEST_FRAME_KEY_RESET_NO_MARKERS_MAC: TextEdit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `226`
- Product: `MAC`
- Source files: `226.JSON`, `226.html`

</details>

**Description**

Original TestLodge ID: TC266

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install TextEdit app.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from macOS Desktop.
2. Verify that the TextEdit Application opens.
3. Press a
4. Verify it produces a
5. Press the "right arrow"(->)
6. Press the SHIFT + 8
7. Verify it produces “Star”

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the macOS
2. Uninstall the TextEdit app.

</details>

---

**TEST_228**

LDML_PROCESSOR_COMPLIANT_TEST_MODIFIER_TAP_NO_RESET_GROUP_MAC: TextEdit

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `228`
- Product: `MAC`
- Source files: `228.JSON`, `228.html`

</details>

**Description**

Original TestLodge ID: TC268

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install TextEdit app.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from macOS Desktop.
2. Verify that the TextEdit Application opens.
3. Press ^
4. Verify it produces ^
5. Press and Release CTRL key.
6. Type e
7. Verify it produces ê

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the macOS
2. Uninstall the TextEdit app.

</details>

---

**TEST_220**

LDML_PROCESSOR_NON_COMPLIANT_TEST_FRAME_KEY_RESET_MARKERS_GROUP_MAC: Chrome Browser

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `220`
- Product: `MAC`
- Source files: `220.JSON`, `220.html`

</details>

**Description**

Original TestLodge ID: TC250

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from macOS Desktop.
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

1. Uninstall Keyman build on the device.
2. Uninstall Chrome browser.

</details>

---

**TEST_223**

LDML_PROCESSOR_NON_COMPLIANT_TEST_MODIFIER_TAP_NO_RESET_GROUP_MAC: Chrome Browser

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `223`
- Product: `MAC`
- Source files: `223.JSON`, `223.html`

</details>

**Description**

Original TestLodge ID: TC253

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from macOS Desktop.
2. Verify that the Chrome browser opens.
3. Navigate to the "www.google.com" page.
4. Click in the search box.
5. Press ^
6. Verify it produces ^
7. Press and release the CTRL
8. Press the e
9. Verify it produces  ê

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall Chrome browser.

</details>

---

**TEST_175**

Test case for ATOM editor with Amharic,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `175`
- Product: `MAC`
- Source files: `175.JSON`, `175.html`

</details>

**Description**

Original TestLodge ID: TC115

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Amharic keyboard.
3. Install Atom Editor.

</details>

<details>
<summary>Action</summary>

1. Click on the Atom Editor icon from the macOS Desktop.
2. Verify the Atom Editor opens.
3. Type t a
4. Verify it produces ታ.
5. Type t, left-arrow, a
6. Verify it produces አት
7. Type t t, left-arrow, a
8. Verify it produces ትአት
9. Type t t
10. Mouse-clicking between the two ትት characters.
11. Type a
12. Verify it produces ትአት
13. Verify in each of three cases, the insertion point should end up before the final ት character.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall the Amharic keyboard.
3. Uninstall Atom Editor.

</details>

---

**TEST_173**

Test case for Chrome_Google_Docs with Amharic,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `173`
- Product: `MAC`
- Source files: `173.JSON`, `173.html`

</details>

**Description**

Original TestLodge ID: TC113

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Amharic keyboard.
3. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from macOS Desktop.
2. Verify that the Chrome browser opens.
3. Open Google document.
4. Type t a
5. Verify it produces ታ.
6. Type t, left-arrow, a
7. Verify it produces አት
8. Type t t, left-arrow, a
9. Verify it produces ትአት
10. Type t t
11. Mouse-clicking between the two ትት characters.
12. Type a
13. Verify it produces ትአት
14. Verify in each of three cases, the insertion point should end up before the final ት character.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall the Amharic keyboard.
3. Uninstall the Chrome browser.

</details>

---

**TEST_190**

Test case for Chrome_URL_Bar with Shortcuts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `190`
- Product: `MAC`
- Source files: `190.JSON`, `190.html`

</details>

**Description**

Original TestLodge ID: TC130

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon.
2. Verify that the Chrome browser opens.
3. Press Command +  F (⌘+F) keys from magic keyboard.
4. Verify that the Find or Control dialog box opens.
5. Press Command + O (⌘-O) keys from magic keyboard.
6. Verify that the File opens.
7. Type ‘testing’ in the URL bar.
8. Press Command + A (⌘-A) keys from magic keyboard.
9. Verify that the text ‘testing’ has been selected.
10. Press Command + X (⌘-X) keys from magic keyboard.
11. Verify that the text ‘testing’ has been cut from the URL.
12. Press Command + V (⌘-V) keys from magic keyboard.
13. Verify that the text ‘testing’ has been pasted in the URL.
14. Press Command + C (⌘-C) keys from magic keyboard.
15. Verify that the text ‘testing’ has been copied from the URL.
16. Press Command + V (⌘-V) keys from magic keyboard.
17. Verify that the text ‘testing’  has been pasted in the URL.
18. Press Command + A (⌘-A) keys from magic keyboard.
19. Verify that the text ‘testing’ has been selected.
20. Type t
21. Verify it replaces selected characters with ት
22. Type a   t
23. Press Command + A (⌘-A) keys
24. Verify that the text ‘testing’ has been selected.
25. Type a
26. Verify it produces አ

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall Chrome browser.

</details>

---

**TEST_193**

Test case for Chrome_Word_Online with Shortcuts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `193`
- Product: `MAC`
- Source files: `193.JSON`, `193.html`

</details>

**Description**

Original TestLodge ID: TC133

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Chrome browser icon from macOS Desktop.
2. Verify that the Chrome browser opens.
3. Open ‘Word online’ document.
4. Press Command +  F (⌘+F) keys from magic keyboard.
5. Verify that the Find or Control dialog box opens.
6. Press Command + O (⌘-O) keys from magic keyboard.
7. Verify that the File opens.
8. Type ‘testing’ in the URL bar.
9. Press Command + A (⌘-A) keys from magic keyboard.
10. Verify that the text ‘testing’ has been selected.
11. Press Command + X (⌘-X) keys from magic keyboard.
12. Verify that the text ‘testing’ has been cut from the URL.
13. Press Command + V (⌘-V) keys from magic keyboard.
14. Verify that the text ‘testing’ has been pasted in the URL.
15. Press Command + C (⌘-C) keys from magic keyboard.
16. Verify that the text ‘testing’ has been copied from the URL.
17. Press Command + V (⌘-V) keys from magic keyboard.
18. Verify that the text ‘testing’  has been pasted in the URL.
19. Press Command + A (⌘-A) keys from magic keyboard.
20. Verify that the text ‘testing’ has been selected.
21. Type t
22. Verify it replaces selected characters with ት
23. Type a   t
24. Press Command + A (⌘-A) keys
25. Verify that the text ‘testing’ has been selected.
26. Type a
27. Verify it produces አ

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall Chrome browser.

</details>

---

**TEST_200**

Test case for Firefox_FB_Search_Control with Shortcuts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `200`
- Product: `MAC`
- Source files: `200.JSON`, `200.html`

</details>

**Description**

Original TestLodge ID: TC140

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the Firefox browser icon from macOS Desktop.
2. Verify that the Firefox browser opens.
3. Type ‘www.facebook.com’ on the URL search bar.
4. Press Enter key.
5. Verify that the Facebook page opens.
6. Verify the ‘Search Facebook’ bar appears on the page.
7. Press Command +  F (⌘+F) keys from magic keyboard.
8. Verify that the Find or Control dialog box opens.
9. Press Command + O (⌘-O) keys from magic keyboard.
10. Verify that the File opens.
11. Type ‘testing’ in the URL bar.
12. Press Command + A (⌘-A) keys from magic keyboard.
13. Verify that the text ‘testing’ has been selected.
14. Press Command + X (⌘-X) keys from magic keyboard.
15. Verify that the text ‘testing’ has been cut from the URL.
16. Press Command + V (⌘-V) keys from magic keyboard.
17. Verify that the text ‘testing’ has been pasted in the URL.
18. Press Command + C (⌘-C) keys from magic keyboard.
19. Verify that the text ‘testing’ has been copied from the URL.
20. Press Command + V (⌘-V) keys from magic keyboard.
21. Verify that the text ‘testing’  has been pasted in the URL.
22. Press Command + A (⌘-A) keys from magic keyboard.
23. Verify that the text ‘testing’ has been selected.
24. Type t
25. Verify it replaces selected characters with ት
26. Type a   t
27. Press Command + A (⌘-A) keys
28. Verify that the text ‘testing’ has been selected.
29. Type a
30. Verify it produces አ

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.

</details>

---

**TEST_180**

Test case for Firefox_URL_Bar with Amharic,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `180`
- Product: `MAC`
- Source files: `180.JSON`, `180.html`

</details>

**Description**

Original TestLodge ID: TC120

<details>
<summary>Setup</summary>

1. Install Keyman build on the device.
2. Install Amharic keyboard.
3. Install Firefox browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Firefox browser icon from macOS Desktop.
2. Verify that the Firefox browser opens.
3. Click on the URL bar.
4. Type t a
5. Verify it produces ታ.
6. Type t, left-arrow, a
7. Verify it produces አት
8. Type t t, left-arrow, a
9. Verify it produces ትአት
10. Type t t
11. Mouse-clicking between the two ትት characters.
12. Type a
13. Verify it produces ትአት
14. Verify in each of three cases, the insertion point should end up before the final ት character.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall the Amharic keyboard.
3. Uninstall the Firefox browser.

</details>

---

**TEST_183**

Test case for Firefox_Word_Online with Amharic,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `183`
- Product: `MAC`
- Source files: `183.JSON`, `183.html`

</details>

**Description**

Original TestLodge ID: TC123

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Amharic keyboard.
3. Install Firefox browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Firefox browser icon from macOS Desktop.
2. Verify that the Firefox browser opens.
3. Open the ‘Word online’ document.
4. Type t a
5. Verify it produces ታ.
6. Type t, left-arrow, a
7. Verify it produces አት
8. Type t t, left-arrow, a
9. Verify it produces ትአት
10. Type t t
11. Mouse-clicking between the two ትት characters.
12. Type a
13. Verify it produces ትአት
14. Verify in each of three cases, the insertion point should end up before the final ት character.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall the Amharic keyboard.
3. Uninstall the Firefox browser.

</details>

---

**TEST_188**

Test case for LibreOffice Writer with Amharic,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `188`
- Product: `MAC`
- Source files: `188.JSON`, `188.html`

</details>

**Description**

Original TestLodge ID: TC128

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Amharic keyboard.
3. Install LibreOffice 7.0 app.

</details>

<details>
<summary>Action</summary>

1. Click on the LibreOffice Writer icon from the macOS Desktop.
2. Verify that the LibreOffice Writer Application opens.
3. Type t a
4. Verify it produces ታ.
5. Type t, left-arrow, a
6. Verify it produces አት
7. Type t t, left-arrow, a
8. Verify it produces ትአት
9. Type t t
10. Mouse-clicking between the two ትት characters.
11. Type a
12. Verify it produces ትአት
13. Verify in each of three cases, the insertion point should end up before the final ት character.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall the Amharic keyboard.
3. Uninstall LibreOffice 7.0 app.

</details>

---

**TEST_208**

Test case for MAIL app with Shortcuts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `208`
- Product: `MAC`
- Source files: `208.JSON`, `208.html`

</details>

**Description**

Original TestLodge ID: TC148

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the MAIL icon from macOS Desktop.
2. Verify that the Mail > message body opens.
3. Press Command +  F (⌘+F) keys from magic keyboard.
4. Verify that the Find or Control dialog box opens.
5. Press Command + O (⌘-O) keys from magic keyboard.
6. Verify that the File opens.
7. Type ‘testing’ in the URL bar.
8. Press Command + A (⌘-A) keys from magic keyboard.
9. Verify that the text ‘testing’ has been selected.
10. Press Command + X (⌘-X) keys from magic keyboard.
11. Verify that the text ‘testing’ has been cut from the URL.
12. Press Command + V (⌘-V) keys from magic keyboard.
13. Verify that the text ‘testing’ has been pasted in the URL.
14. Press Command + C (⌘-C) keys from magic keyboard.
15. Verify that the text ‘testing’ has been copied from the URL.
16. Press Command + V (⌘-V) keys from magic keyboard.
17. Verify that the text ‘testing’  has been pasted in the URL.
18. Press Command + A (⌘-A) keys from magic keyboard.
19. Verify that the text ‘testing’ has been selected.
20. Type t
21. Verify it replaces selected characters with ት
22. Type a   t
23. Press Command + A (⌘-A) keys
24. Verify that the text ‘testing’ has been selected.
25. Type a
26. Verify it produces አ

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.

</details>

---

**TEST_203**

Test case for Messages_App with Shortcuts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `203`
- Product: `MAC`
- Source files: `203.JSON`, `203.html`

</details>

**Description**

Original TestLodge ID: TC143

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the Message App icon from macOS Desktop.
2. Verify that the Message Application opens.
3. Click the To: line
4. Press Command +  F (⌘+F) keys from magic keyboard.
5. Verify that the Find or Control dialog box opens.
6. Press Command + O (⌘-O) keys from magic keyboard.
7. Verify that the File opens.
8. Type ‘testing’ in the URL bar.
9. Press Command + A (⌘-A) keys from magic keyboard.
10. Verify that the text ‘testing’ has been selected.
11. Press Command + X (⌘-X) keys from magic keyboard.
12. Verify that the text ‘testing’ has been cut from the URL.
13. Press Command + V (⌘-V) keys from magic keyboard.
14. Verify that the text ‘testing’ has been pasted in the URL.
15. Press Command + C (⌘-C) keys from magic keyboard.
16. Verify that the text ‘testing’ has been copied from the URL.
17. Press Command + V (⌘-V) keys from magic keyboard.
18. Verify that the text ‘testing’  has been pasted in the URL.
19. Press Command + A (⌘-A) keys from magic keyboard.
20. Verify that the text ‘testing’ has been selected.
21. Type t
22. Verify it replaces selected characters with ት
23. Type a   t
24. Press Command + A (⌘-A) keys
25. Verify that the text ‘testing’ has been selected.
26. Type a
27. Verify it produces አ

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.

</details>

---

**TEST_185**

Test case for Notes_App with Amharic,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `185`
- Product: `MAC`
- Source files: `185.JSON`, `185.html`

</details>

**Description**

Original TestLodge ID: TC125

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Amharic keyboard.
3. Install Notes App.

</details>

<details>
<summary>Action</summary>

1. Click on the Notes App icon from macOS Desktop.
2. Verify that the Notes Application opens.
3. Type t a
4. Verify it produces ታ.
5. Type t, left-arrow, a
6. Verify it produces አት
7. Type t t, left-arrow, a
8. Verify it produces ታት
9. Type t t
10. Mouse-clicking between the two ታ characters.
11. Type a
12. Verify it produces ታት
13. Verify in each of three cases, the insertion point should end up before the final ት character.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall the Amharic keyboard.
3. Uninstall Notes App.

</details>

---

**TEST_178**

Test case for Safari_Google_Docs with Amharic,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `178`
- Product: `MAC`
- Source files: `178.JSON`, `178.html`

</details>

**Description**

Original TestLodge ID: TC118

<details>
<summary>Setup</summary>

1. Install Keyman build.
2. Install Amharic keyboard.
3. Install Safari browser.

</details>

<details>
<summary>Action</summary>

1. Click on the Safari browser icon from macOS Desktop.
2. Verify that the Safari browser opens.
3. Open Google document.
4. Type t a
5. Verify it produces ታ.
6. Type t, left-arrow, a
7. Verify it produces አት
8. Type t t, left-arrow, a
9. Verify it produces ታት
10. Type t t
11. Mouse-clicking between the two ታ characters.
12. Type a
13. Verify it produces ታት
14. Verify in each of three cases, the insertion point should end up before the final ት character.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.
2. Uninstall the Amharic keyboard.
3. Uninstall Safari browser.

</details>

---

**TEST_195**

Test case for Safari_URL_Bar with Shortcuts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `195`
- Product: `MAC`
- Source files: `195.JSON`, `195.html`

</details>

**Description**

Original TestLodge ID: TC135

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the Safari browser icon from macOS Desktop.
2. Verify that the Safari browser opens.
3. Click on the URL bar.
4. Press Command +  F (⌘+F) keys from magic keyboard.
5. Verify that the Find or Control dialog box opens.
6. Press Command + O (⌘-O) keys from magic keyboard.
7. Verify that the File opens.
8. Type ‘testing’ in the URL bar.
9. Press Command + A (⌘-A) keys from magic keyboard.
10. Verify that the text ‘testing’ has been selected.
11. Press Command + X (⌘-X) keys from magic keyboard.
12. Verify that the text ‘testing’ has been cut from the URL.
13. Press Command + V (⌘-V) keys from magic keyboard.
14. Verify that the text ‘testing’ has been pasted in the URL.
15. Press Command + C (⌘-C) keys from magic keyboard.
16. Verify that the text ‘testing’ has been copied from the URL.
17. Press Command + V (⌘-V) keys from magic keyboard.
18. Verify that the text ‘testing’  has been pasted in the URL.
19. Press Command + A (⌘-A) keys from magic keyboard.
20. Verify that the text ‘testing’ has been selected.
21. Type t
22. Verify it replaces selected characters with ት
23. Type a   t
24. Press Command + A (⌘-A) keys from magic keyboard.
25. Verify that the text ‘testing’ has been selected.
26. Type a
27. Verify it produces አ

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.

</details>

---

**TEST_198**

Test case for Safari_World_Online with Shortcuts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `198`
- Product: `MAC`
- Source files: `198.JSON`, `198.html`

</details>

**Description**

Original TestLodge ID: TC138

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the Safari browser icon from macOS Desktop.
2. Verify that the Safari browser opens.
3. Open ‘Word online’ document.
4. Press Command +  F (⌘+F) keys from magic keyboard.
5. Verify that the Find or Control dialog box opens.
6. Press Command + O (⌘-O) keys from magic keyboard.
7. Verify that the File opens.
8. Type ‘testing’ in the URL bar.
9. Press Command + A (⌘-A) keys
10. Verify that the text ‘testing’ has been selected.
11. Press Command + X (⌘-X) keys from magic keyboard.
12. Verify that the text ‘testing’ has been cut from the URL.
13. Press Command + V (⌘-V) keys from magic keyboard.
14. Verify that the text ‘testing’ has been pasted in the URL.
15. Press Command + C (⌘-C) keys from magic keyboard.
16. Verify that the text ‘testing’ has been copied from the URL.
17. Press Command + V (⌘-V) keys from magic keyboard.
18. Verify that the text ‘testing’  has been pasted in the URL.
19. Press Command + A (⌘-A) keys from magic keyboard.
20. Verify that the text ‘testing’ has been selected.
21. Type t
22. Verify it replaces selected characters with ት
23. Type a   t
24. Press Command + A (⌘-A) keys
25. Verify that the text ‘testing’ has been selected.
26. Type a
27. Verify it produces አ

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.

</details>

---

**TEST_205**

Test case for TextEdit_App with Shortcuts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `205`
- Product: `MAC`
- Source files: `205.JSON`, `205.html`

</details>

**Description**

Original TestLodge ID: TC145

<details>
<summary>Setup</summary>

1. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Click on the TextEdit App icon from macOS Desktop.
2. Verify that the TextEdit Application opens.
3. Press Command +  F (⌘+F) keys from magic keyboard.
4. Verify that the Find or Control dialog box opens.
5. Press Command + O (⌘-O) keys from magic keyboard.
6. Verify that the File opens.
7. Type ‘testing’ in the URL bar.
8. Press Command + A (⌘-A) keys from magic keyboard.
9. Verify that the text ‘testing’ has been selected.
10. Press Command + X (⌘-X) keys from magic keyboard.
11. Verify that the text ‘testing’ has been cut from the URL.
12. Press Command + V (⌘-V) keys from magic keyboard.
13. Verify that the text ‘testing’ has been pasted in the URL.
14. Press Command + C (⌘-C) keys from magic keyboard.
15. Verify that the text ‘testing’ has been copied from the URL.
16. Press Command + V (⌘-V) keys from magic keyboard.
17. Verify that the text ‘testing’  has been pasted in the URL.
18. Press Command + A (⌘-A) keys from magic keyboard.
19. Verify that the text ‘testing’ has been selected.
20. Type t
21. Verify it replaces selected characters with ት
22. Type a   t
23. Press Command + A (⌘-A) keys
24. Verify that the text ‘testing’ has been selected.
25. Type a
26. Verify it produces አ

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build on the device.

</details>
