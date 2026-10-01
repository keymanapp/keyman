# Android Acceptance Tests

Generated from the numbered HTML/JSON regression-test export.

**TEST_347**

Add back button to "Adjust Keyboard Height "menu

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `347`
- Product: `Android`
- Source files: `347.JSON`, `347.html`

</details>

**Description**

TC - fix(android): Add back button to "Adjust Keyboard Height "menu #13645

<details>
<summary>Setup</summary>

1.    Install the latest Keyman (.apk)build in the Android device or emulator.
2.    Accept all the Android permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch Keyman for Android.
2.    Dismiss the Get Started menu
3.    Open the "Adjust keyboard height" by clicking the Keyman's Settings --> Adjust keyboard height
4.    Verify that the "Adjust keyboard height" title bar has a back button on the left.
5.    Drag the keyboard to adjust the keyboard height and increase keyboard height.
6.    Verify that the keyboard height and observe it(note)
7.    Press the back button (<--) on the title bar to exit back to the app.
8.    Verify that the keyboard height now matches what was set in the "Adjust keyboard height" menu.
9.    Verify that the back button works to save the adjust of the keyboard height on the "Adjust keyboard height"

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon on the Android device/emulator.
2.    Verify that the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the first voice appears on the list.
5.    Click the first voice option.
6.    Verify that the first voice menu opens.
7.    Click the uninstall option.
8.    Verify that a first voice warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the first voice Uninstallation completed successfully.

</details>

---

**TEST_334**

Cancel flicks when both returning to and releasing at original tap location_TEST_FLICK_GENERAL_USE

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `334`
- Product: `Android`
- Source files: `334.JSON`, `334.html`

</details>

**Description**

TC - change(web): cancel flicks when both returning to and releasing at original tap location_TEST_FLICK_GENERAL_USE #13683

<details>
<summary>Setup</summary>

1.    Install the latest Keymanan (.apk) build on the Android device or emulator.
2.    Accept all the Android permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Select the "Eurolatin" keyboard by clicking the globe key.
3.    Click on the "t" key and then pull down.
4.    Verify that the flick gesture is working fine and the output is correct.

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon on the Android device/emulator.
2.    Verify that the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the Keyman appears on the list.
5.    Click the Keyman option.
6.    Verify that the Keyman menu opens.
7.    Click the uninstall option.
8.    Verify that a Keyman warning message, “Do you want to uninstall this app?” appears with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the Keyman uninstallation completed successfully.

</details>

---

**TEST_337**

Cancel flicks when both returning to and releasing at original tap location_TEST_FLICK_RESET_FULL

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `337`
- Product: `Android`
- Source files: `337.JSON`, `337.html`

</details>

**Description**

TC - change(web): cancel flicks when both returning to and releasing at original tap location_TEST_FLICK_RESET_FULL #13683

<details>
<summary>Setup</summary>

1.    Install the latest Keymanan (.apk) build on the Android device or emulator.
2.    Accept all the Android permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Select the "Eurolatin" keyboard by clicking the globe key.
3.    Flick down the t key.
4.    Verify that the 5 appears.
5.    Flick back up to the center of the key and the 5 change back to t
6.    Flick down again, and the 5 appear in the preview again
7.    Release the key.
8.    Verify that the key has output 5.

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon on the Android device/emulator.
2.    Verify that the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the Keyman appears on the list.
5.    Click the Keyman option.
6.    Verify that the Keyman menu opens.
7.    Click the uninstall option.
8.    Verify that a Keyman warning message, “Do you want to uninstall this app?” appears with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the Keyman uninstallation completed successfully.

</details>

---

**TEST_332**

Check network access before trying to download keyboard

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `332`
- Product: `Android`
- Source files: `332.JSON`, `332.html`

</details>

**Description**

fix(android): Check network access before trying to download keyboard #13978

<details>
<summary>Setup</summary>

1.    Install the latest Keymanan (.apk) build on the Android device or emulator.
2.    Accept all the Android permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Go to Settings> Installed Keyboard or Dictionary > Install from keyman.com,
3.    Verify that the keyboard search page appears.
4.    Search the 'Khmer Angkor' and install it
5.    Successfully download 'Khmer Angkor' keyboard and dictionaries
6.    Go to Settings> Installed Keyboard or Dictionary > Install from keyman.com, search for 'IPA (SIL)'
7.    Verify that the keyboard search page appears.
8.    Search for the 'IPA (SIL)' and install it.
9.    Successfully downloaded 'IPA (SIL)' keyboard and dictionaries
10.    Turn on airplane mode
11.    Click 'Install Keyboard'
12.    Verify a toast message as "No internet connection"

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon on the Android device/emulator.
2.    Verify that the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the Keyman appears on the list.
5.    Click the Keyman option.
6.    Verify that the Keyman menu opens.
7.    Click the uninstall option.
8.    Verify that a Keyman warning message, “Do you want to uninstall this app?” appears with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the Keyman uninstallation completed successfully.

</details>

---

**TEST_344**

Don't report missing kmp.json

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `344`
- Product: `Android`
- Source files: `344.JSON`, `344.html`

</details>

**Description**

TC - fix(android): Don't report missing kmp.json #13411

<details>
<summary>Setup</summary>

1.    Install the latest Keymanan (.apk) build on the Android device or emulator.
2.    Accept all the Android permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."
8.    Download and save the file on your mobile device using the link below.
https://darcywong00.github.io/examples/invalid/khmer10/khmer10_noJson.kmp

</details>

<details>
<summary>Action</summary>

1.    Launch Keyman for Android.
2.    Install the downloaded *.kmp file by following these steps.
3.    From the Keyman settings --> Install Keyboard or Dictionary --> Install from local file --> browse to where khmer10_noJson.kmp was saved (in mobile).
4.    Verify that the toast notification appears and contains the message "No keyboards or predictive text to install."

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon on the Android device/emulator.
2.    Verify that the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the Keyman appears on the list.
5.    Click the Keyman option.
6.    Verify that the Keyman menu opens.
7.    Click the uninstall option.
8.    Verify that a Keyman warning message, “Do you want to uninstall this app?” appears with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the Keyman uninstallation completed successfully.

</details>

---

**TEST_148**

feat(android): Add controls for auto-correct #12443

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `148`
- Product: `Android`
- Source files: `148.JSON`, `148.html`

</details>

**Description**

Original TestLodge ID: TC283

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in the Android device or emulator.
2. Accept all the Android permission requests for storage.
3. Open the Keyman app.
4. Navigate to the settings page.
5. Check the "Enable Keyman as system-wide keyboard" box.
6. Check the "Set the keyboard as the default keyboard" box.
7. Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1. Launch the Keyman app.
2. Enter some text in the keyman's notepad.
3. Verify the default "sil_euro_latin" keyboard displays suggestions.
4. Launch the Chrome browser.
5. Verify the browser appears.
6. Navigate to the "editpad.org" website.
7. Enter some text on the page.
8. Verify the default "sil_euro_latin" keyboard displays suggestions.
9. Return to the Keyman app --> Keyman settings --> Installed languages --> English to bring up the "English Settings" menu
10. Verify the "Predictions with corrections" is set as the default suggestion radio selection.
11. Select the "Predictions only"
12. Verify "Predictions only" is the only selected option
13. Select "Disable suggestions"
14. Verify "Disable suggestions" is the only select option.
15. Exit the menu and then return to Keyman Notepad.
16. Verify the suggestion banner is now disabled and it is showing the Keyman banner.
17. Launch the Chrome browser.
18. Verify the browser appears.
19. Navigate to the "editpad.org" website.
20. Enter some text on the page.
21. Verify the suggestion banner is now disable.
22. Verify the Keyman banner is showing on the OSK.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device/emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the keyman appears on the list.
5. Click the Keyman option.
6. Verify the Keyman menu opens.

</details>

---

**TEST_142**

feat(android): Enhance how ENTER key is handled in apps #12125

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `142`
- Product: `Android`
- Source files: `142.JSON`, `142.html`

</details>

**Description**

Original TestLodge ID: TC276

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk) build on an Android device / emulator.
2. Accept android permission requests for storage
3. Open the Keyman app.
4. Navigate to the settings page.
5. Checked the "Enable Keyman as system-wide keyboard" box.
6. Checked the "Set the keyboard as the default keyboard" box.
7. Enable the "Predictions" to install the "Dictionary."
1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage
3. Open the keyman app.
4. Navigate to the settings page.
5. Checked the "Enable Keyman as system-wide keyboard" box.
6. Checked the "Set the keyboard as the default keyboard" box.
7. Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

Test_Google_Keep:
1. Launch the Google Keep app.
2. Open the empty note by clicking the "+" button.
3. Verify that the keyman keyboard appeared on the Keep App.
4. Enter a sentence using the keyman keyboard.
5. Press the "Enter" key.
6. Verify that an empty line was inserted under the sentence.
Test_Facebook_Messaging:
1. Launch the Facebook Messenger app.
2. Select a person from the contact list.
3. Verify that the keyman keyboard appeared on the Facebook Messenger app.
4. Enter a sentence using the keyman keyboard.
5. Press the "Enter" key.
6. Verify that the empty line was inserted under the sentence.
7. Click the arrow key.
8. Verify that the message was sent to the user.
9. Verify that the blank line was removed from the sent message.
Test_Twitter_Search:
1. Launch the Twitter app.
2. Open the search box by clicking the eyeglass icon.
3. Verify that the keyman keyboard appeared on the Twitter app.
4. Enter a word in the search field.
5. Press the "Enter" key.
6. Verify that the search result appears after pressing the "Enter" key.
1. Launch Keyman App.
2. Navigate to the settings page by clicking the ellipsis button.
3. Verify the Settings menu opens.
4. Verify the "Adjust long-press delay" option appears.
5. Click the "Adjust long-press delay" option.
6. Verify that the "Adjustment point" and arrow buttons appear.
7. Set the delay as 0.3 seconds.
8. Verify that the "Delay Time" text shows after changing the delays.
9. Navigate back to the keyman's notepad page.
10. Verify that the default keyboard(e.g. EuroLatin SIL) appears
11. Press any letter key and then hold it.
12. Verify that the submenu appears after 0.3 seconds of delays.
13. Navigate to the settings page by clicking the ellipsis button.
14. Verify the Settings menu opens.
15. Verify the "Adjust long-press delay" option appears.
16. Click the "Adjust long-press delay" option.
17. Verify that the "Adjustment point" and arrow buttons appear.
18. Set the delay as 1.5 seconds.
19. Verify that the "Delay Time" text shows after changing the delays.
20. Navigate back to the keyman's notepad page.
21. Verify that the default keyboard(e.g. EuroLatin SIL) appears
22. Press any letter key and then hold it.
23. Verify that the submenu appears after 1.5 seconds of delays.
24. Select the letters from the submenu.
25. Press any letter key and then pull down it.
26. Verify that the letter appears after pulling down the letter.
27. Verify that the flicker word is correct.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device or emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the keyman appears on the list.
5. Click the Keyman option.
6. Verify the Keyman menu opens.
7. Click the uninstall option.
8. Verify that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation is completed successfully.
4) feat(android): Add menu to specify long-press delay #12170
1. Click the Settings icon from the Android device/emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman appears on the list.
5. Click the Keyman option.
6. Verify the Keyman menu opens.
7. Click the uninstall option.
8. Verify that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation is completed successfully.

</details>

---

**TEST_144**

fix(android): Auto-mirror back and forward arrows for RTL support ? #12227

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `144`
- Product: `Android`
- Source files: `144.JSON`, `144.html`

</details>

**Description**

Original TestLodge ID: TC278

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in the Android device or emulator.
2. Accept all the Android permission requests for storage.
3. Open the Keyman app.
4. Navigate to the settings page.
5. Check the "Enable Keyman as system-wide keyboard" box.
6. Check the "Set the keyboard as the default keyboard" box.
7. Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1. Launch the Keyman app.
2. Installed the "Arabic Phonetic(SIL)" keyboard by clicking Settings/Install Keyboard/Install from keyman.com.
3. Verify the "Arabic Phonetic(SIL)" keyboard appears.
4. Navigate to the settings page by clicking the ellipsis button.
5. Verify the Settings menu opens.
6. Change the UI language from English to Arabic by selecting the "Display language.".
7. Verify the navigation and return back to the "Keyman" notepad.
8. Verify the ellipsis and share buttons appear at the left side on top.
9. Verify the keyman icon appears on the right side on top.
10. Added a sentence to the notepad using the "Arabic Phonetic(SIL)" keyboard.
11. Verify the text appears from right to left.
12. Navigate to the settings page by clicking the ellipsis button.
13. Verify that the "back arrow" appears on the right side.
14. Go to "Installed Languages.".
15. Verify that the "back arrow" appears on the right side.
16. Verify that the "forward arrow" for installed keyboards points to the left side.
17. Go to the EuroLatin (SIL) keyboard settings.
18. Verify that the "back arrow" appears on the right side.
19. Verify that the "forward arrow" for installed keyboards points to the left side.
20. Go to the English dictionary (MTNT) Settings
21. A blue tick appears on the right side.
22. Verify that the "back arrow" appears on the right side
23. Verify that the "forward arrow" for installed keyboards points to the left side.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device/emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the keyman appears on the list.
5. Click the Keyman option.
6. Verify the Keyman menu opens.
7. Click the uninstall option.
8. Verify that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation is completed successfully.

</details>

---

**TEST_145**

fix(android): Auto-mirror increment and decrement arrows for RTL support ? #12230

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `145`
- Product: `Android`
- Source files: `145.JSON`, `145.html`

</details>

**Description**

Original TestLodge ID: TC279

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build on an Android device/emulator.
2. Accept all the Android permission requests for storage
3. Open the Keyman app.
4. Navigate to the settings page.
5. Check the "Enable Keyman as system-wide keyboard" box.
6. Check the "Set the keyboard as the default keyboard" box.
7. Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1. Launch the Keyman app.
2. Installed the "Arabic Phonetic(SIL)" keyboard by clicking Settings/Install Keyboard/Install from keyman.com
3. Verify the "Arabic Phonetic(SIL)" keyboard appears.
4. Navigate to the settings page by clicking the ellipsis button.
5. Verify the Settings menu opens.
6. Change the UI language from English to Arabic by selecting the "Display language".
7. Verify the navigation and return back to the "Keyman" notepad.
8. Verify the ellipsis and share buttons appear on the left side at the top.
9. Verify the keyman icon appears on the right side, on top.
10. Added a sentence to the notepad using the "Arabic Phonetic(SIL)" keyboard.
11. Verify the text appears from right to left.
12. Open the "Text size" dialog by clicking the ellipsis button.
13. Verify “A-” pointing right makes the text size smaller when clicking it.
14. Verify “A+” pointing left makes the text size bigger when clicking it.
15. Verify the text size changes when doing steps 13 and 14.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon on the Android device or emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the keyman appears on the list.
5. Click the Keyman option.
6. Verify the Keyman menu opens.
7. Click the uninstall option.
8. Verify that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation is completed successfully.

</details>

---

**TEST_147**

fix(android): Hide suggestion banner on password fields #12442

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `147`
- Product: `Android`
- Source files: `147.JSON`, `147.html`

</details>

**Description**

Original TestLodge ID: TC282

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk) build in the Android device or emulator.
2. Accept all the Android permission requests for storage.
3. Open the Keyman app.
4. Navigate to the settings page.
5. Check the "Enable Keyman as system-wide keyboard" box.
6. Check the "Set the keyboard as the default keyboard" box.
7. Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1. Launch the Keyman app.
2. Verify the suggestion banner provides suggestions while typing text in the Keyman's Note app.
3. Launch the Chrome browser.
4. Verify the browser appears.
5. Navigate to the "editpad.org" website.
6. Enter some text on the page.
7. Verify the keyman's keyboard appears.
8. Verify the suggestion banner appears on the Keyman's banner.
9. Open a new tab on the Chrome browser.
10. Navigate to the https://darcywong00.github.io/examples/form.html page.
11. Verify the test page appears.
12. Click in the "Visible Text Field" box.
13. Verify the OSK displays suggestions when typing text.
14. Click in the "Password Field.".
15. Verify the OSK disable suggestions.
16. Verify the "Keyman banner" is showing on the OSK.
17. Click on the "Search Field.".
18. Verify the OSK re-enabled suggestions when typing text.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device/emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the keyman appears on the list.
5. Click the Keyman option.
6. Verify the Keyman menu opens.

</details>

---

**TEST_327**

Handle banner overrides independently when device is locked and unlocked

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `327`
- Product: `Android`
- Source files: `327.JSON`, `327.html`

</details>

**Description**

TC - fix(android): Handle banner overrides independently when device is locked and unlocked #13546

<details>
<summary>Setup</summary>

1.    Install the latest Keyman (.apk)build in the Android device or emulator.
2.    Accept all the Android permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."
8.    Install the basic Lao keyboard.

</details>

<details>
<summary>Action</summary>

1.    Open the Keyman
2.    Select the sil_euro_latin keyboard by pressing the globe key.
3.    Verify that the suggestions appears on the banner
4.    Use the globe key to switch to the Basic Lao keyboard
5.    Verify that the image banner appears
6.    Launch Chrome and browse to "https://darcywong00.github.io/examples/form.html" test page. (It has basic text fields and a password text field)
7.    Verify that the page load correctly
8.    Click in the "Visible Text Field" box.
9.    Use the globe key to select the sil_euro_latin keyboard
10.    Verify suggestions appears on the banner
11.    Use the globe key to switch to the Basic Lao keyboard
12.    Verify that the image banner appears
13.    Use the globe key to select sil_euro_latin keyboard
14.    Verify suggestions appears on the banner
15.    Click in the "Password Field" box.
16.    Verify the suggestions are disable and the image banner appears
17.    Return the Keyman app
18.    Verify that the sil_euro_latin keyboard appeared
19.    Verify suggestions appears in the app
20.    Make sure the Keyman app is the active app
21.    Lock the device
22.    Unlock the device - use Keyman to enter the password.
23.    Verify suggestions are disable on the lock screen.
24.    When the device is unlock and the Keyman app is resume.
25.    Verify that the sil_euro_latin keyboard appears.
26.    Verify suggestions appears and showing the text.

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon from the Android device / emulator.
2.    Verify the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the Keyman appears on the list.
5.    Click the Keyman option.
6.    Verify the Keyman menu opens.
7.    Click the uninstall option.
8.    Verify that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_321**

Keyman website appeared when clicking the "+" button on the keyboard settings page.

Status: `Deprecated`

<details>
<summary>Metadata</summary>

- Source ID: `321`
- Product: `Android`
- Source files: `321.JSON`, `321.html`

</details>

**Description**

TC - bug(android): Keyman website appeared when clicking the "+" button on the keyboard settings page. #12797

<details>
<summary>Setup</summary>

No steps recorded.

</details>

<details>
<summary>Action</summary>

No steps recorded.

</details>

<details>
<summary>Cleanup</summary>

No steps recorded.

</details>

---

**TEST_329**

Re-order host page script load so Sentry can report script load failures

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `329`
- Product: `Android`
- Source files: `329.JSON`, `329.html`

</details>

**Description**

TC - change(android): re-order host page script load so Sentry can report script load failures #13333

<details>
<summary>Setup</summary>

1.    Install the latest Keyman (.apk)build in the Android device or emulator.
2.    Accept all the Android permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Navigate to the "Search the keyboard" by pressing Keyman settings --> Install Keyboard or Dictionary --> Install from keyman.com.
3.    Installed multiple language keyboards. (Khmer_angkor, euro_latin. sil_ipa)
4.    Open the "Keyboard Picker" by pressing the "globe" key.
5.    Select another keyboard.
6.    Close the keyboard picker.
7.    Verify that the Keyman app does not close.
8.    Verify that the selected keyboard appears.
9.    Enter some sentences using the "EutoLatin" keyboard.
10.    Verify that the suggested words appear on the banner.
11.    Change the mobile view from portrait to landscape view.
12.    Verify that the Keyman keyboard appears in default size.
13.    Change the mobile view from landscape to portrait view.
14.    Launch the Chrome browser.
15.    Navigate to the Google search box(text area)
16.    Verify that the Keyman keyboard appeared.

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon from the Android device / emulator.
2.    Verify the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the Keyman appears on the list.
5.    Click the Keyman option.
6.    Verify the Keyman menu opens.
7.    Click the uninstall option.
8.    Verify that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_307**

Revert how keyboard picker menu launches

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `307`
- Product: `Android`
- Source files: `307.JSON`, `307.html`

</details>

**Description**

fix(android/engine): Revert how keyboard picker menu launches #12986

<details>
<summary>Setup</summary>

1.    Install the latest Keyman (.apk)build in the Android device or emulator.
2.    Accept all the Android permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Chrome browser.
2.    Navigate to the "editpad.org" website.
3.    Verified that the website loaded.
4.    Click in the text area.
5.    Verified that the keyman keyboard appeared
6.    Long-press on the globe key to open the "Keyman Keyboard" picker menu.
7.    Verified that the "Keyman keyboard" list appeared
8.    Close the "Keyman Keyboard" picker menu by pressing the back button.
9.    Verified that It returns to the Chrome browser.
10.    Verified that the mouse cursor appeared in the text area.

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon from the Android device / emulator.
2.    Verify the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the Keyman Alpha appears on the list.
5.    Click the Keyman Alpha option.
6.    Verify the Keyman Alpha menu opens.
7.    Click the uninstall  option.
8.    Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_319**

TC - fix(android): Catch WebView exceptions and prompt user to install WebView_TEST_INSTALL_CHROME

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `319`
- Product: `Android`
- Source files: `319.JSON`, `319.html`

</details>

**Description**

TC - fix(android): Catch WebView exceptions and prompt user to install WebView_TEST_INSTALL_CHROME #13140

<details>
<summary>Setup</summary>

1.    Install the latest Keyman (.apk)build in the  Android emulator of API 21 (Android 5.1)
2.    Enable the Android System WebView.
3.    Go to System Settings --> Apps --> All apps --> Android System WebView
4.    On the Android SystemWebView page.
5.    Click "Disable" as directed.
6.    Exit Settings

</details>

<details>
<summary>Action</summary>

1.    Open the Keyman app.
2.    Dismiss the "Get Started" window.
3.    Verify that the keyboard does not display. (Ignore the keyboard Toast error which is a Javascript error )
4.    Verify that the Keyman app has a prompt for the user to "Update Chrome"
5.    Click the "Update Chrome" button on the keyman app.
6.    Verify that the device goes to the Play Store page for Chrome.

</details>

<details>
<summary>Cleanup</summary>

1.    Click the Settings icon from the Android device / emulator.
2.    Verify the Settings menu opens.
3.    Click the Apps option.
4.    Verify that the Keyman Alpha appears on the list.
5.    Click the Keyman Alpha option.
6.    Verify the Keyman Alpha menu opens.
7.    Click the uninstall  option.
8.    Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9.    Click the OK button.
10.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_117**

Test case for adjust the OSK height for the current orientation (portrait or landscape),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `117`
- Product: `Android`
- Source files: `117.JSON`, `117.html`

</details>

**Description**

Original TestLodge ID: TC67

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Click the three dots button which appears at the top right corner of the text pane.
7. Click the Settings option.
8. Click ‘Adjust keyboard height’ option
9. Verify that the ‘Adjust keyboard height’ view opens.
10. Drag the OSK height to change the keyboard height.
11. Click back.
12. Verify that OSK is refreshed to the selected height.
13. Click the Settings option.
14. Click ‘reset to defaults’ button.
15. Click the Back button.
16. Verify the OSK reverts to the original height.
17. Rotate the device back to portrait orientation.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_137**

Test case for BROKEN_SUGGESTION selecting the "undo" suggestion breaks subsequent input (7167)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `137`
- Product: `Android`
- Source files: `137.JSON`, `137.html`

</details>

**Description**

Original TestLodge ID: TC87

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Open Keyman In-App.
2. Type P r o b in the text area using the touchpad.
3. Verify that the predictive text “Problem” appears in the banner.
4. Select Problem from banner.
5. Press Backspace key.
6. Select "Prob" (undoing the last suggestion).
7. Press Spacebar.
8. Verify the word Prob appears in the banner.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_116**

Test case for CHANGING_DISPLAY_LANGUAGE,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `116`
- Product: `Android`
- Source files: `116.JSON`, `116.html`

</details>

**Description**

Original TestLodge ID: TC66

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Click the three dots button which appears at the top right corner of the text pane.
7. Click the Settings option.
8. Click the ‘Display language’ option from the Settings dialog.
9. Select ‘Khmer’
10. Verify that the ‘Get Started’ dialog opens successfully.
11. Verify that the UI has been changed to ‘Khmer’
12. Verify menu selections in Khmer
13. Click the three dots button which appears at the top right corner of the text pane.
14. Verify the Keyman menu in Khmer.
15. Verify Keyman text field has Khmer prompt (for “Start typing here”)
16. Click the Settings option.
17. Click the ‘Change Display Language’
18. Select “English”
19. Verify the application reloads in English language.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_113**

Test case for Clearing text in the Keyman text pane,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `113`
- Product: `Android`
- Source files: `113.JSON`, `113.html`

</details>

**Description**

Original TestLodge ID: TC63

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Type ‘This is the first sentence’ in the text pane.
7. Click the three dots button which appears at the top right corner of the text pane.
8. Verify that the Keyman menu opens.
9. Click the Clear text option.
10. Verify that the entire text has been removed from the text pane.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_135**

Test case for FLICKER_BANNER unnecessary flicker on banner while typing (7162)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `135`
- Product: `Android`
- Source files: `135.JSON`, `135.html`

</details>

**Description**

Original TestLodge ID: TC85

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. In the Keyman app with the default sil_euro_latin keyboard and lexical model enabled, start typing "Fascinating"
2. observe whether or not the suggestion banner is flickering on each keystroke.
3. If flickering happens, FAIL this test.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_109**

Test case for Keyman Settings menu,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `109`
- Product: `Android`
- Source files: `109.JSON`, `109.html`

</details>

**Description**

Original TestLodge ID: TC59

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes.
6. Click the three dots button from the top right corner of the view.
7. Click the ‘Settings’ option from the Keyman menu.
8. Click the Installed languages option.
9. Verify that the English keyboard appears in the menu list.
10. Click the English Keyboard.
11. Verify the English Settings menu appears.
12. Click ‘EuroLatin (SIL) Keyboard option.
13. Verify Info appears with keyboard version, help link, and QR code
14. Click on version and verify update, if keyboard update is available,
15. Click on Help link , if help link is available,
16. Verify keyboard help documentation appears
17. Click the OK button.
18. Verify the page returns to English Settings menu
19. Select Dictionary --> English dictionary (MTNT)
20. Verify the Dictionary info appears
21. Click the Back button on the English dictionary page.
22. Verify it returns to the English Settings menu.
23. Turn off 'Enable Predictions'
24. Verify that the ‘Enable Corrections’ is automatically disabled.
25. Click the Back button three times from the Keyman App.
26. Verify that the ‘Predictive texts’ banner is not visible upon the OSK.
27. Turn on ‘Enable Predictions’
28. Verify that the ‘Enable Corrections’ is automatically enabled.
29. Click the Back button three times from the Keyman App.
30. Verify that the ‘Predictive texts banner’ is visible upon the OSK.
31. Click ‘Dictionary’ (English dictionary (MTNT) in the English Settings view.
32. Verify the English dictionaries view appears.
33. Click on the ‘English dictionary (MTNT)’ option.
34. Verify ‘Dictionary : English dictionary (MTNT) menu opens.
35. Click the Uninstall dictionary option.
36. Verify that a warning message ‘Would you like to delete this dictionary? Appears with Cancel and Delete buttons.
37. Click the Delete button.
38. Verify that the ‘Deleted’ message appears on the menu.
39. Click the Back button three times from the Keyman app.
40. Verify that the Predictive text banner is not visible upon the Keyboard.
41. Click ‘Dictionary’ (English dictionary (MTNT) in the English Settings view.
42. Verify the English dictionaries view appears.
43. Click on the ‘English dictionary (MTNT)’ option.
44. Verify that a warning message ‘ Dictionary added’ appears on the view.
45. Click the Back button three times from the Keyman app.
46. Verify that the English dictionary (Predictive text) banner appears upon the OSK.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_124**

Test case for Landscape to Portrait Orientation in the Keyman app,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `124`
- Product: `Android`
- Source files: `124.JSON`, `124.html`

</details>

**Description**

Original TestLodge ID: TC74

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Rotate the device to portrait orientation.
7. Verify the On Screen Keyboard rotates properly.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_130**

Test case for Landscape to Portrait Orientation in the Keyman System Keyboard,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `130`
- Product: `Android`
- Source files: `130.JSON`, `130.html`

</details>

**Description**

Original TestLodge ID: TC80

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Rotate the device to a landscape orientation.
7. Verify the On Screen Keyboard rotates properly.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_123**

Test case for Portrait to Landscape Orientation in the Keyman app,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `123`
- Product: `Android`
- Source files: `123.JSON`, `123.html`

</details>

**Description**

Original TestLodge ID: TC73

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Rotate the device to a landscape orientation.
7. Verify the On Screen Keyboard rotates properly.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_129**

Test case for Portrait to Landscape Orientation in the Keyman System Keyboard,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `129`
- Product: `Android`
- Source files: `129.JSON`, `129.html`

</details>

**Description**

Original TestLodge ID: TC79

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Rotate the device to a landscape orientation.
7. Verify the On Screen Keyboard rotates properly.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_140**

Test case for Press_And_Hold_Backspace (Holding backspace appears to desync context between web and app (7172),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `140`
- Product: `Android`
- Source files: `140.JSON`, `140.html`

</details>

**Description**

Original TestLodge ID: TC90

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the Mobile device / emulator.
2. Verify that the Keyman Application opens.
3. Type “This is the first sentence”.
4. Hold the Backspace key.
5. Verify that all the letters are getting deleted one by one.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_141**

Test case for Rapid_Typing (Rapid typing on first caps letter sometimes gives TWo cap letters) (7173),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `141`
- Product: `Android`
- Source files: `141.JSON`, `141.html`

</details>

**Description**

Original TestLodge ID: TC91

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the Mobile device / emulator.
2. Verify that the Keyman application opens.
3. Type “T w o” rapidly and consecutively multiple times separated by a space. (while on the Shift layer either in the middle or beginning of the sentence).
4. Verify “T w o” should appear multiple times on the text area.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_110**

Test case for sharing a text to an external application (eg., Notes App),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `110`
- Product: `Android`
- Source files: `110.JSON`, `110.html`

</details>

**Description**

Original TestLodge ID: TC60

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes.
6. Type ‘This is the first sentence’ in the text pane.
7. Select ‘sentence’ text.
8. Verify that a pop up menu displays with ‘SHARE’ option.
9. Click the SHARE option.
10. Verify that some apps like ‘Messages, Copy to clipboard, Gmail, Notes etc.,’ appear on the screen.
11. Click the Notes app.
12. Verify that a message ‘sentence’  appears on the Notes pane.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_118**

Test case for SPACEBAR_CAPTION,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `118`
- Product: `Android`
- Source files: `118.JSON`, `118.html`

</details>

**Description**

Original TestLodge ID: TC68

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Click the three dots button which appears at the top right corner of the text pane.
7. Click the Settings option.
8. Click ‘Spacebar caption’
9. Verify the spacebar caption menu opens.
10. Select the  ‘Language’ radio button.
11. Click the back button.
12. Verify that the space bar label is ‘English’
13. Click the Settings option.
14. Click ‘Spacebar caption’
15. Verify the spacebar caption menu opens.
16. Select the ‘Keyboard’ radio button.
17. Click the Back button.
18. Verify the space bar label is ‘EuroLatin(SIL)’
19. Click the Settings option.
20. Click ‘Spacebar caption’
21. Verify the spacebar caption menu opens.
22. Select the ‘Language+Keyboard’’ radio button.
23. Click the Back button.
24. Verify the space bar label is ‘English - EuroLatin(SIL)’
25. Click the Settings option.
26. Click ‘Spacebar caption’
27. Verify the spacebar caption menu opens.
28. Select the ‘Blank’’ radio button.
29. Click the Back button.
30. Verify the space bar label is blank.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_121**

Test case for type on the OSK would show the expected output English(EuroLatin SIL),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `121`
- Product: `Android`
- Source files: `121.JSON`, `121.html`

</details>

**Description**

Original TestLodge ID: TC71

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Type ‘qwer’ using the OSK.
7. Verify that ‘qwerty’ word should not appear on the suggestion banner.
8. Type 'Run' using the OSK.
9. Verify the word starts with 'Run' 'Running' appears on the suggestion banner.
10. Select 'Run' word from the suggestion banner.
11. Verify that the word 'Run' appears on the text input screen.
12. Short-press a key ‘a’ from the OSK.
13. Verify that the base key ‘a’ appears on the text pane.
14. Long-press a key ‘a’.
15. Verify that the Subkey menu of ‘a’ appears above the ‘a’ key.
16. Select a long-press key from the menu and release it.
17. Verify that the long-press key should appear on the text pane.
18. Long-press a key, while keeping the finger down, move off the long-press options, and release the key.
18. Verify it should output any letters on the text pane.
19. Long press-press a key, while keeping the finger down, move off the long-press options, then move back on a long-press option so it’s highlighted,and release.
20. Verify it should output the long-press key.
21. Quickly type a long paragraph (eg., repeat the word “reply”).
22. Verify long-press keys don’t get stuck (displayed when not touching a key).

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation is completed successfully.

</details>

---

**TEST_127**

Test case for type on the System Keyboard would show the expected output English(EuroLatin SIL),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `127`
- Product: `Android`
- Source files: `127.JSON`, `127.html`

</details>

**Description**

Original TestLodge ID: TC77

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Type ‘qwer’ using the OSK.
7. Verify that ‘qwerty’ word should not appear on the suggestion banner.
8. Type 'Run' using the OSK.
9. Verify that the word starts with 'Run' 'Running' appears on the suggestion banner.
10. Select 'Run' from the suggestion bar.
11. Verify that the word 'Run' appears on the text input screen.
12. Short-press a key ‘a’ from the OSK.
13. Verify that the base key ‘a’ appears on the text pane.
14. Long-press a key ‘a’.
15. Verify that the Subkey menu of ‘a’ appears above the ‘a’ key.
16. Select a long-press key from the menu and release it.
17. Verify that the long-press key should appear on the text pane.
18. Long-press a key, while keeping the finger down, move off the long-press options, and release the key.
19. Verify it should output any letters on the text pane.
20. Long press-press a key, while keeping the finger down, move off the long-press options, then move back on a long-press option so it’s highlighted,and release.
21. Verify it should output the long-press key.
22. Quickly type a long paragraph (eg., repeat the word “reply”).
23. Verify long-press keys don’t get stuck (displayed when not touching a key).

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_120**

Test case for verify OSK is visible and fills the width the bottom of the screen in Landscape orientation, [English (EuroLatin (SIL)],

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `120`
- Product: `Android`
- Source files: `120.JSON`, `120.html`

</details>

**Description**

Original TestLodge ID: TC70

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Long-press  on the q key
7. Verify the sub key menu of the q key appears.
8. Long-press the p key.
9. Verify the sub key menu of the p key appears.
10. Click the SHIFT key
11. Verify uppercase layer appears on the Keyboard.
12. Click the number (123) key.
13. Verify number layer can be selected via 123
14. Long press 1 key.
15. Verify sub key menu of 1 appears.
16. Long press 0 key
17. Verify the sub key menu of 0 appears
18. Click the Backspace key.
19. Verify it deletes one letter or one step backward on the text pane.
20. Click the Spacebar key.
21. Verify it moves one step forward on the text pane.
22. Click the Enter key.
23. Verify the Cursor moves to the next line in the text pane.
24. Click the three dots which appear at the top right corner of the text pane.
25. Click the Installed Keyboard or Dictionary option from the Settings menu.
26. Click Install from keyman.com option from the Install Keyboard or Dictionary view.
27. Verify the Keyboard Search bar appear on the screen.
28. Type ‘Khmer Angkor’ in the search bar.
29. Verify the Khmer Angkor keyboard appears under results.
30. Click on the Khmer Angkor.
31. Verify the Install Keyboard green button appears.
32. Click the Install Keyboard button.
33. Verify a toast notification ‘downloading keyboard package khmer_angkor.kmp’ appears on the screen.
34. Click the Install button from the Install Keyboard dialog.
35. Verify the Khmer Angkor Keyboard Installed message appears.
36. Verify the Welcome to khmer_angkor keyboard appears on the screen.
37. Click the OK button.
38. Verify the Khmer Angkor Keyboard appears as the active keyboard.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_112**

Test case for verifying text size in the Keyman Text pane ,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `112`
- Product: `Android`
- Source files: `112.JSON`, `112.html`

</details>

**Description**

Original TestLodge ID: TC62

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Type ‘This is the first sentence’ in the text pane.
7. Click the three dots button which appear at the top right corner of the text pane.
8. Click the ‘Text Size’ option.
9. Verify that the Text Size dialog appear with the OK button.
10. Verify that the default text size is 16.
11. Set it to ‘’72’ maximum size
12. Click the OK button.
13. Verify that the text size of the text is changed to 72 the maximum size.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_132**

Test case for verifying the External european AZERTY Keyboard,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `132`
- Product: `Android`
- Source files: `132.JSON`, `132.html`

</details>

**Description**

Original TestLodge ID: TC82

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes.
6. Install French Basic Keyboard.
7. Verify the French Basic OSK appears on the Screen.
8. Type on the top letter row ‘azerty’, from the physical keyboard.
9. Verify ‘azerty’ appears on the text pane.
10. Type on the 102 key.
11. Verify < appears on the text pane.
12. Type shift+102 Key
13. Verify > appears on the text pane.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_114**

Test case for verifying the Information document in Keyman,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `114`
- Product: `Android`
- Source files: `114.JSON`, `114.html`

</details>

**Description**

Original TestLodge ID: TC64

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the ‘More info’ option from the Get Started  dialog box.
5. Verify that the ‘Keyman for Android 15.0 Help’ documentation is displayed.
6. Verify Keyman for Android version appears at the bottom of the screen.
7. Click the Close button.
8. Verify that the ‘Get Started’ dialog box appear on the screen.
9. Click the Close button.
10. Verify the ‘Get Started’ dialog closes.
11. Click the three dots button which appear at the top right corner of the text pane.
12. Verify that the keyman menu opens.
13. Click the ‘Info’ option.
14. Verify that the ‘Keyman for Android 15.0 Help’ documentation is displayed.
15. Verify Keyman for Android version appears at the bottom of the screen.
16. Click the Close button.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_126**

Test case for verifying the System Keyboard in the Landscape mode (English (EuroLatin SIL),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `126`
- Product: `Android`
- Source files: `126.JSON`, `126.html`

</details>

**Description**

Original TestLodge ID: TC76

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click the Keyman App from an Android device.
2. Verify that the Keyman opens successfully
3. Verify that the ‘Get Started’ dialog opens successfully.
4. Click the close button from the Get Started dialog.
5. Verify that the Get Started dialog closes
6. Long-press  on the q key
7. Verify the sub key menu of the q key appears.
8. Long-press the p key.
9. Verify the sub key menu of the p key appears.
10. Click the SHIFT key
11. Verify uppercase layer appears on the Keyboard.
12. Click the number (123) key.
13. Verify number layer can be selected via 123
14. Long press 1 key.
15. Verify sub key menu of 1 appears.
16. Long press 0 key
17. Verify the sub key menu of 0 appears
18. Click the Backspace key.
19. Verify it deletes one letter or one step backward on the text pane.
20. Click the Spacebar key.
21. Verify it moves one step forward on the text pane.
22. Click the Enter key.
23. Verify the Cursor moves to the next line in the text pane.
24. Click the three dots which appear at the top right corner of the text pane.
25. Click the Installed Keyboard or Dictionary option from the Settings menu.
26. Click Install from keyman.com option from the Install Keyboard or Dictionary view.
27. Verify the Keyboard Search bar appear on the screen.
28. Type ‘Khmer Angkor’ in the search bar.
29. Verify the Khmer Angkor keyboard appears under results.
30. Click on the Khmer Angkor.
31. Verify the Install Keyboard green button appears.
32. Click the Install Keyboard button.
33. Verify a toast notification ‘downloading keyboard package khmer_angkor.kmp’ appears on the screen.
34. Click the Install button from the Install Keyboard dialog.
35. Verify the Khmer Angkor Keyboard Installed message appears.
36. Verify the Welcome to khmer_angkor keyboard appears on the screen.
37. Click the OK button.
38. Verify the Khmer Angkor Keyboard appears as the active keyboard.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_133**

Test case for verifying  KMP distribution,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `133`
- Product: `Android`
- Source files: `133.JSON`, `133.html`

</details>

**Description**

Original TestLodge ID: TC83

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage
3. Download KhmerAngkor.kmp file from keyman.com site.

</details>

<details>
<summary>Action</summary>

1. Click the Settings option from an Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Storage option from the Settings menu.
4. Click the Apps option.
5. Verify that it opens Apps storage menu.
6. Click the Keyman Alpha option.
7. Verify that Clear storage and Clear cache options are appearing in the Storage menu.
8. Click the Clear Storage option.
9. Verify that “Delete app data?” warning message appears with Cancel and OK buttons.
10. Click the OK button.
11. Verify that the Cache and User data should be cleared.
12. Click the Keyman icon.
13. Verify that the Keyman In-App opens.
14. Click the “Get Started” close button.
15. Click the Settings option.
16. Verify that the Settings menu opens.
17. Click the “Install Keyboard or Dictionary” option.
18. Click the “Install from local file” option.
19. Click the “khmer_angkor.kmp” file.
20. Verify kmp distribution successfully installs.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_107**

Test case for ‘Get Started’ menu from a fresh installation,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `107`
- Product: `Android`
- Source files: `107.JSON`, `107.html`

</details>

**Description**

Original TestLodge ID: TC57

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click on the Keyman App icon from the display
2. Verify the "Get Started" menu is visible
3. Touch "Add a keyboard for your language"
4. Verify "Install Keyboard or Dictionary" menu appears
5. Click the back arrow to return to "Get Started"
6. Touch "Enable Keyman as system-wide keyboard"
7. Verify Android system menus appear for controlling virtual keyboard
8. Click or enable Keyman as a system keyboard.
9. Verify that a Warning message appears on the Screen with the OK button.
10. Click the "OK" button to dismiss the dialogs.
11. Touch the ‘Back’ button
12. Verify that the "Get Started" dialog box appears again.
13. Verify "Enable Keyman as system-wide keyboard" now has a ticked checkbox.
14. Touch "Set Keyman as default keyboard"
15. Select ‘Keyman’ from the ‘Change Keyboard’ dialog.
16. Verify on the "Get Started" menu that "Set Keyman as default keyboard" now has a ticked checkbox.
17. Touch "More info" option.
18. Verify the Info page appears with a version string at the bottom of the page.
19. Touch the Back button (<-) to return to "Get Started" menu.
20. Uncheck the last option 'Show "Get Started" on startup'
21. Click the Close button from the Get Started menu dialog.
22. Click the Back button from the Android device.
23. Verify that the Keyman app closes from the Android device.
24. Click the Keyman app from the Android device.
25. Verify the "Get Started" menu does not appear.

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_106**

Test case to verify History file that contains all the current changes,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `106`
- Product: `Android`
- Source files: `106.JSON`, `106.html`

</details>

**Description**

Original TestLodge ID: TC56

<details>
<summary>Setup</summary>

1. Install the latest Keyman (.apk)build in Android device / emulator.
2. Accept all the Android permission requests for storage

</details>

<details>
<summary>Action</summary>

1. Click on the Keyman App from the Android device.
2. Verify that the Keyman Application opens.
3. Verify that the ‘Get Started’ pop up menu appears.
4. Click the ‘More Info’ option.
5. Verify that the Keyman for Android 15.0 Help page opens.
6. Click the ‘Version History’ link.
7. Verify that the Version History menu opens.
8. Click the ‘help.keyman.com/version-history’ link.
9. Verify that the Keyman Version History by platform page opens.
10. Click the ‘Keyman for Android Version History’ link.
11. Verify HISTORY.md contains all the current changes

</details>

<details>
<summary>Cleanup</summary>

1. Click the Settings icon from the Android device / emulator.
2. Verify the Settings menu opens.
3. Click the Apps option.
4. Verify that the Keyman Alpha appears on the list.
5. Click the Keyman Alpha option.
6. Verify the Keyman Alpha menu opens.
7. Click the uninstall  option.
8. Verify  that a Keyman warning message “Do you want to uninstall this app?” with Cancel and OK buttons.
9. Click the OK button.
10. Verify that the the Keyman Uninstallation completed successfully

</details>
