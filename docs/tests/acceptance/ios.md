# IOS Acceptance Tests

Generated from the numbered HTML/JSON regression-test export.

**TEST_356**

Allow swipe-dismissal of Settings to be cancelled_TEST_BUTTON_DISMISSAL

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `356`
- Product: `IOS`
- Source files: `356.JSON`, `356.html`

</details>

**Description**

TC - fix(ios): allow swipe-dismissal of Settings to be cancelled_TEST_BUTTON_DISMISSAL #13686

<details>
<summary>Setup</summary>

1.    Install the latest Keyman version(*.dmg) on the iPhone or iPad device/simulator.
2.    Accept all the iOS permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Dismiss the "Get Started" dialog.
3.    Open Keyman Settings by clicking the three dots --> Settings
4.    Tap on the Adjust Keyboard Height.
5.    Verify that the "Adjust Keyboard" page appears
6.    Change the height of the keyboard to 50% height of the screen.
7.    Verify that the keyboard template changed.
8.    Click the  "Back" button on the "Adjust Keyboard"
9.    Click the "Done" on the "Keyman settings" page
10.    Verify that the keyboard appears automatically, with no need to tap in the editor page.
11.    Confirm that the keyboard height is changed to 50% height of the screen.

</details>

<details>
<summary>Cleanup</summary>

1.    Long-press the Keyman icon from the display.
2.    Verify that the “Remove App” option appears on the screen.
3.    Click the “Remove App” option.
4.    Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5.    Click the ‘Delete App’ option.
6.    Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and Cancel’ button.
7.    Click the Delete button.
8.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_351**

Do not show notification for downloaded keyboard when showing its installer_TEST_REPRO_12590_SINGLE

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `351`
- Product: `IOS`
- Source files: `351.JSON`, `351.html`

</details>

**Description**

TC - fix(ios): do not show notification for downloaded keyboard when showing its installer_TEST_REPRO_12590_SINGLE #13687

<details>
<summary>Setup</summary>

1.    Install the latest Keyman (.dmg)build on the iPad device/simulator.
2.    Accept all the iOS permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Dismiss the "Get Started" dialog.
3.    Navigate to the "Installed Languages" page by clicking the three dots --> settings --> Installed Languages
4.    Click on the “+” icon at the top right.
5.    Search "ekwtamil99uni"
6.    Click on the "Thamizha Tamil99" keyboard.
7.    Verify that the keyboard information page appears.
8.    Click on the “Install keyboard” button.
9.    The screen closes and moves back to the “Installed Languages” page for a few seconds.
10.    Verify that the package-installer page appears with no effects of the issue.

</details>

<details>
<summary>Cleanup</summary>

1.    Long-press the Keyman icon from the display.
2.    Verify that the “Remove App” option appears on the screen.
3.    Click the “Remove App” option.
4.    Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5.    Click the ‘Delete App’ option.
6.    Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and Cancel’ buttons.
7.    Click the Delete button.
8.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_353**

Do not show notification for downloaded keyboard when showing its installer_TEST_REPRO_UPDATES_AVAILABLE

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `353`
- Product: `IOS`
- Source files: `353.JSON`, `353.html`

</details>

**Description**

TC - fix(ios): do not show notification for downloaded keyboard when showing its installer_TEST_REPRO_UPDATES_AVAILABLE #13687

<details>
<summary>Setup</summary>

1.    Install the older Keyman version( 18 stable version *.dmg) on the iPhone or iPad device/simulator.
2.    Accept all the iOS permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."
8.    Go to jahorton.github.io page.
9.    Download the khmer_angkor.kmp older version package.
10.    Install khmer_angkor.kmp the package which was downloaded earlier step.

</details>

<details>
<summary>Action</summary>

1.    Install the latest Keyman 19 version.
2.    Launch the Keyman app.
3.    Dismiss the "Get Started" dialog.
4.    Navigate to the "Installed Languages" page by clicking the three dots --> settings --> Installed Languages
5.    Return to the main screen and close Keyman from the back end.
6.    Reopen the Keyman app.
7.    Dismiss the "Get Started" dialog.
8.    Navigate to the "Installed Languages" page by clicking the three dots --> settings --> Installed Languages
9.    Click on the “+” icon at the top right.
10.    Search "ekwtamil99uni"
11.    Verified that the “Update Available” appears

</details>

<details>
<summary>Cleanup</summary>

1.    Long-press the Keyman icon from the display.
2.    Verify that the “Remove App” option appears on the screen.
3.    Click the “Remove App” option.
4.    Verify that a warning message appears “Remove Keyman? on the screen with ‘Delete App’ option.
5.    Click the ‘Delete App’ option.
6.    Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and Cancel’ button.
7.    Click the Delete button.
8.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_358**

Prevent re-display of keyboard when re-entering app to its menus_TEST_REPRO_13629

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `358`
- Product: `IOS`
- Source files: `358.JSON`, `358.html`

</details>

**Description**

fix(ios): prevent re-display of keyboard when re-entering app to its menus_TEST_REPRO_13629 #13630

<details>
<summary>Setup</summary>

1.    Install the latest Keyman version(*.dmg) in the iPhone or iPad device/simulator.
2.    Accept all the iOS permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Open the "settings" menu by clicking the "three dots"
3.    Select the "System Keyboard Settings" option.
4.    Verify that the "Keyman" page appeared on the iPhone.
5.    At the top-left corner of the device: use the "< Keyman" system option to return directly from the system's Settings app to the Keyman app.
6.    Verify that the page redirects to the Keyman app's "Keyman" page.
7.    Verify that the keyboard did not appear on the "Keyman" page.

</details>

<details>
<summary>Cleanup</summary>

1.    Long-press the Keyman icon from the display.
2.    Verify that the “Remove App” option appears on the screen.
3.    Click the “Remove App” option.
4.    Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5.    Click the ‘Delete App’ option.
6.    Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and Cancel’ button.
7.    Click the Delete button.
8.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_360**

Prevent re-display of keyboard when re-entering app to its menus_TEST_REPRO_EST_REPRO_6387

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `360`
- Product: `IOS`
- Source files: `360.JSON`, `360.html`

</details>

**Description**

fix(ios): prevent re-display of keyboard when re-entering app to its menus_TEST_REPRO_EST_REPRO_6387 #13630

<details>
<summary>Setup</summary>

1.    Install the latest Keyman version(*.dmg) on the iPhone or iPad device/simulator.
2.    Accept all the iOS permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Minimize the Keyman app using the "swipe gestures" (it stays under the recent).
3.    Launch your test device's Safari and visit https://jahorton.github.io.
4.    Download the sil_ipa.kmp option near the top.
5.    Go to the File app and then double-tap on the "sil_ipa.kmp" file.
6.    Verified that the Package installer appeared, and then the Keyboard did not appear on that.

</details>

<details>
<summary>Cleanup</summary>

1.    Long-press the Keyman icon from the display.
2.    Verify that the “Remove App” option appears on the screen.
3.    Click the “Remove App” option.
4.    Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5.    Click the ‘Delete App’ option.
6.    Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and Cancel’ button.
7.    Click the Delete button.
8.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_349**

Slide dismissal of keyboard-height adjuster will properly resize the keyboard_TEST_REPRO_13443

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `349`
- Product: `IOS`
- Source files: `349.JSON`, `349.html`

</details>

**Description**

TC - fix(ios): slide dismissal of keyboard-height adjuster will properly resize the keyboard_TEST_REPRO_13443 #13631

<details>
<summary>Setup</summary>

1.    Install the latest Keyman (.dmg)build on the iPad device/simulator.
2.    Accept all the iOS permission requests for storage.
3.    Open the Keyman app.
4.    Navigate to the settings page.
5.    Check the "Enable Keyman as system-wide keyboard" box.
6.    Check the "Set the keyboard as the default keyboard" box.
7.    Enable the "Predictions" to install the "Dictionary."

</details>

<details>
<summary>Action</summary>

1.    Launch the Keyman app.
2.    Dismiss the "Get Started" dialog.
3.    Open Keyman Settings -> Adjust Keyboard Height.
4.    Verify that the "Adjust Keyboard" page appears.
5.    Change the keyboard's height sufficiently to be noticed when you exit the settings.
6.    Swipe down on the sheet so that "Adjust Keyboard" is dismissed.
7.    Verify that the "Adjust Keyboard" returns to the main(text-editing view)
8.    Verify that the keyboard appears automatically, with no need to tap in the text area.
9.    Verify that the keyboard size change and matches the adjustment from step 5.

</details>

<details>
<summary>Cleanup</summary>

1.    Long-press the Keyman icon from the display.
2.    Verify that the “Remove App” option appears on the screen.
3.    Click the “Remove App” option.
4.    Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5.    Click the ‘Delete App’ option.
6.    Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ buttons.
7.    Click the Delete button.
8.    Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_153**

Test case for adding a new keyboard via Get Started menu,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `153`
- Product: `IOS`
- Source files: `153.JSON`, `153.html`

</details>

**Description**

Original TestLodge ID: TC95

<details>
<summary>Setup</summary>

1. Install latest Keyman(.ios) build in iPhone / iPad  device or Simulator.
2. Ensure the device or emulator is enabled with internet connection.

</details>

<details>
<summary>Action</summary>

1. Click on the Keyman icon from the display.
2. Verify that the Keyman application opens.
3. Verify that the 'Get Started’ menu opens.
4. Click the ‘Add keyboard for your language’ option from the Get Started menu dialog.
5. Verify that the Keyboard Search menu opens.
6. Enter ‘US Basic’ in the Search field.
7. Verify that the US Basic keyboard link opens.
8. Click the US Basic link button.
9. Verify that the US basic Keyboard menu opens.
10. Click the Install Keyboard button.
11. Verify that the US Basic Keyboard installed Successfully.
12. Verify that the US Basic Keyboard appears as the active keyboard in the text input screen.

</details>

<details>
<summary>Cleanup</summary>

1. Long press the Keyman icon from the display.
2. Verify that the “Remove App” option appears on the screen.
3. Click the “Remove App” option.
4. Verify that a warning message appears “Remove Keyman? on the screen with ‘Delete App’ option.
5. Click the ‘Delete App’ option.
6. Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ button.
7. Click the Delete button.
8. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_158**

Test case for appearing Text Size in the Input text Screen,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `158`
- Product: `IOS`
- Source files: `158.JSON`, `158.html`

</details>

**Description**

Original TestLodge ID: TC100

<details>
<summary>Setup</summary>

1. Install latest Keyman(.ios) build in iPhone / iPad  device or Simulator.
2. Ensure the device or emulator is enabled with internet connection.

</details>

<details>
<summary>Action</summary>

1. Click on the Keyman icon from the display.
2. Verify that the Keyman application opens.
3. Type “Keyman” on the Input text Screen.
4. Click the “Text Size” option from the Keyman menu list.
5. Verify that the Text Size option menu appears at the bottom of the Screen.
6. Set it to the Maximum Size “72”
7. Verify that the word “Keyman” appeared to be in big size in the Input text screen.
8. Set the Text Size to the minimum “16”
9. Verify that the word “Keyman” appeared to be in Small size in the Input text screen.

</details>

<details>
<summary>Cleanup</summary>

1. Long press the Keyman icon from the display.
2. Verify that the “Remove App” option appears on the screen.
3. Click the “Remove App” option.
4. Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5. Click the ‘Delete App’ option.
6. Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ button.
7. Click the Delete button.
8. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_170**

Test case for bug(iOS): predictive-text consistency after backspaces #10127,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `170`
- Product: `IOS`
- Source files: `170.JSON`, `170.html`

</details>

**Description**

Original TestLodge ID: TC215

<details>
<summary>Setup</summary>

1. Install Keyman's latest Stable build from the Keyman.com site.
2. Install sil_euro_latin  keyboard.

</details>

<details>
<summary>Action</summary>

1. Open Keyman In-App.
2. Type ‘somwwhat’.
3. Press the Backspace key until ‘somw’.
4. Verify the Predictions should be displayed on the suggestion banner.

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

**TEST_168**

Test case for for changing UI language into Amharic in the Keyman In-App,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `168`
- Product: `IOS`
- Source files: `168.JSON`, `168.html`

</details>

**Description**

Original TestLodge ID: TC110

<details>
<summary>Setup</summary>

1. Install latest Keyman(.ios) build in iPhone / iPad  device or Simulator.
2. Ensure the device or emulator is enabled with internet connection.

</details>

<details>
<summary>Action</summary>

1. Click the Settings icon from the iOS device / emulator.
2. Verify that the Settings menu opens.
3. Click the General option.
4. Verify  the General menu opens.
5. Click the Language & Religion option.
6. Click the Add language blue link button.
7. Type “Amharic” in the Search bar.
8. Verify the Amharic language appears on the Screen.
9. Click the Amharic language.
10. Verify that the ‘Prefer Amharic’ option appears on the Screen.
11. Click the Prefer Amharic option.
12. Verify the Amharic language has been updated on the device.
13. Click on the Keyman icon from the display.
14. Verify that the Keyman application opens.
15. Click the Settings option from the Keyman list menu.
16. Verify the UI language of the Keyman Settings menu has been changed to Amharic language.
17. Verify that the UI of the  Keyman menu list should appear in Amharic.

</details>

<details>
<summary>Cleanup</summary>

1. Long press the Keyman icon from the display.
2. Verify that the “Remove App” option appears on the screen.
3. Click the “Remove App” option.
4. Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5. Click the ‘Delete App’ option.
6. Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ button.
7. Click the Delete button.
8. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_160**

Test case for Info Help topic,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `160`
- Product: `IOS`
- Source files: `160.JSON`, `160.html`

</details>

**Description**

Original TestLodge ID: TC102

<details>
<summary>Setup</summary>

• Install latest Keyman(.ios) build in iPhone / iPad  device or Simulator.
• Ensure the device or emulator is enabled with internet connection.

</details>

<details>
<summary>Action</summary>

1. Click on the Keyman icon from the display.
2. Verify that the Keyman application opens.
3. Verify that the Get Started menu dialog opens along with the Keyman app.
4. Click More Info option from the Get Started menu dialog.
5. Verify that the “Keyman for iPhone and iPad 17.0 Help’ topic opens.
6. Click the Done button.
7. Verify that the “Keyman for..” help topic closes.
8. Click the Close (X) button which appears at the bottom of the help topic.
9. Verify that the “Keyman for..” help topic closes.
10. Click the Keyman list menu (three dots which appear at the top right corner of the screen).
11. Verify that the Info option appears in the Keyman list menu.
12. Click the Info option.
13. Verify that the “Keyman for iPhone and iPad 17.0 Help’ topic opens.
14. Click the Done button.
15. Verify that the “Keyman for ..” helpt topic closes.
16. Click the Close (X) button which appears at the bottom of the help topic.
17. Verify that the “Keyman for.. “ help topic closes.

</details>

<details>
<summary>Cleanup</summary>

1. Long press the Keyman icon from the display.
2. Verify that the “Remove App” option appears on the screen.
3. Click the “Remove App” option.
4. Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5. Click the ‘Delete App’ option.
6. Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ button.
7. Click the Delete button.
8. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_165**

Test case for Long Press on all layers in the On Screen Keyboard,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `165`
- Product: `IOS`
- Source files: `165.JSON`, `165.html`

</details>

**Description**

Original TestLodge ID: TC107

<details>
<summary>Setup</summary>

1. Install latest Keyman(.ios) build in iPhone / iPad  device or Simulator.
2. Ensure the device or emulator is enabled with internet connection.

</details>

<details>
<summary>Action</summary>

1. Click on the Keyman icon from the display.
2. Verify that the Keyman application opens.
3. Verify that EuroLatin (SIL) keyboard appeared as the active keyboard.
4. Verify the Shift layer is enabled.
5. Long press A Base key from the keyboard.
6. Verify that the Sub Key menu appears at the top of the Base key A.
7. Click one of the sub key letters.
8. Verify that the Sub Key letter appears in the input text screen.
9. Click the Shift Key.
10. Verify the Shift layer is disabled.
11. Long press ‘a’ Base key from the keyboard.
12. Verify that the Sub Key menu appears at the top of the Base Key a.
13. Click one of the sub key letters.
14. Verify that the Sub Key letter appears in the input text screen.
15. Click the Num key.
16. Long press 1 Base key.
17. Verify that the Sub key menu appears at the top of the Base key 1.
18. Click one of the sub key numbers.
19. Verify that the Sub key number appears in the input text screen.

</details>

<details>
<summary>Cleanup</summary>

1. Long press the Keyman icon from the display.
2. Verify that the “Remove App” option appears on the screen.
3. Click the “Remove App” option.
4. Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5. Click the ‘Delete App’ option.
6. Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ button.
7. Click the Delete button.
8. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_155**

Test case for Switch_Keyboard,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `155`
- Product: `IOS`
- Source files: `155.JSON`, `155.html`

</details>

**Description**

Original TestLodge ID: TC97

<details>
<summary>Setup</summary>

1. Install latest Keyman(.ios) build in iPhone / iPad  device or Simulator.
2. Ensure the device or emulator is enabled with internet connection.
3. Install US Basic Keyboard from Keyman.com Site.

</details>

<details>
<summary>Action</summary>

1. Click on the Keyman icon from the display.
2. Verify that the Keyman application opens.
3. Verify that “EuroLatin (SIL)” keyboard as the default keyboard.
4. Click the globe key.
5. Verify that the Keyboard picker menu.
6.  Click on “English  (US Basic)” keyboard.
7. Verify that the Keyboard should be changed to US Basic keyboard in the input text screen.
8. Click the globe key.
9. Verify the keyboard picker menu.
10. Click the “EuroLatin (SIL)” keyboard.
11. Verify that the keyboard switches back to “EuroLatin (SIL)” keyboard as the active keyboard in the input text screen.

</details>

<details>
<summary>Cleanup</summary>

1. Long press the Keyman icon from the display.
2. Verify that the “Remove App” option appears on the screen.
3. Click the “Remove App” option.
4. Verify that a warning message appears “Remove Keyman? on the screen with ‘Delete App’ option.
5. Click the ‘Delete App’ option.
6. Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ button.
7. Click the Delete button.
8. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_163**

Test case for US Basic Keyboard,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `163`
- Product: `IOS`
- Source files: `163.JSON`, `163.html`

</details>

**Description**

Original TestLodge ID: TC105

<details>
<summary>Setup</summary>

1. Install latest Keyman(.ios) build in iPhone / iPad  device or Simulator.
2. Ensure the device or emulator is enabled with internet connection.

</details>

<details>
<summary>Action</summary>

1. Click on the Keyman icon from the display.
2. Verify that the Keyman application opens.
3. Click the Settings option from the Keyboard list menu.
4. Verify the Keyman Settings page opens.
5. Click the Installed Languages option.
6. Verify that the Install Languages menu opens.
7. Click the (+) plus button which appears at the top right corner of the screen.
8. Type ‘US basic’ in the Keyboard Search bar.
9. Verify that the US Basic Keyboard link appears on the keyboard list.
10. Click the US Basic Keyboard link button.
11. Verify that the Install Keyboard button appears on the screen.
12. Click the Install Keyboard button.
13. Verify that the ‘English’ has been selected in the available languages list menu.
14. Click the Install button.
15. Click the Done button.
16. Verify that the ‘English - US Basic’  keyboard successfully installed and appeared as an active keyboard in the Keyman app.
17. Type ‘US basic’ in the Keyboard Search bar.
18. Verify that the US Basic Keyboard link appears on the keyboard list.
19. Click the US Basic Keyboard link button.
20. Verify that the Install Keyboard button appears on the screen.
21. Click the Install Keyboard button.
22. Click ‘Tagalog’ from the available languages list menu.
23. Click the Install button.
24. Click the Done button.
25. Verify that the ‘Tagalog - US Basic’ keyboard successfully installed in the Keyman app.
26. Set ‘English - US Basic’ keyboard as the active in-app keyboard.
27. Verify that predictive text suggestions appear.
28. Click the globe key.
29. Click the ‘Tagalog - US Basic’ keyboard.
30. Verify that predictive text suggestions do not appear.
31.  Click the number Key.
32. Verify the number keys and special key appear on the number layer.
33. Click the Special key.
34. Verify that the currency and symbol layer appear on the keyboard.
35. Select the currency layer, then press the £ key.
36. Verify that the keyboard outputs correctly and returns to the default layer.
37. Go to the symbol layer and press the © key.
38. Verify that the keyboard outputs correctly and remains on the symbol layer.
39. Revert to the default layer and ensure basic key inputs work.
40. Long-press e on the default layer and select a subkey.
41. Verify that the selected key produces the correct output.
42. Click the Setting option from the Keyboard list menu.
43. Verify that the Keyman Settings menu opens.
44. Click the Installed Languages option.
45. Verify that the Installed Languages menu opens.
46. Click the ‘English’ keyboard option.
47. Verify that the US Basic English keyboard appears under the EuroLatin (SIL) keyboard.
48. Click the US Basic English keyboard option.
49. Verify that the US Basic English keyboard menu opens.
50. Click the Keyboard Help option.
51. Verify that the US Basic English Keyboard help topic opens.
52. Click the Installed Languages option.
53. Verify that the Installed Languages menu opens.
54. Click the ‘Tagalog’ Keyboard option.
55. Verify that the Tagalog Settings menu opens.
56. Click the US Basic Keyboard option.
57. Verify that the Tagalog Settings / US Basic menu opens.
58. Click the Keyboard Help option.
59. Verify that the US Basic keyboard help topic opens.
60. scan the QR code with a phone
61. Verify that it links to the current version of that keyboard's public download page on keyman.com.
62. Click the Settings button from the Keyman list menu.
63. Click the Installed Languages option.
64. Click the English (2 keyboards installed) Keyboard option.
65. Verify that the English Settings menu opens.
66. Click the US Basic keyboard option.
67. Verify that the US Basic menu opens.
68. Click the Uninstall Keyboard option.
69. Verify that “would you like to uninstall this keyboard?” warning message appears with Cancel and Uninstall buttons.
70. Click the Uninstall button.
71. Verify that the US Basic - English Keyboard was successfully uninstalled from the Installed languages menu.

</details>

<details>
<summary>Cleanup</summary>

1. Long press the Keyman icon from the display.
2. Verify that the “Remove App” option appears on the screen.
3. Click the “Remove App” option.
4. Verify that a warning message appears “Remove Keyman? on the screen with the ‘Delete App’ option.
5. Click the ‘Delete App’ option.
6. Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ button.
7. Click the Delete button.
8. Verify that the Keyman Uninstallation completed successfully.

</details>

---

**TEST_150**

Test Case for “Get_Started” menu

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `150`
- Product: `IOS`
- Source files: `150.JSON`, `150.html`

</details>

**Description**

Original TestLodge ID: TC92

<details>
<summary>Setup</summary>

1. Install latest Keyman(.ios) build in iPhone / iPad  device or Simulator.
2. Ensure the device or emulator is enabled with internet connection.

</details>

<details>
<summary>Action</summary>

1. Click the Keyman icon from the screen.
2. Verify that the Keyman application opens.
3. Verify the “Get Started” menu appears along with the application.
4. Click the “Add a keyboard for your languages” option from the Get Started menu.
5. Verify that the ‘Install Keyboard or Dictionary’ menu opens.
6. Click the ‘Setup Keyman as system wide keyboard > Enable keyman’’ option from the Get Started menu.
7. Verify that it pulls up the keyman settings menu within iOS settings.
8. Click the ‘More Info’ option from the Get Started menu.
9. Verify that the “Keyman for iPhone and iPad 17.0 Help’ topic opens.
10. Enable “Don’t show again” toggle switch.
11. Click the “Done” option which appears at the top right corner of the menu.
12. Close the Keyman application.
13. Verify that the Keyman application is closed.
14. Click the Keyman icon.
15. Verify that the “Get Started” menu does not appear after the Keyman application opens.

</details>

<details>
<summary>Cleanup</summary>

1. Long press the Keyman icon from the display.
2. Verify that the “Remove App” option appears on the screen.
3. Click the “Remove App” option.
4. Verify that a warning message appears “Remove Keyman? on the screen with ‘Delete App’ option.
5. Click the ‘Delete App’ option.
6. Verify that another warning message “Delete Keyman? - Delete this app will also delete its data” appears on the Screen with ‘Delete’ and ‘Cancel’ button.
7. Click the Delete button.
8. Verify that the Keyman Uninstallation completed successfully.

</details>
