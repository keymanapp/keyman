# Gestures Acceptance Tests

**TEST_287**

Test case for 10_KEY_ROTA,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `287`
- Product: `Gestures`
- Source files: `287.JSON`, `287.html`

</details>

**Description**

Original TestLodge ID: TC188

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select English - Classic 10-key using from Keyboard picker menu.
6. Verify that all of the key numbers 1-9 aside from 7 have three or more lower case letters as key hints.
7. Tap the 8 key, then wait for a second.
8. Tap the 8 key twice in quick succession, then wait for a second.
9. Verify the output ‘8a’ in the text input screen.
10. Tap the 8 key continuously for at least 2 seconds.
11. Verify that it  can rotate through : 8a8, 8aa, 8ab, 8ac, 8aA, 8aB, 8aC, then back to 8a8
12. Stop tapping when the new character is a capital letter.
13. Verify that the key hints are now capitalized
14. Double-tap the 9 key once.
15. Verify that “D”  is emitted as the result.
16. Repeatedly tap 9 for at least five seconds.
17. Verify that if a lowercase letter is the result, the key hints is lowercase.
18. Verify that if an uppercase letter is the result, the key hints is uppercase.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_288**

Test case for 10_KEY_DIACRITICS,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `288`
- Product: `Gestures`
- Source files: `288.JSON`, `288.html`

</details>

**Description**

Original TestLodge ID: TC189

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select English - Diacritic 10-key Rota using from Keyboard picker menu.
6. Verify that English - Diacritic 10-key Rota OSK appeared on the screen.
7. Tap the ◌̀ key. (The key between backspace [above] and enter [below] representing a diacritic.
8. Double-tap the 8 key.
9. Verify à letter appear on the screen.
10. Triple-tap the ◌̀ key.
11. Verify that a circumflex diacritic appeared over the à.
12. Triple-tap the 9 key.
13. Verify that the total result àê appeared on the screen.
14. Deselect the text-area element.
15. Reselect the text-area element, placing the point of text entry (the caret) at the end, after the ê. (Note for future test-maintenance : the point is to trigger a context-reset.)
16. Double-tap the 8 key.
17. Verify that àềa appeared on the screen.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_289**

Test case for APP_10_KEY_DIACRITICS,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `289`
- Product: `Gestures`
- Source files: `289.JSON`, `289.html`

</details>

**Description**

Original TestLodge ID: TC190

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Download this KMP : https://jahorton.github.io/diacritic_rota.kmp
4. Install it for use within the Keyman app.
5. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Tap the 7 key twice.
2. Verify that 77 is output on the screen.
3. Tap the ◌̀ key.
4. Double-tap the 8 key.
5. Verify that the total result 77à appeared on the screen.
6. Triple-tap the ◌̀ key.
7. Verify that a circumflex diacritic appeared over the à
8. Triple-tap the 9 key.
9. Verify that the total result 77àê appeared on the screen.
10. Tap the ◌̀ key.
11. Verify that a diacritic appeared over the ê.
12. Open the Keyman In-App Settings menu.
13. Close the in-app Settings menu.
14. Double-tap the 8 key.
15. Verify the output 77àềa appeared on the screen.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_290**

Test case for BASIC_SIMPLE_SHIFT,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `290`
- Product: `Gestures`
- Source files: `290.JSON`, `290.html`

</details>

**Description**

Original TestLodge ID: TC191

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select Norwegian - EuroLatin(SIL)  using the Keyboard picker menu.
6. Verify that the Norwegian - EuroLatin(SIL) OSK appeared on the screen.
7. Tap the Spacebar.
8. Verify that the keyboard is on the default (lowercase) layer after this.
9. Tap the Shift key.
10. Verify that the keyboard stays on the Shift layer.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_291**

Test case of BASIC_MODIPRESS,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `291`
- Product: `Gestures`
- Source files: `291.JSON`, `291.html`

</details>

**Description**

Original TestLodge ID: TC192

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select Norwegian - EuroLatin(SIL)  using the Keyboard picker menu.
6. Verify that the Norwegian - EuroLatin(SIL) OSK appeared on the screen.
7. Tap the Spacebar.
8. Tap and hold the Shift key.
9. Verify that the keyboard changed to the Shift layer.
10. Tap the A key.
11. Verify that it produces letter A on the Screen.
12. Release the Shift key.
13. Verify that the Keyboard changed to the default layer.
14. Repeat Steps from 8 to 13 quickly.
15. Holding Shift just long enough to tap the Shift layers A key
16. Release it quickly.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>
**TEST_292**

Test case for BASIC_MODIPRESS_HOLD,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `292`
- Product: `Gestures`
- Source files: `292.JSON`, `292.html`

</details>

**Description**

Original TestLodge ID: TC193

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select Norwegian - EuroLatin(SIL)  using the Keyboard picker menu.
6. Verify that the Norwegian - EuroLatin(SIL) OSK appeared on the screen.
7. Tap the Spacebar.
8. Tap and hold the Shift key.
9. Verify that the keyboard changed to the Shift layer.
10. Wait at least one second.
11. Release the Shift key.
12. Verify that the keyboard changed to the default layer as a result of Step 11.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_293**

Test case for NUMERIC_FROM_SHIFT,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `293`
- Product: `Gestures`
- Source files: `293.JSON`, `293.html`

</details>

**Description**

Original TestLodge ID: TC194

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select Norwegian - EuroLatin(SIL)  using the Keyboard picker menu.
6. Verify that the Norwegian - EuroLatin(SIL) OSK appeared on the screen.
7. Tap the Spacebar.
8. Verify that the keyboard is on the default (lowercase) layers after this.
9. Tap the Shift key.
10. Verify Keyboard is in the Shift layer.
11. Tap and hold the numeric key (123) underneath the Shift key.
12. Verify that the keyboard changed to the numeric layer - numbers in the top row, mathematical symbols elsewhere.
13. Long press the % key.
14. Select ‱  subkey.
15. Verified that  ‱  is emitted as text.
16. Release the numeric key.
17. Verify that the keyboard changed back to the Shift layer.
18. Tap the numeric key, releasing it immediately.
19. Repeat from 13 to 15, long pressing the % key.
20. Select the ‱  subkey.
21. Verify that the keyboard automatically changed back to the default (lowercase)layer.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_294**

Test case for DELAYED_SUBKEY,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `294`
- Product: `Gestures`
- Source files: `294.JSON`, `294.html`

</details>

**Description**

Original TestLodge ID: TC195

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select Norwegian - EuroLatin(SIL)  using the Keyboard picker menu.
6. Verify that the Norwegian - EuroLatin(SIL) OSK appeared on the screen.
7. Tap the Spacebar.
8. Tap the Shift key.
9. Verify that the Shift layer is active.
10. Tap and hold the numeric (123) key underneath the Shift key.
11. Verify that the keyboard changed to the numeric layer - numbers in the top row, mathematical symbols elsewhere.
12. Long press the % key.
13. Release the numeric key.
14. Verify that the keyboard changed remained on the numeric layer, with the subkey menu still displayed.
15. Select the ‱ subkey.
16. Verify that ‱ is emitted as text and that the layer has changed back to the Shift layer.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_295**

Test case for DOUBLETAP_CAPS,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `295`
- Product: `Gestures`
- Source files: `295.JSON`, `295.html`

</details>

**Description**

Original TestLodge ID: TC196

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select Norwegian - EuroLatin(SIL)  using the Keyboard picker menu.
6. Verify that the Norwegian - EuroLatin(SIL) OSK appeared on the screen.
7. Type “All”.
8. Double-tap the Shift key.
9. Type “CAPS”.
10. Tap the Shift key.
11. Type “Well” without touching the Shift key at any point.
12. Verify the final result, in total : All CAPS word Well

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_297**

Test case for ALTERNATING_SHIFT_AND_KEY,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `297`
- Product: `Gestures`
- Source files: `297.JSON`, `297.html`

</details>

**Description**

Original TestLodge ID: TC198

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select “English - Gesture Prototyping”  using the Keyboard picker menu.
6. Verify that the English - Gesture Prototyping OSK appeared on the screen.
7. Tap and hold the SHIFT key.
8. Tap the H key and release it.
9. Release the SHIFT key.
10. Verify that an H is output on the text screen.
11. Verify that the layer returns to default after step 7.
12. Repeat steps 7-10 five times.
13. Repeat steps 7-9 quickly for at least 10 seconds.
14. Tap the numeric (123) key.
15. Verify that the layer switches properly to keys with numbers in the top row and symbols in the middle two rows.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_300**

Test case for FLICK_LOCKING,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `300`
- Product: `Gestures`
- Source files: `300.JSON`, `300.html`

</details>

**Description**

Original TestLodge ID: TC201

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select “English - Gesture Prototyping”  using the Keyboard picker menu.
6. Verify that the English - Gesture Prototyping OSK appeared on the screen.
7. Tap and hold the ‘u’ key.
8. Drag it to a straight up position.
9. Verify that  the û key is in the preview area.
10. Verify that the animation is not “jumpy”.
11. Return your finger to its original position.
12. Verify that the ‘u’ key should like back into place.
13. Drag your finger in an up-and-right motion.
14. Verify that you should also see a preview for ú as you do so.
15. Verify that the animation is not “jumpy”.
16. Lift your finger and end the gesture, while the ú is visible and in the center.
17. Verify that a ú is output.
18. Tap and hold the a key.
19. Drag it to a straight up position.
20. Verify that the â key is in the preview area.
21. Verify that the animation is not “jumpy”.
22. Return your finger to its original position.
23. Verify that the ‘a’ key should like back into place.
24. Drag your finger in an up-and-right motion.
25. Verify that a preview for ú appeared.
26. Verify that the animation is not “jumpy”.
27. Lift your finger and end the gesture, while the ú is visible and in the center.
28. Verify that a á is output.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

**TEST_302**

Test case for FLICK_DURING_MODIPRESS,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `302`
- Product: `Gestures`
- Source files: `302.JSON`, `302.html`

</details>

**Description**

Original TestLodge ID: TC203

<details>
<summary>Setup</summary>

1. Android Mobile is ready to test.
2. Install Keyman latest Stable build from Keyman.com site.
3. Install Chrome browser.
4. KeymanWeb Test Home link is available for testing.

</details>

<details>
<summary>Action</summary>

1. Click the “Test unminified Keymanweb” link button.
2. Verify that ‘KeymanWeb Sample Page - Unminified Source’ page opens.
3. Click the text  input screen.
4. Click the globe key.
5. Select “English - Gesture Prototyping”  using the Keyboard picker menu.
6. Verify that the English - Gesture Prototyping OSK appeared on the screen.
7. Tap and hold the SHIFT key.
8. Tap and hold the U key.
9. Drag it purely to a straight up position.
10. Verify that  the Û key in the preview area.
11. Verify that the animation is not “jumpy”.
12. Release the SHIFT key.
13. Verify that the Shift layer (all uppercase) remains visible.
14. Verify that the flick animation is still active.
15. Return the finger to its original position.
16. Verify that the ‘U’ key  slid back into place.
17. Drag the finger in an up-and-right motion.
18. Verify that a preview for Ú . appeared
19. Verify that the animation is not “jumpy”.
20. Tap on the Q key, while Ú the is visible and in the center.
21. Verify that the keyboard returns to the default (lowercase) layer.
22. Verify that it shows either Ú or Úq as the output.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the Search bar (in Task bar)
2. Verify that the ‘Control panel’ appeared in the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the the Keyman Uninstallation completed successfully

</details>

---

