# Web Acceptance Tests

**TEST_334_334**

Cancel flicks when both returning to and releasing at original tap location_TEST_FLICK_GENERAL_USE

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `334`
- Product: `Web`
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

**TEST_337_337**

Cancel flicks when both returning to and releasing at original tap location_TEST_FLICK_RESET_FULL

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `337`
- Product: `Web`
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

**TEST_286**

fix(web): correctly handle cross-origin stylesheets when calculating keyboard size and key cap font size #11472

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `286`
- Product: `Web`
- Source files: `286.JSON`, `286.html`

</details>

**Description**

Original TestLodge ID: TC216

<details>
<summary>Setup</summary>

1. Install Keyman's latest stable build from the Keyman.com site.
2. Install the Chrome browser.

</details>

<details>
<summary>Action</summary>

1. Open the Chrome browser.
2. Open the developer tool by pressing the F12 key.
3. Verify that the "developer" tool pane appeared on the browser.
4. Click on the console tab.
5. Clear the existing log by pressing CTRL+L.
6. Paste the "https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/463931:id/index.html" URL in the address bar.
7. Verify that the "KeymanWeb Repo Index" page appeared.
8. Click the "View Keymanweb website-oriented manual test pages" link button.
9. Verify that the "KeymanWeb Testing" page appeared.
10. Verify that the URL changed to "/src/test/manual/web/index.html" in the address bar.
11. Click the "Tests keyboard documentation rendering" link button.
12. Verify that the URL changed to "/src/test/manual/web/build-visual-keyboard/index.html" in the address bar.
13. Verify that the "KeymanWeb: Keyboard documentation rendering" page is on the browser.
14. Verify that the page rendering happened successfully.
15. Verify that no error appeared on console tab.
16. Close the Chrome browser.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control Panel’ in the search bar (in the taskbar).
2. Verify that the ‘Control Panel’ appeared on the menu list.
3. Click the Control panel option.
4. Verify that the Control Panel dialog appeared.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman version from the list.
8. Click the Uninstall option.
9. Verify that the keyman uninstallation was completed successfully.

</details>

---

**TEST_312**

TC - fix(web): numpad + and - with zoom shortcut

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `312`
- Product: `Web`
- Source files: `312.JSON`, `312.html`

</details>

**Description**

TC - fix(web): numpad + and - with zoom shortcut use #12865

<details>
<summary>Setup</summary>

1.    Install Firefox in windows.
2.    Install Chrome in windows.
3.    Install Safari in macOS.
4.    Install Chrome in macOS.
5.    Install Firefox in macOS.
6.    Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1.    Open the "KeymanWeb Test Home" link on Chrome.
2.    Click on the "View Keymanweb website-oriented manual test pages." link
3.    Click on the "Test unminified Keymanweb" link
4.    Verify that the "KeymanWeb Sample Page - Unminified Source" page appears.
5.    Click in the text area box.
6.    Verify that the OSK appears for english.
7.    Select the "Norwegian - EuroLatin (SIL)" keyboard.
8.    Verify that the OSK appears for Norwegian - EuroLatin.
9.    Enter some "other letters"
10.    Select the text.
11.    Press the CTR + plus(+ on the numpad) using a physical keyboard.
12.    Verify that the web page's size increased. (zoom in)
13.    Press the CTR + minus(- on the numpad) using a physical keyboard.
14.    Verify that the web page's size decreased. (zoom out)

</details>

<details>
<summary>Cleanup</summary>

1.    Uninstall Firefox browser in Windows.
2.    Uninstall Chrome in Windows.
3.    Uninstall Safari in macOS.
4.    Uninstall Chrome in macOS.
5.    Uninstall Firefox in macOS.
6.    Uninstall Firefox in Linux OS.

</details>

---

**TEST_285**

Test case for (web): intermittent K_BKSP rule is deleting more than it should #9268,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `285`
- Product: `Web`
- Source files: `285.JSON`, `285.html`

</details>

**Description**

Original TestLodge ID: TC211

<details>
<summary>Setup</summary>

1. Install Keyman Developer's latest Stable build in a browser.
2.
Action Setup:
1. Open a project in Keyman Developer.
2. Click the Build tab
3. Click the ‘Test Keyboard on web’ button in the web debugger pane.
4. Verify that the localhost list appeared in the keyboard web pane
5. Select the Keyboard from the Keyboard dropdown list.
6. Type ‘x  /  k ‘
7. Select x letter
8. Press Backspace key.
9. Verify that it deletes only the x letter.

</details>

<details>
<summary>Action</summary>

No steps recorded.

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

**TEST_263**

Test case for Baseline_Add_Khmer_Angkor

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `263`
- Product: `Web`
- Source files: `263.JSON`, `263.html`

</details>

**Description**

Original TestLodge ID: TC164

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3.Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click ‘Add a keyboard by keyboard name’ dropdown list.
9. Select khmer_angkor keyboard.
10. Click the Add button.
11. Verified that the Khmer_Angkor keyboard appears on the screen.
12. Type Ctrl+Alt+B in the text input screen.
13. It produces ឞ in the screen.
14. Delete the character again.
15. Type ‘ s u j r k’ using the (US) keyboard.
16. It produces ស្រុក in the text input screen.
17. Hit the Backspace key.
18. It shows ស្រុ on the screen.
19. Hit the Backspace key.
20. It shows ស្រ in the screen.
21. Hit the Backspace key.
22. It shows ស on the screen.
23. Hit the Backspace key.
24. Verify it deletes the remaining letter on the screen.
25. Open Chrome browser in Windows OS.
22.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
23.Verify that the testing index page opens.
24. Click 'View Keymanweb use samples' link button.
25. Verify KeymanWeb Samples page opens.
26. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
27. Verify the corresponding KeymaWeb Sample page opens.
24. Click ‘Add a keyboard by keyboard name’ dropdown list.
25. Select khmer_angkor keyboard.
26. Click the Add button.
27. Verified that the Khmer_Angkor keyboard appears on the screen.
28. Type Ctrl+Alt+B in the text input screen.
29. It produces ឞ in the screen.
30. Delete the character again.
31. Type ‘ s u j r k’ using the (US) keyboard.
32. It produces ស្រុក in the text input screen.
33. Hit the Backspace key.
34. It shows ស្រុ on the screen.
35. Hit the Backspace key.
36. It shows ស្រ in the screen.
37. Hit the Backspace key.
38. It shows ស on the screen.
39. Hit the Backspace key.
40. Verify it deletes the remaining letter on the screen.
41. Open Safari browser in macOS
42. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
43.Verify that the testing index page opens.
44. Click 'View Keymanweb use samples' link button.
45. Verify KeymanWeb Samples page opens.
46. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
47. Verify the corresponding KeymaWeb Sample page opens.
48. Click ‘Add a keyboard by keyboard name’ dropdown list.
49. Select khmer_angkor keyboard.
50. Click the Add button.
51. Verified that the Khmer_Angkor keyboard appears on the screen.
52. Type Ctrl+Alt+B in the text input screen.
53. It produces ឞ in the screen.
54. Delete the character again.
55. Type ‘ s u j r k’ using the (US) keyboard.
56. It produces ស្រុក in the text input screen.
57. Hit the Backspace key.
58. It shows ស្រុ on the screen.
59. Hit the Backspace key.
60. It shows ស្រ in the screen.
61. Hit the Backspace key.
62. It shows ស on the screen.
63. Hit the Backspace key.
64. Verify it deletes the remaining letter on the screen.
61. Open Chrome browser in macOS.
62.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
63.Verify that the testing index page opens.
64. Click 'View Keymanweb use samples' link button.
65. Verify KeymanWeb Samples page opens.
66. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
67. Verify the corresponding KeymaWeb Sample page opens.
68. Click ‘Add a keyboard by keyboard name’ dropdown list.
69. Select khmer_angkor keyboard.
70. Click the Add button.
71. Verified that the Khmer_Angkor keyboard appears on the screen.
72. Type Ctrl+Alt+B in the text input screen.
73. It produces ឞ in the screen.
74. Delete the character again.
75. Type ‘ s u j r k’ using the (US) keyboard.
76. It produces ស្រុក in the text input screen.
77. Hit the Backspace key.
78. It shows ស្រុ on the screen.
79. Hit the Backspace key.
80. It shows ស្រ in the screen.
81. Hit the Backspace key.
82. It shows ស on the screen.
83. Hit the Backspace key.
84. Verify it deletes the remaining letter on the screen.
85. Open Firefox browser in macOS.
86.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
87.Verify that the testing index page opens.
88. Click 'View Keymanweb use samples' link button.
89. Verify KeymanWeb Samples page opens.
90. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
91. Verify the corresponding KeymaWeb Sample page opens.
92. Click ‘Add a keyboard by keyboard name’ dropdown list.
93. Select khmer_angkor keyboard.
94. Click the Add button.
95. Verified that the Khmer_Angkor keyboard appears on the screen.
96. Type Ctrl+Alt+B in the text input screen.
97. It produces ឞ in the screen.
98. Delete the character again.
99. Type ‘ s u j r k’ using the (US) keyboard.
100. It produces ស្រុក in the text input screen.
101. Hit the Backspace key.
102. It shows ស្រុ on the screen.
103. Hit the Backspace key.
104. It shows ស្រ in the screen.
105. Hit the Backspace key.
106. It shows ស on the screen.
107. Hit the Backspace key.
108. Verify it deletes the remaining letter on the screen.
101. Open Firefox browser in Linux OS.
102. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
103.Verify that the testing index page opens.
104. Click 'View Keymanweb use samples' link button.
105. Verify KeymanWeb Samples page opens.
106. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
107. Verify the corresponding KeymaWeb Sample page opens.
108. Click ‘Add a keyboard by keyboard name’ dropdown list.
109. Select khmer_angkor keyboard.
110. Click the Add button.
111. Verified that the Khmer_Angkor keyboard appears on the screen.
112. Type Ctrl+Alt+B in the text input screen.
113. It produces ឞ in the screen.
114. Delete the character again.
115. Type ‘ s u j r k’ using the (US) keyboard.
116. It produces ស្រុក in the text input screen.
117. Hit the Backspace key.
118. It shows ស្រុ on the screen.
119. Hit the Backspace key.
120. It shows ស្រ in the screen.
121. Hit the Backspace key.
122. It shows ស on the screen.
123. Hit the Backspace key.
124. Verify it deletes the remaining letter on the screen.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_277**

Test case for Baseline_Add_Khmer_Angkor (Touch / Hardware Platforms)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `277`
- Product: `Web`
- Source files: `277.JSON`, `277.html`

</details>

**Description**

Original TestLodge ID: TC178

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon from the Android Mobile.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click khmer_angkor from the ‘Add a keyboard by keyboard name’ section.
9. Click the Add button.
10. Verify that the Khmer Angkor keyboard appears on the Screen.
11. Type the following sequences on touch screen keyboard:
12. ស
13. ុ
14. ្រ (subkey of រ)
15. ក
16. Verify that the output produces ស្រុក.
17. Hit Backspace key. (as this is a reorder (keyboard) rule test)
18. Verify that it shows ស្រុ
19. Hit Backspace key.
20. Verify that it shows ស្រ
21. Hit Backspace key.
22. Verify that it shows ស
23. Hit Backspace key.
24. Verify that it shows an empty text area.
25. Click the Chrome browser icon from the Android mobile attached with a physical keyboard.
26. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
27. Verify that the testing index page opens.
28. Click 'View Keymanweb use samples' link button.
29. Verify KeymanWeb Samples page opens.
30. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
31. Verify the corresponding KeymaWeb Sample page opens.
32. Click khmer_angkor from the ‘Add a keyboard by keyboard name’ section.
33. Click the Add button.
34. Verify that the Khmer Angkor keyboard appears on the Screen.
35. Type the following (US) keys in the following sequences:
36. Type s
37. Type u
38. Type j
39. Type r
40. Type k
41. Verify that the output produces ស្រុក.
42. Hit Backspace key. (as this is a reorder (keyboard) rule test)
43. Verify that it shows ស្រុ
44. Hit Backspace key.
45. Verify that it shows ស្រ
46. Hit Backspace key.
47. Verify that it shows ស
48. Hit Backspace key.
49. Verify that it shows an empty text area.
50. Click the Safari browser in an iPhone Mobile.
51.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
52. Verify that the testing index page opens.
53. Click 'View Keymanweb use samples' link button.
54. Verify KeymanWeb Samples page opens.
55. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
56. Verify the corresponding KeymaWeb Sample page opens.
57. Click khmer_angkor from the ‘Add a keyboard by keyboard name’ section.
58. Click the Add button.
59. Verify that the Khmer Angkor keyboard appears on the Screen.
60. Type the following sequences on touch screen keyboard:
61. ស
62. ុ
63. ្រ (subkey of រ)
64. ក
65. Verify that the output produces ស្រុក.
66. Hit Backspace key. (as this is a reorder (keyboard) rule test)
67. Verify that it shows ស្រុ
68. Hit Backspace key.
69. Verify that it shows ស្រ
70. Hit Backspace key.
71. Verify that it shows ស
72. Hit Backspace key.
73. Verify that it shows an empty text area.
74. Click the Safari browser in an iPhone Mobile attached to a physical keyboard.
75. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
76. Verify that the testing index page opens.
77. Click 'View Keymanweb use samples' link button.
78. Verify KeymanWeb Samples page opens.
79. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
80. Verify the corresponding KeymaWeb Sample page opens.
81. Click khmer_angkor from the ‘Add a keyboard by keyboard name’ section.
82. Click the Add button.
83. Verify that the Khmer Angkor keyboard appears on the Screen.
84. Type the following (US) keys in the following sequences:
85. Type s
86. Type u
87. Type j
88. Type r
89. Type k
90. Verify that the output produces ស្រុក.
91. Hit Backspace key. (as this is a reorder (keyboard) rule test)
92. Verify that it shows ស្រុ
93. Hit Backspace key.
94. Verify that it shows ស្រ
95. Hit Backspace key.
96. Verify that it shows ស
97. Hit Backspace key.
98. Verify that it shows an empty text area.
99. Click Safari browser on an iPad device.
100. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
101. Verify that the testing index page opens.
102. Click 'View Keymanweb use samples' link button.
103. Verify KeymanWeb Samples page opens.
104. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
105. Verify the corresponding KeymaWeb Sample page opens.
106. Click khmer_angkor from the ‘Add a keyboard by keyboard name’ section.
107. Click the Add button.
108. Verify that the Khmer Angkor keyboard appears on the Screen.
109. Type the following sequences on touch screen keyboard:
110. ស
111. ុ
112. ្រ (subkey of រ)
113. ក
114. Verify that the output produces ស្រុក.
115. Hit Backspace key. (as this is a reorder (keyboard) rule test)
116. Verify that it shows ស្រុ
117. Hit Backspace key.
118. Verify that it shows ស្រ
119. Hit Backspace key.
120. Verify that it shows ស
121. Hit Backspace key.
122. Verify that it shows an empty text area.
123. Click Safari browser on an iPad device attached to a physical keyboard.
124. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
125. Verify that the testing index page opens.
126. Click 'View Keymanweb use samples' link button.
127. Verify KeymanWeb Samples page opens.
128. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
129. Verify the corresponding KeymanWeb Sample page opens.
130. Click khmer_angkor from the ‘Add a keyboard by keyboard name’ section.
131. Click the Add button.
132. Verify that the Khmer Angkor keyboard appears on the Screen.
133. Type the following (US) keys in the following sequences:
134. Type s
135. Type u
136. Type j
137. Type r
138. Type k
139. Verify that the output produces ស្រុក.
140. Hit Backspace key. (as this is a reorder (keyboard) rule test)
141. Verify that it shows ស្រុ
142. Hit Backspace key.
143. Verify that it shows ស្រ
144. Hit Backspace key.
145. Verify that it shows ស
146. Hit Backspace key.
147. Verify that it shows an empty text area.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Chrome browser in Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_262**

Test case for Baseline_Add_KM

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `262`
- Product: `Web`
- Source files: `262.JSON`, `262.html`

</details>

**Description**

Original TestLodge ID: TC163

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/433344:id/index.html' in the Search bar.
3.Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click ‘Add a keyboard by BCP-47 language code’ dropdown list.
9. Select km keyboard.
10. Verify that the Khmer Angkor keyboard is added and displayed on the Screen.
11. Open Chrome browser in Windows OS.
12.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
13.Verify that the testing index page opens.
14. Click 'View Keymanweb use samples' link button.
15. Verify KeymanWeb Samples page opens.
16. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
17. Verify the corresponding KeymaWeb Sample page opens.
18. Click ‘Add a keyboard by BCP-47 language code’ dropdown list.
19. Select km keyboard.
20. Verify that the Khmer Angkor keyboard is added and displayed on the Screen.
21. Open Safari browser in macOS.
22. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
23.Verify that the testing index page opens.
24. Click 'View Keymanweb use samples' link button.
25. Verify KeymanWeb Samples page opens.
26. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
27. Verify the corresponding KeymaWeb Sample page opens.
28. Click ‘Add a keyboard by BCP-47 language code’ dropdown list.
29. Select km keyboard.
30. Verify that the Khmer Angkor keyboard is added and displayed on the Screen.
31. Open Chrome browser in macOS.
32.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
33.Verify that the testing index page opens.
34. Click 'View Keymanweb use samples' link button.
35. Verify KeymanWeb Samples page opens.
36. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
37. Verify the corresponding KeymaWeb Sample page opens.
38. Click ‘Add a keyboard by BCP-47 language code’ dropdown list.
39. Select km keyboard.
40. Verify that the Khmer Angkor keyboard is added and displayed on the Screen.
41. Open Firefox browser in macOS.
42. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
43.Verify that the testing index page opens.
44. Click 'View Keymanweb use samples' link button.
45. Verify KeymanWeb Samples page opens.
46. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
47. Verify the corresponding KeymaWeb Sample page opens.
48. Click ‘Add a keyboard by BCP-47 language code’ dropdown list.
49. Select km keyboard.
50. Verify that the Khmer Angkor keyboard is added and displayed on the Screen.
51. Open Firefox browser in Linux OS.
52.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
53.Verify that the testing index page opens.
54. Click 'View Keymanweb use samples' link button.
55. Verify KeymanWeb Samples page opens.
56. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
57. Verify the corresponding KeymaWeb Sample page opens.
58. Click ‘Add a keyboard by BCP-47 language code’ dropdown list.
59. Select km keyboard.
60. Verify that the Khmer Angkor keyboard is added and displayed on the Screen.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_276**

Test case for Baseline_Add_KM (Touch / Hardware Platforms),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `276`
- Product: `Web`
- Source files: `276.JSON`, `276.html`

</details>

**Description**

Original TestLodge ID: TC177

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon from the Android Mobile.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click km from the ‘Add a keyboard by BCP-47 language code’ section.
9. Click the Add button.
10. Verify that the khmer_angkor keyboard should be added and appear on the Screen.
11. Click the Chrome browser in Android Mobile attached with a physical keyboard.
12. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
13. Verify that the testing index page opens.
14. Click 'View Keymanweb use samples' link button.
15. Verify KeymanWeb Samples page opens.
16. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
17. Verify the corresponding KeymaWeb Sample page opens.
18. Click km from the ‘Add a keyboard by BCP-47 language code’ section.
19. Click the Add button.
20. Verify that the khmer_angkor keyboard should be added and appear on the Screen.
21. Click Safari browser in an iPhone Mobile.
22. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
23. Verify that the testing index page opens.
24. Click 'View Keymanweb use samples' link button.
25. Verify KeymanWeb Samples page opens.
26. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
27. Verify the corresponding KeymaWeb Sample page opens.
28. Click km from the ‘Add a keyboard by BCP-47 language code’ section.
29. Click the Add button.
30. Verify that the khmer_angkor keyboard should be added and appear on the Screen.
31. Click Safari browser in an iPhone Mobile with a physical keyboard.
32. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
33. Verify that the testing index page opens.
34. Click 'View Keymanweb use samples' link button.
35. Verify KeymanWeb Samples page opens.
36. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
37. Verify the corresponding KeymaWeb Sample page opens.
38. Click km from the ‘Add a keyboard by BCP-47 language code’ section.
39. Click the Add button.
40. Verify that the khmer_angkor keyboard should be added and appear on the Screen.
41. Click Safari browser in an iPad Tablet.
42. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
43. Verify that the testing index page opens.
44. Click 'View Keymanweb use samples' link button.
45. Verify KeymanWeb Samples page opens.
46. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
47. Verify the corresponding KeymaWeb Sample page opens.
48. Click km from the ‘Add a keyboard by BCP-47 language code’ section.
49. Click the Add button.
50. Verify that the khmer_angkor keyboard should be added and appear on the Screen.
51. Click Safari browser in an iPad Tablet with a physical keyboard.
52. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
53. Verify that the testing index page opens.
54. Click 'View Keymanweb use samples' link button.
55. Verify KeymanWeb Samples page opens.
56. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
57. Verify the corresponding KeymaWeb Sample page opens.
58. Click km from the ‘Add a keyboard by BCP-47 language code’ section.
59. Click the Add button.
60. Verify that the khmer_angkor keyboard should be added and appear on the Screen.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Chrome browser in Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_261**

Test case for Baseline_Add_Sil_Ipa

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `261`
- Product: `Web`
- Source files: `261.JSON`, `261.html`

</details>

**Description**

Original TestLodge ID: TC162

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/433344:id/index.html' in the Search bar.
3.Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Type ‘n   >’ in the text input screen.
9. Verify it produces ‘ŋ’
10. Open Chrome browser in Windows OS.
11. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/433344:id/index.html' in the Search bar.
12.Verify that the testing index page opens.
13. Click 'View Keymanweb use samples' link button.
14. Verify KeymanWeb Samples page opens.
15. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
16. Verify the corresponding KeymaWeb Sample page opens.
17. Type ‘n   >’ in the text input screen.
18. Verify it produces ‘ŋ’
19. Open Safari browser in macOS.
20.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/433344:id/index.html' in the Search bar.
21.Verify that the testing index page opens.
22. Click 'View Keymanweb use samples' link button.
23. Verify KeymanWeb Samples page opens.
24. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
25. Verify the corresponding KeymaWeb Sample page opens.
26. Type ‘n   >’ in the text input screen.
27. Verify it produces ‘ŋ’
28. Open Chrome browser in macOS.
29. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/433344:id/index.html' in the Search bar.
30.Verify that the testing index page opens.
31. Click 'View Keymanweb use samples' link button.
32. Verify KeymanWeb Samples page opens.
33. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
34. Verify the corresponding KeymaWeb Sample page opens.
35. Type ‘n   >’ in the text input screen.
36. Verify it produces ‘ŋ’
37. Open Firefox in macOS.
38. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/433344:id/index.html' in the Search bar.
39.Verify that the testing index page opens.
40. Click 'View Keymanweb use samples' link button.
41. Verify KeymanWeb Samples page opens.
42. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
43. Verify the corresponding KeymaWeb Sample page opens.
44. Type ‘n   >’ in the text input screen.
45. Verify it produces ‘ŋ’
46. Open Firefox in Linux OS.
47.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/433344:id/index.html' in the Search bar.
48.Verify that the testing index page opens.
49. Click 'View Keymanweb use samples' link button.
50. Verify KeymanWeb Samples page opens.
51. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
52. Verify the corresponding KeymaWeb Sample page opens.
53. Type ‘n   >’ in the text input screen.
54. Verify it produces ‘ŋ’

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_275**

Test case for Baseline_Add_SIL_IPA (Touch / Hardware Platforms)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `275`
- Product: `Web`
- Source files: `275.JSON`, `275.html`

</details>

**Description**

Original TestLodge ID: TC176

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon from the Android Mobile.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click sil_ipa from the ‘Add a keyboard by keyboard name’ section.
9. Click the Add button.
10. Verify that the sil_ipa keyboard appears on the Screen.
11. Long-press ‘n’ key, while keeping the finger down, move on the long-press options ‘ŋ‘ so it's highlighted, and release.
12.  Verify that the letter ‘ŋ’ appears on the text input screen.
13. Click the Chrome browser icon from the Android Mobile with the physical keyboard.
14. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
15. Verify that the testing index page opens.
16. Click 'View Keymanweb use samples' link button.
17. Verify KeymanWeb Samples page opens.
18. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
19. Verify the corresponding KeymaWeb Sample page opens.
20. Click sil_ipa from the ‘Add a keyboard by keyboard name’ section.
21. Click the Add button.
22. Verify that the sil_ipa keyboard appears on the Screen.
23. Type n letter from physical keyboard.
24. Type > symbol from physical keyboard.
25. Verify that the letter ‘ŋ’ appears on the text input screen.
26. Click the Safari browser in iPhone Mobile.
27. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
28. Verify that the testing index page opens.
29. Click 'View Keymanweb use samples' link button.
30. Verify KeymanWeb Samples page opens.
31. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
32. Verify the corresponding KeymaWeb Sample page opens.
33. Click sil_ipa from the ‘Add a keyboard by keyboard name’ section.
34. Click the Add button.
35. Verify that the sil_ipa keyboard appears on the Screen.
36. Long-press ‘n’ key, while keeping the finger down, move on the long-press options ‘ŋ‘ so it's highlighted, and release.
37.  Verify that the letter ‘ŋ’ appears on the text input screen.
38. Click the Safari browser in iPhone Mobile with a physical keyboard.
39. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
40. Verify that the testing index page opens.
41. Click 'View Keymanweb use samples' link button.
42. Verify KeymanWeb Samples page opens.
43. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
44. Verify the corresponding KeymaWeb Sample page opens.
45. Click sil_ipa from the ‘Add a keyboard by keyboard name’ section.
46. Click the Add button.
47. Verify that the sil_ipa keyboard appears on the Screen.
48. Long-press ‘n’ key, while keeping the finger down, move on the long-press options ‘ŋ‘ so it's highlighted, and release.
49.  Verify that the letter ‘ŋ’ appears on the text input screen.
50. Click the Safari browser in iPad Tablet.
51. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
52.Verify that the testing index page opens.
53. Click 'View Keymanweb use samples' link button.
54. Verify KeymanWeb Samples page opens.
55. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
56. Verify the corresponding KeymaWeb Sample page opens.
57. Click sil_ipa from the ‘Add a keyboard by keyboard name’ section.
58. Click the Add button.
59. Verify that the sil_ipa keyboard appears on the Screen.
60. Long-press ‘n’ key, while keeping the finger down, move on the long-press options ‘ŋ‘ so it's highlighted, and release.
61.  Verify that the letter ‘ŋ’ appears on the text input screen.
62. Click the Safari browser in the iPad Tablet with a physical keyboard.
63.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
64. Verify that the testing index page opens.
65. Click 'View Keymanweb use samples' link button.
66. Verify KeymanWeb Samples page opens.
67. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
68. Verify the corresponding KeymaWeb Sample page opens.
69. Click sil_ipa from the ‘Add a keyboard by keyboard name’ section.
70. Click the Add button.
71. Verify that the sil_ipa keyboard appears on the Screen.
72. Long-press ‘n’ key, while keeping the finger down, move on the long-press options ‘ŋ‘ so it's highlighted, and release.
73.  Verify that the letter ‘ŋ’ appears on the text input screen.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in an Android Mobile. (Touch)
2. Uninstall Chrome browser in an Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an  iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_264**

Test case for Baseline_Add_Spanish

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `264`
- Product: `Web`
- Source files: `264.JSON`, `264.html`

</details>

**Description**

Original TestLodge ID: TC165

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3.Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click ‘Add a keyboard by language name(s) section’ dropdown list.
9. Select ‘spanish’.
10. Click the Add button.
11. Verify that a Spanish keyboard is added to the list.
12. Open Chrome browser in Windows OS.
13.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
14.Verify that the testing index page opens.
15. Click 'View Keymanweb use samples' link button.
16. Verify KeymanWeb Samples page opens.
17. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
18. Verify the corresponding KeymaWeb Sample page opens.
19. Click ‘Add a keyboard by language name(s) section’ dropdown list.
20. Select ‘spanish’.
21. Click the Add button.
22. Verify that a Spanish keyboard is added to the list.
23. Open Safari browser in macOS.
24.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
25.Verify that the testing index page opens.
26. Click 'View Keymanweb use samples' link button.
27. Verify KeymanWeb Samples page opens.
28. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
29. Verify the corresponding KeymaWeb Sample page opens.
30. Click ‘Add a keyboard by language name(s) section’ dropdown list.
31. Select ‘spanish’.
32. Click the Add button.
33. Verify that a Spanish keyboard is added to the list.
34. Open Chrome browser in macOS.
35.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
36.Verify that the testing index page opens.
37. Click 'View Keymanweb use samples' link button.
38. Verify KeymanWeb Samples page opens.
39. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
40. Verify the corresponding KeymaWeb Sample page opens.
41. Click ‘Add a keyboard by language name(s) section’ dropdown list.
42. Select ‘spanish’.
43. Click the Add button.
44. Verify that a Spanish keyboard is added to the list.
45. Open Firefox browser in macOS.
46. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
47.Verify that the testing index page opens.
48. Click 'View Keymanweb use samples' link button.
49. Verify KeymanWeb Samples page opens.
50. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
51. Verify the corresponding KeymaWeb Sample page opens.
52. Click ‘Add a keyboard by language name(s) section’ dropdown list.
53. Select ‘spanish’.
54. Click the Add button.
55. Verify that a Spanish keyboard is added to the list.
56. Open Firefox browser in Linux OS.
57.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
58.Verify that the testing index page opens.
59. Click 'View Keymanweb use samples' link button.
60. Verify KeymanWeb Samples page opens.
61. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
62. Verify the corresponding KeymaWeb Sample page opens.
63. Click ‘Add a keyboard by language name(s) section’ dropdown list.
64. Select ‘spanish’.
65. Click the Add button.
66. Verify that a Spanish keyboard is added to the list.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_278**

Test case for Baseline_Add_Spanish (Touch / Hardware Platforms),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `278`
- Product: `Web`
- Source files: `278.JSON`, `278.html`

</details>

**Description**

Original TestLodge ID: TC179

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon from the Android Mobile.
2. Click the Chrome browser icon from the Android Mobile.
3. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
4. Verify that the testing index page opens.
5. Click 'View Keymanweb use samples' link button.
6. Verify KeymanWeb Samples page opens.
7. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
8. Verify the corresponding KeymaWeb Sample page opens.
9. Click spanish from the ‘Add a keyboard by language name(s)’ section.
10. Click the Add button.
11. Verify that the Spanish keyboard is added to the list.
12. Click the Chrome browser in Android Mobile attached with a physical keyboard.
13. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
14. Verify that the testing index page opens.
15. Click 'View Keymanweb use samples' link button.
16. Verify KeymanWeb Samples page opens.
17. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
18. Verify the corresponding KeymaWeb Sample page opens.
19. Click spanish from the ‘Add a keyboard by language name(s)’ section.
20. Click the Add button.
21. Verify that the Spanish keyboard is added to the list.
22. Click Safari browser in an iPhone Mobile.
23.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
24. Verify that the testing index page opens.
25. Click 'View Keymanweb use samples' link button.
26. Verify KeymanWeb Samples page opens.
27. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
28. Verify the corresponding KeymaWeb Sample page opens.
29. Click spanish from the ‘Add a keyboard by language name(s)’ section.
30. Click the Add button.
31. Verify that the Spanish keyboard is added to the list.
32. Click Safari browser in an iPhone mobile attached with a physical keyboard.
33. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
34. Verify that the testing index page opens.
35. Click 'View Keymanweb use samples' link button.
36. Verify KeymanWeb Samples page opens.
37. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
38. Verify the corresponding KeymaWeb Sample page opens.
39. Click spanish from the ‘Add a keyboard by language name(s)’ section.
40. Click the Add button.
41. Verify that the Spanish keyboard is added to the list.
42. Click Safari browser in an iPad Tablet.
43. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
44. Verify that the testing index page opens.
45. Click 'View Keymanweb use samples' link button.
46. Verify KeymanWeb Samples page opens.
47. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
48. Verify the corresponding KeymaWeb Sample page opens.
49. Click spanish from the ‘Add a keyboard by language name(s)’ section.
50. Click the Add button.
51. Verify that the Spanish keyboard is added to the list.
52. Click Safari browser in an iPad Tablet attached with a physical keyboard.
53. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
54. Verify that the testing index page opens.
55. Click 'View Keymanweb use samples' link button.
56. Verify KeymanWeb Samples page opens.
57. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
58. Verify the corresponding KeymaWeb Sample page opens.
59. Click spanish from the ‘Add a keyboard by language name(s)’ section.
60. Click the Add button.
61. Verify that the Spanish keyboard is added to the list.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Chrome browser in Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_280**

Test case for Baseline_Cameroon (Touch Platforms),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `280`
- Product: `Web`
- Source files: `280.JSON`, `280.html`

</details>

**Description**

Original TestLodge ID: TC181

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Safari browser in an iPhone Mobile (Touch)
3. Install Safari browser in an iPad Tablet (Touch)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon from the Android Mobile.
2. Enter https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/448575:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click sil_cameroon_qwerty from the ‘Add a keyboard by keyboard name’ section.
9. Click the Add button.
10. Verify that the SIL_Cameroon_Qwerty keyboard is added to the list.
11. Verify that the Cameroon Qwerty keyboard appears on the screen.
12. Press q.
13. Verify that the letter q appears on the text input screen.
14. Press the Shift key.
15. Verify that the Shift layer appears on the screen.
16. Press Q.
17. Verify that the letter Q appears on the screen.
18. Press the Symbol layer.
19. Verify that the symbols appear on the screen.
20. Press $
21. Verify that the symbol $ appears on the screen.
22. Press the tri-color key.
23. Verify that the special keys appear on the keyboard.
24. Press ŋ
25. Verify that the letter ŋ appears on the screen.
26. Click the Safari browser in an iPhone Mobile (Touch)
27. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/448575:id/index.html' in the Search bar.
28. Verify that the testing index page opens.
29. Click 'View Keymanweb use samples' link button.
30. Verify KeymanWeb Samples page opens.
31. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
32. Verify the corresponding KeymaWeb Sample page opens.
33. Click sil_cameroon_qwerty from the ‘Add a keyboard by keyboard name’ section.
34. Click the Add button.
35. Verify that the SIL_Cameroon_Qwerty keyboard is added to the list.
36. Verify that the Cameroon Qwerty keyboard appears on the screen.
37. Press q.
38. Verify that the letter q appears on the text input screen.
39. Press the Shift key.
40. Verify that the Shift layer appears on the screen.
41. Press Q.
42. Verify that the letter Q appears on the screen.
43. Press the Symbol layer.
44. Verify that the symbols appear on the screen.
45. Press $
46. Verify that the symbol $ appears on the screen.
47. Press the tri-color key.
48. Verify that the special keys appear on the keyboard.
49. Press ŋ
50. Verify that the letter ŋ appears on the screen.
51. Click the Safari browser in an iPad Tablet (Touch)
52. Enter https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/448575:id/index.html' in the Search bar.
53. Verify that the testing index page opens.
54. Click 'View Keymanweb use samples' link button.
55. Verify KeymanWeb Samples page opens.
56. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
57. Verify the corresponding KeymaWeb Sample page opens.
58. Click sil_cameroon_qwerty from the ‘Add a keyboard by keyboard name’ section.
59. Click the Add button.
60. Verify that the SIL_Cameroon_Qwerty keyboard is added to the list.
61. Verify that the Cameroon Qwerty keyboard appears on the screen.
62. Press q.
63. Verify that the letter q appears on the text input screen.
64. Press the Shift key.
65. Verify that the Shift layer appears on the screen.
66. Press Q.
67. Verify that the letter Q appears on the screen.
68. Press the Symbol layer.
69. Verify that the symbols appear on the screen.
70. Press $
71. Verify that the symbol $ appears on the screen.
72. Press the tri-color key.
73. Verify that the special keys appear on the keyboard.
74. Press ŋ
75. Verify that the letter ŋ appears on the screen.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Safari browser in an iPhone Mobile. (Touch)
3. Uninstall Safari browser in an iPad Tablet. (Touch)

</details>

---

**TEST_279**

Test case for Baseline_Swedish (Touch / Hardware Platforms),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `279`
- Product: `Web`
- Source files: `279.JSON`, `279.html`

</details>

**Description**

Original TestLodge ID: TC180

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon from the Android Mobile.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the
Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Switch to Swedish keyboard.
9. Verify that the Swedish keyboard appears on the display screen.
10. Long press . (period) on the OSK.
11. Verify that there is a subkey ' appears over the basekey.
12. Touch the subkey ' .
13. Press letter e.
14. Verify that the two characters should not combine. (‘ e)
15. Long press p.
16. Verify that it shows the subkey.
17. Long press G.
18. Verify that it shows the subkey.
19. Click the Chrome browser icon from the Android Mobile attached with a physical keyboard.
20. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
21. Verify that the testing index page opens.
22. Click 'View Keymanweb use samples' link button.
23. Verify KeymanWeb Samples page opens.
24. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
25. Verify the corresponding KeymaWeb Sample page opens.
26. Switch to Swedish keyboard.
27. Type ‘
28. Type e
29. Verify that the output shows é in the text input screen.
30. Type o
31. Type \
32. Type e
33. Verify that the output shows œ in the text input screen.
34. Click Safari browser in an iPhone mobile.
35. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
36. Verify that the testing index page opens.
37. Click 'View Keymanweb use samples' link button.
38. Verify KeymanWeb Samples page opens.
39. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
40. Verify the corresponding KeymaWeb Sample page opens.
41. Switch to Swedish keyboard.
42. Verify that the Swedish keyboard appears on the display screen.
43. Long press . (period) on the OSK.
44. Verify that there is a subkey ' appears over the basekey.
45. Touch the subkey ' .
46. Press letter e.
47. Verify that the two characters should not combine. (‘ e)
48. Long press p.
49. Verify that it shows the subkey.
50. Long press G.
51. Verify that it shows the subkey.
52. Click Safari browser in an iPhone mobile attached with a physical keyboard.
53. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
54. Verify that the testing index page opens.
55. Click 'View Keymanweb use samples' link button.
56. Verify KeymanWeb Samples page opens.
57. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
58. Verify the corresponding KeymaWeb Sample page opens.
59. Switch to Swedish keyboard.
60. Type ‘
61. Type e
62. Verify that the output shows é in the text input screen.
63. Type o
64. Type \
65. Type e
66. Verify that the output shows œ in the text input screen.
67. Click Safari browser in an iPad Tablet.
68. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
69. Verify that the testing index page opens.
70. Click 'View Keymanweb use samples' link button.
71. Verify KeymanWeb Samples page opens.
72. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
73. Verify the corresponding KeymaWeb Sample page opens.
74. Switch to Swedish keyboard.
75. Verify that the Swedish keyboard appears on the display screen.
76. Long press . (period) on the OSK.
77. Verify that there is a subkey ' appears over the basekey.
78. Touch the subkey ' .
79. Press letter e.
80. Verify that the two characters should not combine. (‘ e)
81. Long press p.
82. Verify that it shows the subkey.
83. Long press G.
84. Verify that it shows the subkey.
85. Click Safari browser in an iPad Tablet with a physical keyboard.
86. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
87. Verify that the testing index page opens.
88. Click 'View Keymanweb use samples' link button.
89. Verify KeymanWeb Samples page opens.
90. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
91. Verify the corresponding KeymaWeb Sample page opens.
92. Switch to Swedish keyboard.
93. Type ‘
94. Type e
95. Verify that the output shows é in the text input screen.
96. Type o
97. Type \
98. Type e
99. Verify that the output shows œ in the text input screen.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Chrome browser in Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_265**

Test case for Basline_Swedish

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `265`
- Product: `Web`
- Source files: `265.JSON`, `265.html`

</details>

**Description**

Original TestLodge ID: TC166

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2.  Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3.Verify that the testing index page opens.
4. Click 'View Keymanweb use samples' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Example1 - Toggle UI, all resources in same folder as page' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Type ‘   e  in the text input screen.
9. It produces é on the screen.
10. Type o \ e on the screen.
11. It produces œ on the screen.
12. Open Chrome browser in Windows OS.
13. Enter ‘web/samples/minified.html’ in the Search bar.
14. Verify that the minified page opens in Firefox.
15. Type ‘   e  in the text input screen.
16. It produces é on the screen.
17. Type o \ e on the screen.
18. It produces œ on the screen.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_269**

Test case for Element Attachment Tests

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `269`
- Product: `Web`
- Source files: `269.JSON`, `269.html`

</details>

**Description**

Original TestLodge ID: TC170

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'Tests the new Attachment/Enablement API functionality' link button.
5. Verify KeymanWeb Samples - Attachment API Testing page opens.
6. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
7. Enable ‘auto’ attachment mode radio button.
8. Verify that it automatically attaches to each dynamically-added control.
9. Enable ‘’manual’ attachment mode radio button.
10. Verify that manual attachment mode results in a page with no KMW-activated controls.
11. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
10. Open Chrome browser in Windows.
11. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
12. Verify that the testing index page opens.
13. Click 'Tests the new Attachment/Enablement API functionality' link button.
14. Verify KeymanWeb Samples - Attachment API Testing page opens.
15. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
16. Enable ‘auto’ attachment mode radio button.
17. Verify that it automatically attaches to each dynamically-added control.
18. Enable ‘’manual’ attachment mode radio button.
19. Verify that manual attachment mode results in a page with no KMW-activated controls.
20. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
21. Open Safari in macOS.
22. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
23. Verify that the testing index page opens.
24. Click 'Tests the new Attachment/Enablement API functionality' link button.
25. Verify KeymanWeb Samples - Attachment API Testing page opens.
26. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
27. Enable ‘auto’ attachment mode radio button.
28. Verify that it automatically attaches to each dynamically-added control.
29. Enable ‘’manual’ attachment mode radio button.
30. Verify that manual attachment mode results in a page with no KMW-activated controls.
31. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
32. Open Chrome browser in macOS.
33. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
34 Verify that the testing index page opens.
35. Click 'Tests the new Attachment/Enablement API functionality' link button.
36. Verify KeymanWeb Samples - Attachment API Testing page opens.
37. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
38. Enable ‘auto’ attachment mode radio button.
39. Verify that it automatically attaches to each dynamically-added control.
40. Enable ‘’manual’ attachment mode radio button.
41. Verify that manual attachment mode results in a page with no KMW-activated controls.
42. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
43. Open Firefox browser in macOS.
44. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
45. Verify that the testing index page opens.
46. Click 'Tests the new Attachment/Enablement API functionality' link button.
47. Verify KeymanWeb Samples - Attachment API Testing page opens.
48. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
49. Enable ‘auto’ attachment mode radio button.
50. Verify that it automatically attaches to each dynamically-added control.
51. Enable ‘’manual’ attachment mode radio button.
52. Verify that manual attachment mode results in a page with no KMW-activated controls.
53. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
54. Open Firefox browser in Linux OS.
55. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
56. Verify that the testing index page opens.
57. Click 'Tests the new Attachment/Enablement API functionality' link button.
58. Verify KeymanWeb Samples - Attachment API Testing page opens.
59. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
60. Enable ‘auto’ attachment mode radio button.
61. Verify that it automatically attaches to each dynamically-added control.
62. Enable ‘’manual’ attachment mode radio button.
63. Verify that manual attachment mode results in a page with no KMW-activated controls.
64. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
Clean Steps:
1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

<details>
<summary>Cleanup</summary>

No steps recorded.

</details>

---

**TEST_281**

Test case for Element Attachment Tests (Touch / Hardware platforms),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `281`
- Product: `Web`
- Source files: `281.JSON`, `281.html`

</details>

**Description**

Original TestLodge ID: TC182

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon in an Android Mobile.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb website-oriented manual test pages' link button.
5. Verify KeymanWeb 17 Testing page opens.
6. Click 'Tests the new Attachment/Enablement API functionality' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
9. Enable ‘auto’ attachment mode radio button.
10. Verify that it automatically attaches to each dynamically-added control.
7. Enable ‘’manual’ attachment mode radio button.
8. Verify that manual attachment mode results in a page with no KMW-activated controls.
9. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
10. Click the Chrome browser icon in an Android Mobile device attached with a physical keyboard.
11.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
12. Verify that the testing index page opens.
13. Click 'View Keymanweb website-oriented manual test pages' link button.
14. Verify KeymanWeb 17 Testing page opens.
15. Click 'Tests the new Attachment/Enablement API functionality' link button.
16. Verify the corresponding KeymaWeb Sample page opens.
17. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
18. Enable ‘auto’ attachment mode radio button.
19. Verify that it automatically attaches to each dynamically-added control.
20. Enable ‘’manual’ attachment mode radio button.
21. Verify that manual attachment mode results in a page with no KMW-activated controls.
22. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
23. Click the Safari browser icon in an iPhone mobile.
24. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
25. Verify that the testing index page opens.
26. Click 'View Keymanweb website-oriented manual test pages' link button.
27. Verify KeymanWeb 17 Testing page opens.
28. Click 'Tests the new Attachment/Enablement API functionality' link button.
29. Verify the corresponding KeymaWeb Sample page opens.
30. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
31. Enable ‘auto’ attachment mode radio button.
32. Verify that it automatically attaches to each dynamically-added control.
33. Enable ‘’manual’ attachment mode radio button.
34. Verify that manual attachment mode results in a page with no KMW-activated controls.
35. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
36. Click the Safari browser icon in an iPhone attached with a physical keyboard.
37. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
38. Verify that the testing index page opens.
39. Click 'View Keymanweb website-oriented manual test pages' link button.
40. Verify KeymanWeb 17 Testing page opens.
41. Click 'Tests the new Attachment/Enablement API functionality' link button.
42. Verify the corresponding KeymaWeb Sample page opens.
43. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
44. Enable ‘auto’ attachment mode radio button.
45. Verify that it automatically attaches to each dynamically-added control.
46. Enable ‘’manual’ attachment mode radio button.
47. Verify that manual attachment mode results in a page with no KMW-activated controls.
48. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
49. Click the Safari browser on an iPad tablet.
50. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
51. Verify that the testing index page opens.
52. Click 'View Keymanweb website-oriented manual test pages' link button.
53. Verify KeymanWeb 17 Testing page opens.
54. Click 'Tests the new Attachment/Enablement API functionality' link button.
55. Verify the corresponding KeymaWeb Sample page opens.
56. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
57. Enable ‘auto’ attachment mode radio button.
58. Verify that it automatically attaches to each dynamically-added control.
59. Enable ‘’manual’ attachment mode radio button.
60. Verify that manual attachment mode results in a page with no KMW-activated controls.
61. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.
62. Click the Safari browser icon in an iPad Tablet attached with a physical keyboard.
63. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
64. Verify that the testing index page opens.
65. Click 'View Keymanweb website-oriented manual test pages' link button.
66. Verify KeymanWeb 17 Testing page opens.
67. Click 'Tests the new Attachment/Enablement API functionality' link button.
68. Verify the corresponding KeymaWeb Sample page opens.
69. Verify there are 2 radio buttons namely auto and manual appear one by one on the page.
70. Enable ‘auto’ attachment mode radio button.
71. Verify that it automatically attaches to each dynamically-added control.
72. Enable ‘’manual’ attachment mode radio button.
73. Verify that manual attachment mode results in a page with no KMW-activated controls.
74. Verify that the KeymanWeb is properly attached to and detaches from each relevant type of control with the page’s API-interface controls.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Chrome browser in Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_271**

Test case for Element_Hopping

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `271`
- Product: `Web`
- Source files: `271.JSON`, `271.html`

</details>

**Description**

Original TestLodge ID: TC172

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3.Verify that the testing index page opens.
4.Click 'Tests the new Attachment/Enablement API functionality' link button.
5. Verify KeymanWeb Samples - Attachment API Testing page opens.
6. Click the ‘Create Inputs’ button one time.
7. Click the ‘Create Textarea’ button one time.
8. Click / touch the first of the new page elements.
9. Verify that the OSK should display.
10. Click / touch the other page element.
11. Verify that the OSK should remain visible.
12. Click / touch the first element again.
13. Verify that the OSK should remain visible.
14. Click / touch a blank area of the page.
15. Verify that the OSK should automatically hide.
16. Open Chrome browser in Windows OS.
17. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
18. Verify that the testing index page opens.
19. Click 'Tests the new Attachment/Enablement API functionality' link button.
20. Verify KeymanWeb Samples - Attachment API Testing page opens.
21. Click the ‘Create Inputs’ button one time.
22. Click the ‘Create Textarea’ button one time.
23. Click / touch the first of the new page elements.
24. Verify that the OSK should display.
25. Click / touch the other page element.
26. Verify that the OSK should remain visible.
27. Click / touch the first element again.
28. Verify that the OSK should remain visible.
29. Click / touch a blank area of the page.
30. Verify that the OSK should automatically hide.
31. Open Safari browser in macOS.
32. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
33. Verify that the testing index page opens.
34. Click 'Tests the new Attachment/Enablement API functionality' link button.
35. Verify KeymanWeb Samples - Attachment API Testing page opens.
36. Click the ‘Create Inputs’ button one time.
37. Click the ‘Create Textarea’ button one time.
38. Click / touch the first of the new page elements.
39. Verify that the OSK should display.
40. Click / touch the other page element.
41. Verify that the OSK should remain visible.
42. Click / touch the first element again.
43. Verify that the OSK should remain visible.
44. Click / touch a blank area of the page.
45. Verify that the OSK should automatically hide.
46. Open Chrome browser in macOS.
47. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
48. Verify that the testing index page opens.
49. Click 'Tests the new Attachment/Enablement API functionality' link button.
50. Verify KeymanWeb Samples - Attachment API Testing page opens.
51. Click the ‘Create Inputs’ button one time.
52. Click the ‘Create Textarea’ button one time.
53. Click / touch the first of the new page elements.
54. Verify that the OSK should display.
55. Click / touch the other page element.
56. Verify that the OSK should remain visible.
57. Click / touch the first element again.
58. Verify that the OSK should remain visible.
59. Click / touch a blank area of the page.
60. Verify that the OSK should automatically hide.
61. Open Firefox in macOS.
62. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
63. Verify that the testing index page opens.
64. Click 'Tests the new Attachment/Enablement API functionality' link button.
65. Verify KeymanWeb Samples - Attachment API Testing page opens.
66. Click the ‘Create Inputs’ button one time.
67. Click the ‘Create Textarea’ button one time.
68. Click / touch the first of the new page elements.
69. Verify that the OSK should display.
70. Click / touch the other page element.
71. Verify that the OSK should remain visible.
72. Click / touch the first element again.
73. Verify that the OSK should remain visible.
74. Click / touch a blank area of the page.
75. Verify that the OSK should automatically hide.
76. Open Firefox in Linux OS.
77. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
78. Verify that the testing index page opens.
79. Click 'Tests the new Attachment/Enablement API functionality' link button.
80. Verify KeymanWeb Samples - Attachment API Testing page opens.
81. Click the ‘Create Inputs’ button one time.
82. Click the ‘Create Textarea’ button one time.
83. Click / touch the first of the new page elements.
84. Verify that the OSK should display.
85. Click / touch the other page element.
86. Verify that the OSK should remain visible.
87. Click / touch the first element again.
88. Verify that the OSK should remain visible.
89. Click / touch a blank area of the page.
90. Verify that the OSK should automatically hide.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_283**

Test case for Element_Hopping (Touch / Hardware Platform),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `283`
- Product: `Web`
- Source files: `283.JSON`, `283.html`

</details>

**Description**

Original TestLodge ID: TC184

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser in an Android mobile. (Touch)
2. Enter     ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb website-oriented manual test pages' link button.
5. Verify KeymanWeb 17 Testing page opens.
6. Click 'Tests the new Attachment/Enablement API functionality' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click the ‘Create Inputs’ button one time.
9. Click the ‘Create Textarea’ button one time.
10. Click / touch the first of the new page elements.
11. Verify that the OSK should display.
12. Click / touch the other page element.
13. Verify that the OSK should remain visible.
14. Click / touch the first element again.
15. Verify that the OSK should remain visible.
16. Click / touch a blank area of the page.
17. Verify that the OSK should automatically hide.
18. Click the Chrome browser in Android mobile attached with physical keyboard
19.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
20. Verify that the testing index page opens.
21. Click 'View Keymanweb website-oriented manual test pages' link button.
22. Verify KeymanWeb 17 Testing page opens.
23. Click 'Tests the new Attachment/Enablement API functionality' link button.
24. Verify the corresponding KeymaWeb Sample page opens.
25. Click the ‘Create Inputs’ button one time.
26. Click the ‘Create Textarea’ button one time.
27. Click / touch the first of the new page elements.
28. Verify that the OSK should display.
29. Click / touch the other page element.
30. Verify that the OSK should remain visible.
31. Click / touch the first element again.
32. Verify that the OSK should remain visible.
33. Click / touch a blank area of the page.
34. Verify that the OSK should automatically hide.
35. Click the Safari browser icon in an iPhone mobile.
36. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
43. Verify that the testing index page opens.
44. Click 'View Keymanweb website-oriented manual test pages' link button.
45. Verify KeymanWeb 17 Testing page opens.
46. Click 'Tests the new Attachment/Enablement API functionality' link button.
47. Verify the corresponding KeymaWeb Sample page opens.
48. Click the ‘Create Inputs’ button one time.
49. Click the ‘Create Textarea’ button one time.
50. Click / touch the first of the new page elements.
51. Verify that the OSK should display.
52. Click / touch the other page element.
53. Verify that the OSK should remain visible.
54. Click / touch the first element again.
55. Verify that the OSK should remain visible.
56. Click / touch a blank area of the page.
57. Verify that the OSK should automatically hide.
58. Click the Safari browser icon in an iPhone mobile attached with physical keyboard
59. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
60. Verify that the testing index page opens.
61. Click 'View Keymanweb website-oriented manual test pages' link button.
62. Verify KeymanWeb 17 Testing page opens.
63. Click 'Tests the new Attachment/Enablement API functionality' link button.
64. Verify the corresponding KeymaWeb Sample page opens.
65. Click the ‘Create Inputs’ button one time.
66. Click the ‘Create Textarea’ button one time.
67. Click / touch the first of the new page elements.
68. Verify that the OSK should display.
69. Click / touch the other page element.
70. Verify that the OSK should remain visible.
71. Click / touch the first element again.
72. Verify that the OSK should remain visible.
73. Click / touch a blank area of the page.
74. Verify that the OSK should automatically hide.
75. Click the Safari browser icon on the iPad device.
76.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
77. Verify that the testing index page opens.
78. Click 'View Keymanweb website-oriented manual test pages' link button.
79. Verify KeymanWeb 17 Testing page opens.
80. Click 'Tests the new Attachment/Enablement API functionality' link button.
81. Verify the corresponding KeymaWeb Sample page opens.
83. Click the ‘Create Inputs’ button one time.
84. Click the ‘Create Textarea’ button one time.
85. Click / touch the first of the new page elements.
86. Verify that the OSK should display.
87. Click / touch the other page element.
88. Verify that the OSK should remain visible.
89. Click / touch the first element again.
90. Verify that the OSK should remain visible.
91. Click / touch a blank area of the page.
92. Verify that the OSK should automatically hide.
93. Click the Safari browser icon in the iPad device attached with the physical keyboard.
94. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
95. Verify that the testing index page opens.
96. Click 'View Keymanweb website-oriented manual test pages' link button.
97. Verify KeymanWeb 17 Testing page opens.
98. Click 'Tests the new Attachment/Enablement API functionality' link button.
99. Verify the corresponding KeymaWeb Sample page opens.
100. Click the ‘Create Inputs’ button one time.
101. Click the ‘Create Textarea’ button one time.
102. Click / touch the first of the new page elements.
103. Verify that the OSK should display.
104. Click / touch the other page element.
105. Verify that the OSK should remain visible.
106. Click / touch the first element again.
107. Verify that the OSK should remain visible.
108. Click / touch a blank area of the page.
109. Verify that the OSK should automatically hide.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Chrome browser in Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_274**

Test case for Japanese_Focus

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `274`
- Product: `Web`
- Source files: `274.JSON`, `274.html`

</details>

**Description**

Original TestLodge ID: TC175

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3.Verify that the testing index page opens.
4. Click 'View Keymaweb website-oriented manual test pages' link button.
5. Verify KeymanWeb Samples page opens.
6.Click 'Test unminified Keymanweb' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Type 'japanese' under "Add a keyboard by keyboard name" text box.
9. Click Add button.
10. Select Japanese keyboard.
11. Click the text box against the label name “Add a keyboard by keyboard name”.
12. Type japanese.
13. Click the Add button.
14. Select the Japanese keyboard.
15. Verify the OSK should change the shape notably.
16. Type a.
17. Verify a “picker” displaying a few options should display.
18. Type 2.
19. Verify the second option should replace the context.
20. Select the page’s textarea element.
21. Verify that the OSK’s title bar should appear on the screen.
22. Click a blank area on the page.
23. Verify that the OSK should automatically hide and the caret should disappear from the textarea.
24. Select the page’s input field (the second editable control).
25. Verify that the OSK should reappear under the newly-selected control.
26. Select the page’s textarea element again.
27. Verify that the OSK should remain visible.
28. Verify the caret should move from the input control to the textarea control.
29. Open Chrome browser in Windows OS.
30. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
31. Verify that the testing index page opens.
32. Click 'View Keymaweb website-oriented manual test pages' link button.
33. Verify KeymanWeb Samples page opens.
34. Click 'Test unminified Keymanweb' link button.
35. Verify the corresponding KeymaWeb Sample page opens.
36. Type 'japanese' under "Add a keyboard by keyboard name" text box.
37. Click Add button.
38. Select Japanese keyboard.
39. Click the text box against the label name “Add a keyboard by keyboard name”.
40. Type japanese.
41. Click the Add button.
42. Select the Japanese keyboard.
43. Verify the OSK should change the shape notably.
44. Type a.
45. Verify a “picker” displaying a few options should display.
46. Type 2.
47. Verify the second option should replace the context.
48. Select the page’s textarea element.
49. Verify that the OSK’s title bar should appear on the screen.
50. Click a blank area on the page.
51. Verify that the OSK should automatically hide and the caret should disappear from the textarea.
52. Select the page’s input field (the second editable control).
53. Verify that the OSK should reappear under the newly-selected control.
54. Select the page’s textarea element again.
55. Verify that the OSK should remain visible.
56. Verify the caret should move from the input control to the textarea control.
57. Open Safari browser in macOS.
58. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
59. Verify that the testing index page opens.
60. Click 'View Keymaweb website-oriented manual test pages' link button.
61. Verify KeymanWeb Samples page opens.
62. Click 'Test unminified Keymanweb' link button.
63. Verify the corresponding KeymaWeb Sample page opens.
64. Type 'japanese' under "Add a keyboard by keyboard name" text box.
65. Click Add button.
66. Select Japanese keyboard.
67. Click the text box against the label name “Add a keyboard by keyboard name”.
68. Type japanese.
69. Click the Add button.
70. Select the Japanese keyboard.
71. Verify the OSK should change the shape notably.
72. Type a.
73. Verify a “picker” displaying a few options should display.
74. Type 2.
75. Verify the second option should replace the context.
76. Select the page’s textarea element.
77. Verify that the OSK’s title bar should appear on the screen.
78. Click a blank area on the page.
79. Verify that the OSK should automatically hide and the caret should disappear from the textarea.
80. Select the page’s input field (the second editable control).
81. Verify that the OSK should reappear under the newly-selected control.
82. Select the page’s textarea element again.
83. Verify that the OSK should remain visible.
84. Verify the caret should move from the input control to the textarea control.
85. Open Chrome browser in macOS.
86.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
87. Verify that the testing index page opens.
88. Click 'View Keymaweb website-oriented manual test pages' link button.
89. Verify KeymanWeb Samples page opens.
90. Click 'Test unminified Keymanweb' link button.
91. Verify the corresponding KeymaWeb Sample page opens.
92. Type 'japanese' under "Add a keyboard by keyboard name" text box.
93. Click Add button.
94. Select Japanese keyboard.
95. Click the text box against the label name “Add a keyboard by keyboard name”.
96. Type japanese.
97. Click the Add button.
98. Select the Japanese keyboard.
99. Verify the OSK should change the shape notably.
100. Type a.
101. Verify a “picker” displaying a few options should display.
102. Type 2.
103. Verify the second option should replace the context.
104. Select the page’s textarea element.
105. Verify that the OSK’s title bar should appear on the screen.
106. Click a blank area on the page.
107. Verify that the OSK should automatically hide and the caret should disappear from the textarea.
108. Select the page’s input field (the second editable control).
109. Verify that the OSK should reappear under the newly-selected control.
110. Select the page’s textarea element again.
111. Verify that the OSK should remain visible.
112. Verify the caret should move from the input control to the textarea control.
113. Open Firefox browser in macOS.
114. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
115. Verify that the testing index page opens.
116. Click 'View Keymaweb website-oriented manual test pages' link button.
117. Verify KeymanWeb Samples page opens.
118. Click 'Test unminified Keymanweb' link button.
119. Verify the corresponding KeymaWeb Sample page opens.
120. Type 'japanese' under "Add a keyboard by keyboard name" text box.
121. Click Add button.
122. Select Japanese keyboard.
123. Click the text box against the label name “Add a keyboard by keyboard name”.
124. Type japanese.
125. Click the Add button.
126. Select the Japanese keyboard.
127. Verify the OSK should change the shape notably.
128. Type a.
129. Verify a “picker” displaying a few options should display.
130. Type 2.
131. Verify the second option should replace the context.
132. Select the page’s textarea element.
133. Verify that the OSK’s title bar should appear on the screen.
134. Click a blank area on the page.
135. Verify that the OSK should automatically hide and the caret should disappear from the textarea.
136. Select the page’s input field (the second editable control).
137. Verify that the OSK should reappear under the newly-selected control.
138. Select the page’s textarea element again.
139. Verify that the OSK should remain visible.
140. Verify the caret should move from the input control to the textarea control.
141. Open Firefox browser in Linux OS.
142. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
143. Verify that the testing index page opens.
144. Click 'View Keymaweb website-oriented manual test pages' link button.
145. Verify KeymanWeb Samples page opens.
146. Click 'Test unminified Keymanweb' link button.
147. Verify the corresponding KeymaWeb Sample page opens.
148. Type 'japanese' under "Add a keyboard by keyboard name" text box.
149. Click Add button.
150. Select Japanese keyboard.
151. Click the text box against the label name “Add a keyboard by keyboard name”.
152. Type japanese.
153. Click the Add button.
154. Select the Japanese keyboard.
155. Verify the OSK should change the shape notably.
156. Type a.
157. Verify a “picker” displaying a few options should display.
158. Type 2.
159. Verify the second option should replace the context.
160. Select the page’s textarea element.
161. Verify that the OSK’s title bar should appear on the screen.
162. Click a blank area on the page.
163. Verify that the OSK should automatically hide and the caret should disappear from the textarea.
164. Select the page’s input field (the second editable control).
165. Verify that the OSK should reappear under the newly-selected control.
166. Select the page’s textarea element again.
167. Verify that the OSK should remain visible.
168. Verify the caret should move from the input control to the textarea control.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_273**

Test case for Japanese_Typing

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `273`
- Product: `Web`
- Source files: `273.JSON`, `273.html`

</details>

**Description**

Original TestLodge ID: TC174

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.
7. Run on both INPUT and TEXTAREA elements.
8. Press Ctrl+F5 to reload the page between tests.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymaweb website-oriented manual test pages' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Test unminified Keymanweb' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Click the text box against the label name “Add a keyboard by keyboard name”.
9. Type japanese.
10. Click the Add button.
11. Select the Japanese keyboard.
12. Verify the OSK should change the shape notably.
13. Type a.
14. Verify a “picker” displaying a few options should display.
15. Type 2.
16. Verify the second option should replace the context.
17. Open Chrome browser in Windows OS.
18.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
19. Verify that the testing index page opens.
20. Click 'View Keymaweb website-oriented manual test pages' link button.
21. Verify KeymanWeb Samples page opens.
22. Click 'Test unminified Keymanweb' link button.
23. Verify the corresponding KeymaWeb Sample page opens.
24. Click the text box against the label name “Add a keyboard by keyboard name”.
25. Type japanese.
26. Click the Add button.
27. Select the Japanese keyboard.
28. Verify the OSK should change the shape notably.
29. Type a.
30. Verify a “picker” displaying a few options should display.
31. Type 2.
32. Verify the second option should replace the context.
33. Open Safari browser in macOS.
34.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
35. Verify that the testing index page opens.
36. Click 'View Keymaweb website-oriented manual test pages' link button.
37. Verify KeymanWeb Samples page opens.
38. Click 'Test unminified Keymanweb' link button.
39. Verify the corresponding KeymaWeb Sample page opens.
40. Click the text box against the label name “Add a keyboard by keyboard name”.
41. Type japanese.
42. Click the Add button.
43. Select the Japanese keyboard.
44. Verify the OSK should change the shape notably.
45. Type a.
46. Verify a “picker” displaying a few options should display.
47. Type 2.
48. Verify the second option should replace the context.
49. Open Chrome browser in macOS.
50. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
51. Verify that the testing index page opens.
52. Click 'View Keymaweb website-oriented manual test pages' link button.
53. Verify KeymanWeb Samples page opens.
54. Click 'Test unminified Keymanweb' link button.
55. Verify the corresponding KeymaWeb Sample page opens.
56. Click the text box against the label name “Add a keyboard by keyboard name”.
57. Type japanese.
58. Click the Add button.
59. Select the Japanese keyboard.
60. Verify the OSK should change the shape notably.
61. Type a.
62. Verify a “picker” displaying a few options should display.
63. Type 2.
64. Verify the second option should replace the context.
66. Open Firefox in macOS.
67. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
68. Verify that the testing index page opens.
69. Click 'View Keymaweb website-oriented manual test pages' link button.
70. Verify KeymanWeb Samples page opens.
71. Click 'Test unminified Keymanweb' link button.
72. Verify the corresponding KeymaWeb Sample page opens.
73. Click the text box against the label name “Add a keyboard by keyboard name”.
74. Type japanese.
75. Click the Add button.
76. Select the Japanese keyboard.
77. Verify the OSK should change the shape notably.
78. Type a.
79. Verify a “picker” displaying a few options should display.
80. Type 2.
81. Verify the second option should replace the context.
82. Open Firefox in Linux OS.
83.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
84. Verify that the testing index page opens.
85. Click 'View Keymaweb website-oriented manual test pages' link button.
86. Verify KeymanWeb Samples page opens.
87. Click 'Test unminified Keymanweb' link button.
88. Verify the corresponding KeymaWeb Sample page opens.
89. Click the text box against the label name “Add a keyboard by keyboard name”.
90. Type japanese.
91. Click the Add button.
92. Select the Japanese keyboard.
93. Verify the OSK should change the shape notably.
94. Type a.
95. Verify a “picker” displaying a few options should display.
96. Type 2.
97. Verify the second option should replace the context.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_270**

Test case for Normal_Use

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `270`
- Product: `Web`
- Source files: `270.JSON`, `270.html`

</details>

**Description**

Original TestLodge ID: TC171

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox in windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'Tests the new Attachment/Enablement API functionality' link button.
5. Verify KeymanWeb Samples - Attachment API Testing page opens.
6. Click the “Create Inputs” button one time.
7. Click / touch the new page element.
8. Verify the On Screen Keyboard should display.
7. Type “K” using OSK.
8. Type “L” using OSK.
9. Verify that the OSK keys should produce their expected output.
11. Click / touch a blank area of the page.
12. Verify that the OSK should automatically hide.
13.  Open Chrome in windows OS.
14.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
15. Verify that the testing index page opens.
16. Click 'Tests the new Attachment/Enablement API functionality' link button.
17. Verify KeymanWeb Samples - Attachment API Testing page opens.
18. Click the “Create Inputs” button one time.
19. Click / touch the new page element.
20. Verify the On Screen Keyboard should display.
21. Type “K” using OSK.
22. Type “L” using OSK.
23. Verify that the OSK keys should produce their expected output.
24. Click / touch a blank area of the page.
25. Verify that the OSK should automatically hide.
26. Open Safari browser macOS.
27. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
28. Verify that the testing index page opens.
29. Click 'Tests the new Attachment/Enablement API functionality' link button.
30. Verify KeymanWeb Samples - Attachment API Testing page opens.
31. Click the “Create Inputs” button one time.
32. Click / touch the new page element.
33. Verify the On Screen Keyboard should display.
34. Type “K” using OSK.
35. Type “L” using OSK.
36. Verify that the OSK keys should produce their expected output.
37. Click / touch a blank area of the page.
38. Verify that the OSK should automatically hide.
39. Open Chrome browser in macOS.
40. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
41. Verify that the testing index page opens.
42. Click 'Tests the new Attachment/Enablement API functionality' link button.
43. Verify KeymanWeb Samples - Attachment API Testing page opens.
44. Click the “Create Inputs” button one time.
45. Click / touch the new page element.
46. Verify the On Screen Keyboard should display.
47. Type “K” using OSK.
48. Type “L” using OSK.
49. Verify that the OSK keys should produce their expected output.
50. Click / touch a blank area of the page.
51. Verify that the OSK should automatically hide.
52. Open Firefox browser macOS.
53. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
54. Verify that the testing index page opens.
55. Click 'Tests the new Attachment/Enablement API functionality' link button.
56. Verify KeymanWeb Samples - Attachment API Testing page opens.
57. Click the “Create Inputs” button one time.
58. Click / touch the new page element.
59. Verify the On Screen Keyboard should display.
60. Type “K” using OSK.
61. Type “L” using OSK.
62. Verify that the OSK keys should produce their expected output.
63. Click / touch a blank area of the page.
64. Verify that the OSK should automatically hide.
65. Open Firefox browser in Linux OS.
66. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
Verify that the testing index page opens.
67. Click 'Tests the new Attachment/Enablement API functionality' link button.
68. Verify KeymanWeb Samples - Attachment API Testing page opens.
69. Click the “Create Inputs” button one time.
70. Click / touch the new page element.
71. Verify the On Screen Keyboard should display.
72. Type “K” using OSK.
73. Type “L” using OSK.
74. Verify that the OSK keys should produce their expected output.
75. Click / touch a blank area of the page.
76. Verify that the OSK should automatically hide.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_282**

Test case for Normal_Use (Touch / Hardware Platforms)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `282`
- Product: `Web`
- Source files: `282.JSON`, `282.html`

</details>

**Description**

Original TestLodge ID: TC183

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon in an Android Mobile.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb website-oriented manual test pages' link button.
5. Verify KeymanWeb 17 Testing page opens.
6. Click 'Tests the new Attachment/Enablement API functionality' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
3. Click the “Create Inputs” button one time.
4. Click / touch the new page element.
5. Verify the On Screen Keyboard should display.
6. Type “K” using OSK.
7. Type “L” using OSK.
8. Verify that the OSK keys should produce their expected output.
9. Click / touch a blank area of the page.
10. Verify that the OSK should automatically hide.
11. Click the Chrome browser icon in an Android mobile attached with a physical keyboard.
12. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
13. Verify that the testing index page opens.
14. Click 'View Keymanweb website-oriented manual test pages' link button.
15. Verify KeymanWeb 17 Testing page opens.
16. Click 'Tests the new Attachment/Enablement API functionality' link button.
17. Verify the corresponding KeymaWeb Sample page opens.
18. Click the “Create Inputs” button one time.
19. Click / touch the new page element.
20. Verify the On Screen Keyboard should display.
21. Type “K” using OSK.
22. Type “L” using OSK.
23. Verify that the OSK keys should produce their expected output.
24. Click / touch a blank area of the page.
25. Verify that the OSK should automatically hide.
26. Click the Safari browser in an iPhone mobile.
27. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
28. Verify that the testing index page opens.
29. Click 'View Keymanweb website-oriented manual test pages' link button.
30. Verify KeymanWeb 17 Testing page opens.
31. Click 'Tests the new Attachment/Enablement API functionality' link button.
32. Verify the corresponding KeymaWeb Sample page opens.
33. Click the “Create Inputs” button one time.
34. Click / touch the new page element.
35. Verify the On Screen Keyboard should display.
36. Type “K” using OSK.
37. Type “L” using OSK.
38. Verify that the OSK keys should produce their expected output.
39. Click / touch a blank area of the page.
40. Verify that the OSK should automatically hide.
41. Click the Safari browser in an iPhone mobile attached with a physical keyboard.
42. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
43. Verify that the testing index page opens.
44. Click 'View Keymanweb website-oriented manual test pages' link button.
45. Verify KeymanWeb 17 Testing page opens.
46. Click 'Tests the new Attachment/Enablement API functionality' link button.
47. Verify the corresponding KeymaWeb Sample page opens.
48. Click the “Create Inputs” button one time.
49. Click / touch the new page element.
50. Verify the On Screen Keyboard should display.
51. Type “K” using OSK.
52. Type “L” using OSK.
53. Verify that the OSK keys should produce their expected output.
54. Click / touch a blank area of the page.
55. Verify that the OSK should automatically hide.
56. Click Safari browser in an iPad Tablet.
57. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
58. Verify that the testing index page opens.
59. Click 'View Keymanweb website-oriented manual test pages' link button.
60. Verify KeymanWeb 17 Testing page opens.
61. Click 'Tests the new Attachment/Enablement API functionality' link button.
62. Verify the corresponding KeymaWeb Sample page opens.
63. Click the “Create Inputs” button one time.
64. Click / touch the new page element.
65. Verify the On Screen Keyboard should display.
66. Type “K” using OSK.
67. Type “L” using OSK.
68. Verify that the OSK keys should produce their expected output.
69. Click / touch a blank area of the page.
70. Verify that the OSK should automatically hide.
71. Click Safari browser in an iPad Tablet with a physical keyboard.
72. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
73. Verify that the testing index page opens.
74. Click 'View Keymanweb website-oriented manual test pages' link button.
75. Verify KeymanWeb 17 Testing page opens.
76. Click 'Tests the new Attachment/Enablement API functionality' link button.
77. Verify the corresponding KeymaWeb Sample page opens.
78. Click the “Create Inputs” button one time.
79. Click / touch the new page element.
80. Verify the On Screen Keyboard should display.
81. Type “K” using OSK.
82. Type “L” using OSK.
83. Verify that the OSK keys should produce their expected output.
84. Click / touch a blank area of the page.
85. Verify that the OSK should automatically hide.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Chrome browser in Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_272**

Test case for Specific_Keyboards

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `272`
- Product: `Web`
- Source files: `272.JSON`, `272.html`

</details>

**Description**

Original TestLodge ID: TC173

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install Safari in macOS.
4. Install Chrome in macOS.
5. Install Firefox in macOS.
6. Install Firefox in Linux OS.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
Verify that the testing index page opens.
Click 'Tests the new Attachment/Enablement API functionality' link button.
Verify KeymanWeb Samples - Attachment API Testing page opens.
4. Click the ‘Create Inputs’ button three times.
5. Click / touch Dynamic area #0.
6. Click the element’s “Set to Dzongkha’ button.
7. Change the keyboard to French.
8. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
9. Click Dynamic area #1.
10. Verify the Dzongkha keyboard should be displayed.
11. Click Dynamic area #2.
12. Change the keyboard to Lao.
13. Click Dynamic area#0.
14. Verify that the French keyboard should be displayed.
15. Click Dynamic area #2.
16. Verify that the Lao keyboard should be displayed.
17. Open Chrome browser in Windows OS.
18.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
19. Verify that the testing index page opens.
20. Click 'Tests the new Attachment/Enablement API functionality' link button.
21. Verify KeymanWeb Samples - Attachment API Testing page opens.
22. Click the ‘Create Inputs’ button three times.
23. Click / touch Dynamic area #0.
24. Click the element’s “Set to Dzongkha’ button.
25. Change the keyboard to French.
26. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
27. Click Dynamic area #1.
28. Verify the Dzongkha keyboard should be displayed.
29. Click Dynamic area #2.
30. Change the keyboard to Lao.
31. Click Dynamic area#0.
32. Verify that the French keyboard should be displayed.
33. Click Dynamic area #2.
34. Verify that the Lao keyboard should be displayed.
35. Open Safari browser in macOS.
36. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
37. Verify that the testing index page opens.
38. Click 'Tests the new Attachment/Enablement API functionality' link button.
39. Verify KeymanWeb Samples - Attachment API Testing page opens.
40. Click the ‘Create Inputs’ button three times.
41. Click / touch Dynamic area #0.
42. Click the element’s “Set to Dzongkha’ button.
43. Change the keyboard to French.
44. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
45. Click Dynamic area #1.
46. Verify the Dzongkha keyboard should be displayed.
47. Click Dynamic area #2.
48. Change the keyboard to Lao.
49. Click Dynamic area#0.
50. Verify that the French keyboard should be displayed.
51. Click Dynamic area #2.
52. Verify that the Lao keyboard should be displayed.
53. Open Safari browser in macOS.
54. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
55. Verify that the testing index page opens.
56. Click 'Tests the new Attachment/Enablement API functionality' link button.
57. Verify KeymanWeb Samples - Attachment API Testing page opens.
58. Click the ‘Create Inputs’ button three times.
59. Click / touch Dynamic area #0.
60. Click the element’s “Set to Dzongkha’ button.
61. Change the keyboard to French.
62. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
63. Click Dynamic area #1.
64. Verify the Dzongkha keyboard should be displayed.
65. Click Dynamic area #2.
66. Change the keyboard to Lao.
67. Click Dynamic area#0.
68. Verify that the French keyboard should be displayed.
69. Click Dynamic area #2.
70. Verify that the Lao keyboard should be displayed.
71. Open Chrome browser in macOS.
72. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
73. Verify that the testing index page opens.
74. Click 'Tests the new Attachment/Enablement API functionality' link button.
75. Verify KeymanWeb Samples - Attachment API Testing page opens.
76. Click the ‘Create Inputs’ button three times.
77. Click / touch Dynamic area #0.
78. Click the element’s “Set to Dzongkha’ button.
79. Change the keyboard to French.
80. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
81. Click Dynamic area #1.
82. Verify the Dzongkha keyboard should be displayed.
83. Click Dynamic area #2.
84. Change the keyboard to Lao.
85. Click Dynamic area#0.
86. Verify that the French keyboard should be displayed.
87. Click Dynamic area #2.
88. Verify that the Lao keyboard should be displayed.
89. Open Firefox browser in macOS.
90.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
91. Verify that the testing index page opens.
92. Click 'Tests the new Attachment/Enablement API functionality' link button.
93. Verify KeymanWeb Samples - Attachment API Testing page opens.
94. Click the ‘Create Inputs’ button three times.
95. Click / touch Dynamic area #0.
96. Click the element’s “Set to Dzongkha’ button.
97. Change the keyboard to French.
98. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
99. Click Dynamic area #1.
100. Verify the Dzongkha keyboard should be displayed.
101. Click Dynamic area #2.
102. Change the keyboard to Lao.
103.  Click Dynamic area#0.
104. Verify that the French keyboard should be displayed.
105. Click Dynamic area #2.
106. Verify that the Lao keyboard should be displayed.
107. Open Firefox browser in Linux OS.
108. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
109. Verify that the testing index page opens.
110. Click 'Tests the new Attachment/Enablement API functionality' link button.
111. Verify KeymanWeb Samples - Attachment API Testing page opens.
112. Click the ‘Create Inputs’ button three times.
113. Click / touch Dynamic area #0.
114. Click the element’s “Set to Dzongkha’ button.
115. Change the keyboard to French.
116. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
117. Click Dynamic area #1.
118. Verify the Dzongkha keyboard should be displayed.
119. Click Dynamic area #2.
120. Change the keyboard to Lao.
121. Click Dynamic area#0.
122. Verify that the French keyboard should be displayed.
123. Click Dynamic area #2.
124. Verify that the Lao keyboard should be displayed.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser in Windows.
2. Uninstall Chrome in Windows.
3. Uninstall Safari in macOS.
4. Uninstall Chrome in macOS.
5. Uninstall Firefox in macOS.
6. Uninstall Firefox in Linux OS.

</details>

---

**TEST_284**

Test case for Specific_Keyboards (Touch / Hardware Platforms),

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `284`
- Product: `Web`
- Source files: `284.JSON`, `284.html`

</details>

**Description**

Original TestLodge ID: TC185

<details>
<summary>Setup</summary>

1. Install Chrome browser in an Android Mobile (Touch)
2. Install Chrome browser in an Android Mobile attached with Physical Keyboard. (Hardware)
3. Install Safari browser in an iPhone Mobile (Touch)
4. Install Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Install Safari browser in an iPad Tablet (Touch)
6. Install Safari browser in an iPad Tablet attached with Physical Keyboard. (Hardware)

</details>

<details>
<summary>Action</summary>

1. Click the Chrome browser icon in an Android mobile device.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymanweb website-oriented manual test pages' link button.
5. Verify KeymanWeb 17 Testing page opens.
6. Click 'Tests the new Attachment/Enablement API functionality' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8 . Click the ‘Create Inputs’ button three times.
9. Click / touch Dynamic area #0.
10. Click the element’s “Set to Dzongkha’ button.
11. Change the keyboard to French.
12. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
13. Click Dynamic area #1.
14. Verify the Dzongkha keyboard should be displayed.
15. Click Dynamic area #2.
16. Change the keyboard to Lao.
17. Click Dynamic area#0.
18. Verify that the French keyboard should be displayed.
19. Click Dynamic area #2.
20. Verify that the Lao keyboard should be displayed.
21. Click the Chrome browser icon in an Android mobile device with a physical keyboard.
22. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
23. Verify that the testing index page opens.
25. Click 'View Keymanweb website-oriented manual test pages' link button.
26. Verify KeymanWeb 17 Testing page opens.
27. Click 'Tests the new Attachment/Enablement API functionality' link button.
28. Verify the corresponding KeymaWeb Sample page opens.
29. Click the ‘Create Inputs’ button three times.
30. Click / touch Dynamic area #0.
31. Click the element’s “Set to Dzongkha’ button.
32. Change the keyboard to French.
33. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
34. Click Dynamic area #1.
35. Verify the Dzongkha keyboard should be displayed.
36. Click Dynamic area #2.
37. Change the keyboard to Lao.
38. Click Dynamic area#0.
39. Verify that the French keyboard should be displayed.
40. Click Dynamic area #2.
41. Verify that the Lao keyboard should be displayed.
42. Click the Safari browser icon in an iPhone mobile device.
43. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
44. Verify that the testing index page opens.
45. Click 'View Keymanweb website-oriented manual test pages' link button.
46. Verify KeymanWeb 17 Testing page opens.
47. Click 'Tests the new Attachment/Enablement API functionality' link button.
48. Verify the corresponding KeymaWeb Sample page opens.
49. Click the ‘Create Inputs’ button three times.
50. Click / touch Dynamic area #0.
51. Click the element’s “Set to Dzongkha’ button.
52. Change the keyboard to French.
53. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
54. Click Dynamic area #1.
55. Verify the Dzongkha keyboard should be displayed.
56. Click Dynamic area #2.
57. Change the keyboard to Lao.
58. Click Dynamic area#0.
59. Verify that the French keyboard should be displayed.
60. Click Dynamic area #2.
61. Verify that the Lao keyboard should be displayed.
62. Click the Safari browser icon in an iPhone mobile device attached with a physical keyboard.
63. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
64. Verify that the testing index page opens.
65. Click 'View Keymanweb website-oriented manual test pages' link button.
66. Verify KeymanWeb 17 Testing page opens.
67. Click 'Tests the new Attachment/Enablement API functionality' link button.
68. Verify the corresponding KeymaWeb Sample page opens.
69. Click the ‘Create Inputs’ button three times.
70. Click / touch Dynamic area #0.
71. Click the element’s “Set to Dzongkha’ button.
72. Change the keyboard to French.
73. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
74. Click Dynamic area #1.
75. Verify the Dzongkha keyboard should be displayed.
76. Click Dynamic area #2.
77. Change the keyboard to Lao.
78. Click Dynamic area#0.
79. Verify that the French keyboard should be displayed.
80. Click Dynamic area #2.
81. Verify that the Lao keyboard should be displayed.
82. Click the Safari browser icon on an iPad device.
83. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
84. Verify that the testing index page opens.
85. Click 'View Keymanweb website-oriented manual test pages' link button.
86. Verify KeymanWeb 17 Testing page opens.
87. Click 'Tests the new Attachment/Enablement API functionality' link button.
88. Verify the corresponding KeymaWeb Sample page opens.
89. Click the ‘Create Inputs’ button three times.
90. Click / touch Dynamic area #0.
91. Click the element’s “Set to Dzongkha’ button.
92. Change the keyboard to French.
93. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
94. Click Dynamic area #1.
95. Verify the Dzongkha keyboard should be displayed.
96. Click Dynamic area #2.
97. Change the keyboard to Lao.
98. Click Dynamic area#0.
99. Verify that the French keyboard should be displayed.
100. Click Dynamic area #2.
101. Verify that the Lao keyboard should be displayed.
102. Click the Safari browser icon in an iPad device attached with a physical keyboard.
103. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
104. Verify that the testing index page opens.
105. Click 'View Keymanweb website-oriented manual test pages' link button.
106. Verify KeymanWeb 17 Testing page opens.
107. Click 'Tests the new Attachment/Enablement API functionality' link button.
108. Verify the corresponding KeymaWeb Sample page opens.
109. Click the ‘Create Inputs’ button three times.
110. Click / touch Dynamic area #0.
111. Click the element’s “Set to Dzongkha’ button.
112. Change the keyboard to French.
113. Click Dynamic area #1 ‘s ‘Set to Dzongkha button.
114. Click Dynamic area #1.
115. Verify the Dzongkha keyboard should be displayed.
116. Click Dynamic area #2.
117. Change the keyboard to Lao.
118. Click Dynamic area#0.
119. Verify that the French keyboard should be displayed.
120. Click Dynamic area #2.
121. Verify that the Lao keyboard should be displayed.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Chrome browser in Android Mobile. (Touch)
2. Uninstall Chrome browser in Android Mobile attached with Physical Keyboard.(Hardware)
3. Uninstall Safari browser in an iPhone Mobile.
4. Uninstall Safari browser in an iPhone Mobile attached with Physical Keyboard. (Hardware)
5. Uninstall Safari browser in an iPad Tablet.
6. Uninstall Safari browser in an iPad Tablet with Physical Keyboard.

</details>

---

**TEST_268**

Test case for Text Selection_Tests (LinuxOS)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `268`
- Product: `Web`
- Source files: `268.JSON`, `268.html`

</details>

**Description**

Original TestLodge ID: TC169

<details>
<summary>Setup</summary>

1. Install Firefox in Linux OS.
2. Install web_context_tests keyboard.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Linux OS.
2.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymaweb website-oriented manual test pages' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Test unminified Keymanweb' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Select 'web_context_test' keyboard.
9. Type a b c x  in the TextArea.
10. Select bc.
11. Type q.
12. Verify that the output shows ‘aqx’.
13. Press Ctrl+F5 keys.
14. Verify that the page reloads the page.
15. Type a b c x.
16. Select bc.
17. Type p.
18. Verify that the output shows ‘aqx’.
19. Type a b c d.
20. Press Ctrl+F5 keys.
21. Verify that the page reloads the page.
22. Type a b c d.
23. Verify that the output shows !.
24. Press Ctrl+F5 keys.
25. Type a b c x.
26. Select the x character.
27. Type d.
28. Verify that the final letter should be changed and the output should be abcd.
29. Press Ctrl+F5 keys.
30. Type a b c x.
31. Select the x character.
32. Press Backspace key to delete the character.
33. Type d.
34. Verify that the final letter should be !.
35. Press Ctrl+F5 keys.
36. Type a b c x.
37. Select the x character.
38. Type y.
39. Press Backspace key.
40. Type d.
41. Verify that the final letter should be !.
42. Press Ctrl+F5 keys.
43. Type x a b c x.
44. Select the abc characters.
45. Type d.
46. Verify that the final letter should be xdx.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser.
2. Uninstall web_context_tests keyboard.

</details>

---

**TEST_267**

Test case for Text Selection_Tests (macOS)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `267`
- Product: `Web`
- Source files: `267.JSON`, `267.html`

</details>

**Description**

Original TestLodge ID: TC168

<details>
<summary>Setup</summary>

1. Install Safari in macOS
2. Install Chrome in macOS
3. Install Firefox in macOS
4. Install web_context_tests keyboard.
5. Run on both INPUT and TEXTAREA elements.
6. Press Ctrl+F5 to reload the page between tests.

</details>

<details>
<summary>Action</summary>

1. Open Safari browser in macOS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymaweb website-oriented manual test pages' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Test unminified Keymanweb' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Select 'web_context_test' keyboard.
9. Type a b c x  in the TextArea.
10. Select bc.
11. Type q.
12. Verify that the output shows ‘aqx’.
13. Press Ctrl+F5 keys.
14. Verify that the page reloads the page.
15. Type a b c x.
16. Select bc.
17. Type p.
18. Verify that the output shows ‘aqx’.
19. Type a b c d.
20. Press Ctrl+F5 keys.
21. Verify that the page reloads the page.
22. Type a b c d.
23. Verify that the output shows !.
24. Press Ctrl+F5 keys.
25. Type a b c x.
26. Select the x character.
27. Type d.
28. Verify that the final letter should be changed and the output should be abcd.
29. Press Ctrl+F5 keys.
30. Type a b c x.
31. Select the x character.
32. Press Backspace key to delete the character.
33. Type d.
34. Verify that the final letter should be !.
35. Press Ctrl+F5 keys.
36. Type a b c x.
37. Select the x character.
38. Type y.
39. Press Backspace key.
40. Type d.
41. Verify that the final letter should be !.
42. Press Ctrl+F5 keys.
43. Type x a b c x.
44. Select the abc characters.
45. Type d.
46. Verify that the final letter should be xdx.
47. Open Chrome browser in macOS.
48. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
49. Verify that the testing index page opens.
50. Click 'View Keymaweb website-oriented manual test pages' link button.
51. Verify KeymanWeb Samples page opens.
52. Click 'Test unminified Keymanweb' link button.
53. Verify the corresponding KeymaWeb Sample page opens.
54. Select 'web_context_test' keyboard.
55. Type a b c x  in the TextArea.
56. Select bc.
57. Type q.
58. Verify that the output shows ‘aqx’.
59. Press Ctrl+F5 keys.
60. Verify that the page reloads the page.
61. Type a b c x.
62. Select bc.
63. Type p.
64. Verify that the output shows ‘aqx’.
65. Type a b c d.
66. Press Ctrl+F5 keys.
67. Verify that the page reloads the page.
68. Type a b c d.
69. Verify that the output shows !.
70. Press Ctrl+F5 keys.
71. Type a b c x.
72. Select the x character.
73. Type d.
74. Verify that the final letter should be changed and the output should be abcd.
75. Press Ctrl+F5 keys.
76. Type a b c x.
77. Select the x character.
78. Press Backspace key to delete the character.
79. Type d.
80. Verify that the final letter should be !.
81. Press Ctrl+F5 keys.
82. Type a b c x.
83. Select the x character.
84. Type y.
85. Press Backspace key.
86. Type d.
87. Verify that the final letter should be !.
88. Press Ctrl+F5 keys.
89. Type x a b c x.
90. Select the abc characters.
91. Type d.
92. Verify that the final letter should be xdx.
93. Open Firefox browser in macOS.
94. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
95. Verify that the testing index page opens.
96. Click 'View Keymaweb website-oriented manual test pages' link button.
97. Verify KeymanWeb Samples page opens.
98. Click 'Test unminified Keymanweb' link button.
99. Verify the corresponding KeymaWeb Sample page opens.
100. Select 'web_context_test' keyboard.
101. Type a b c x  in the TextArea.
102. Select bc.
103. Type q.
104. Verify that the output shows ‘aqx’.
105. Press Ctrl+F5 keys.
106. Verify that the page reloads the page.
107. Type a b c x.
108. Select bc.
109. Type p.
110. Verify that the output shows ‘aqx’.
111. Type a b c d.
112. Press Ctrl+F5 keys.
113. Verify that the page reloads the page.
114. Type a b c d.
115. Verify that the output shows !.
116. Press Ctrl+F5 keys.
117. Type a b c x.
118. Select the x character.
119. Type d.
120. Verify that the final letter should be changed and the output should be abcd.
121. Press Ctrl+F5 keys.
122. Type a b c x.
123. Select the x character.
124. Press Backspace key to delete the character.
125. Type d.
126. Verify that the final letter should be !.
127. Press Ctrl+F5 keys.
128. Type a b c x.
129. Select the x character.
130. Type y.
131. Press Backspace key.
132. Type d.
133. Verify that the final letter should be !.
134. Press Ctrl+F5 keys.
135. Type x a b c x.
136. Select the abc characters.
137. Type d.
138. Verify that the final letter should be xdx.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Safari browser.
2. Uninstall Chrome browser.
3. Uninstall Firefox browser.
4. Uninstall web_context_tests keyboard.

</details>

---

**TEST_266**

Test case for Text Selection_Tests (Windows)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `266`
- Product: `Web`
- Source files: `266.JSON`, `266.html`

</details>

**Description**

Original TestLodge ID: TC167

<details>
<summary>Setup</summary>

1. Install Firefox in windows.
2. Install Chrome in windows.
3. Install web_context_tests keyboard.
4. Run on both INPUT and TEXTAREA elements.
5. Press Ctrl+F5 to reload the page between tests.

</details>

<details>
<summary>Action</summary>

1. Open Firefox browser in Windows OS.
2. Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
3. Verify that the testing index page opens.
4. Click 'View Keymaweb website-oriented manual test pages' link button.
5. Verify KeymanWeb Samples page opens.
6. Click 'Test unminified Keymanweb' link button.
7. Verify the corresponding KeymaWeb Sample page opens.
8. Select 'web_context_test' keyboard.
9. Type a b c x  in the TextArea.
10. Select bc.
11. Type q.
12. Verify that the output shows ‘aqx’.
13. Press Ctrl+F5 keys.
14. Verify that the page reloads the page.
15. Type a b c x.
16. Select bc.
17. Type p.
18. Verify that the output shows ‘aqx’.
19. Type a b c d.
20. Press Ctrl+F5 keys.
21. Verify that the page reloads the page.
22. Type a b c d.
23. Verify that the output shows !.
24. Press Ctrl+F5 keys.
25. Type a b c x.
26. Select the x character.
27. Type d.
28. Verify that the final letter should be changed and the output should be abcd.
29. Press Ctrl+F5 keys.
30. Type a b c x.
31. Select the x character.
32. Press Backspace key to delete the character.
33. Type d.
34. Verify that the final letter should be !.
35. Press Ctrl+F5 keys.
36. Type a b c x.
37. Select the x character.
38. Type y.
39. Press Backspace key.
40. Type d.
41. Verify that the final letter should be !.
42. Press Ctrl+F5 keys.
43. Type x a b c x.
44. Select the abc characters.
45. Type d.
46. Verify that the final letter should be xdx.
47. Open Chrome browser in Windows OS.
48.Enter ‘https://build.palaso.org/repository/download/Keymanweb_TestPullRequests/391530:id/index.html' in the Search bar.
49. Verify that the testing index page opens.
50. Click 'View Keymaweb website-oriented manual test pages' link button.
51. Verify KeymanWeb Samples page opens.
52. Click 'Test unminified Keymanweb' link button.
53. Verify the corresponding KeymaWeb Sample page opens.
54. Select 'web_context_test' keyboard.
55. Type a b c x  in the TextArea.
56. Select bc.
57. Type q.
58. Verify that the output shows ‘aqx’.
59. Press Ctrl+F5 keys.
60. Verify that the page reloads the page.
61. Type a b c x.
62. Select bc.
63. Type p.
64. Verify that the output shows ‘aqx’.
65. Type a b c d.
66. Press Ctrl+F5 keys.
67. Verify that the page reloads the page.
68. Type a b c d.
69. Verify that the output shows !.
70. Press Ctrl+F5 keys.
71. Type a b c x.
72. Select the x character.
73. Type d.
74. Verify that the final letter should be changed and the output should be abcd.
75. Press Ctrl+F5 keys.
76. Type a b c x.
77. Select the x character.
78. Press Backspace key to delete the character.
79. Type d.
80. Verify that the final letter should be !.
81. Press Ctrl+F5 keys.
82. Type a b c x.
83. Select the x character.
84. Type y.
85. Press Backspace key.
86. Type d.
87. Verify that the final letter should be !.
88. Press Ctrl+F5 keys.
89. Type x a b c x.
90. Select the abc characters.
91. Type d.
92. Verify that the final letter should be xdx.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Firefox browser.
2. Uninstall Chrome browser.
3. Uninstall web_context_tests keyboard.

</details>
