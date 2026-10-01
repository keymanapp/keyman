# Developer Acceptance Tests

**TEST_339**

Evaluate and apply npm audit fix (without forced changes)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `339`
- Product: `Developer`
- Source files: `339.JSON`, `339.html`

</details>

**Description**

TC - maint(common): evaluate and apply npm audit fix (without forced changes) #13897

<details>
<summary>Setup</summary>

1.    Download Keyman Developer build from https://keyman.com/windows/ Site to the Desktop.
2.    Install KeymanDeveloper build.
3.    Install the Keyman build.

</details>

<details>
<summary>Action</summary>

1.    Open the "Khmer Angkor"(or Tamil) keyboard project loaded in the Keyman Developer.
2.    On the keyboards tab: Click the "Build keyboards" button to build the keyboard.
3.    Click the "*.knm" link, and a new tab opens.
4.    Verified that the new tab appeared with *.kmn
5.    Click on the "Build" tab. Click the "Compile Keyboard" button.
6.    Click on the "Start Debugging" button.
7.    Enter some text in the debug window.
8.    Return to the "Build" tab.
9.    Click on the "Test Keyboard on Web" button.
10.    Select "http://localhost:8008" in the server-listed pane.
11.    Click on the "Open in browser" button.
12.    Verified that the web keyboard appeared on the browser.
13.    Select the "Khmer_Angkor"(or Tamil) keyboard and then select Windows by clicking the device button.
14.    Press some keys on the OSK.
15.    Verified that the key shows the khmer letter.
16.    Change the device from Windows to Mobile by clicking the "Device" button.
17.    Press some keys on the OSK.
18.    Verified that the key is showing the khmer letter.

</details>

<details>
<summary>Cleanup</summary>

1.    Type ‘Control panel’ in the search bar (in the taskbar).
2.    Verify that the ‘Control Panel’ appears in the menu list.
3.    Click the Control Panel option.
4.    Verify that the Control Panel dialog appears.
5.    Click the ‘Uninstall a Program’ option.
6.    Verify that the Programs and Features dialog box opens.
7.    Select the installed Keyman Developer version from the list.
8.    Click the Uninstall option.
9.    Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_258**

feat(developer): kmc generate #11014(TEST_KEYMAN_KEYBOARD)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `258`
- Product: `Developer`
- Source files: `258.JSON`, `258.html`

</details>

**Description**

Original TestLodge ID: TC303

<details>
<summary>Setup</summary>

1. Install Keyman Developer latest stable build from Keyman.com site.(or state.keyman.com)
2. Created a new folder under the download folder.
3. Open the command prompt in this folder.
4. Executed below commands to generate keyman keyboards.
E.g. kmc generate keyman-keyboard "Folder_Name"
kmc generate keyman-keyboard kmnkeyboard
5. Verify that the keyboard folder was created with the specified name

</details>

<details>
<summary>Action</summary>

1. Go to the folder that had keyboards.
2. Open the command prompt in this folder.
3. Execute the "kmc build keyboard_folder_name" command.
4. Verify that the build folder was created inside the keyboard's folder.
5. Verify that the *.kmp file was created inside the build folder.
6. Install the *.kmp file in Windows 10 or 11
7. Verify that the keyboard was added under "English"
8. Go to the source folder.
9. Verify that the generated files.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the search bar (in the task bar)
2. Verify that the ‘Control Panel’ appears in the menu list.
3. Click the Control Panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman Developer version from the list.
8. Click the Uninstall option.
9. Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_260**

feat(developer): kmc generate #11014(TEST_LEXICAL_MODEL)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `260`
- Product: `Developer`
- Source files: `260.JSON`, `260.html`

</details>

**Description**

Original TestLodge ID: TC305

<details>
<summary>Setup</summary>

1. Install Keyman Developer latest stable build from Keyman.com site.(or state.keyman.com)
2. Created a new folder under the download folder.
3. Open the command prompt in this folder.
4. Executed below commands to generate keyman keyboards.
E.g. kmc generate keyman-keyboard "Folder_Name"
kmc generate lexical-model en.lexicalmodeluniq
5. Verify that the keyboard folder was created with the specified name

</details>

<details>
<summary>Action</summary>

1. Go to the folder that had keyboards.
2. Open the command prompt in this folder.
3. Execute the "kmc build keyboard_folder_name" command.
4. Verify that the build folder was created inside the keyboard's folder.
5. Verify that the *.kmp file was created inside the build folder.
6. Install the *.kmp file on an Android 12 mobile device.
7. Verify that the keyboard was added under "English."
8. Go to the source folder.
9. Verify that the generated files.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the search bar (in the task bar)
2. Verify that the ‘Control Panel’ appears in the menu list.
3. Click the Control Panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman Developer version from the list.
8. Click the Uninstall option.
9. Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_253**

feat(developer): kmc-copy #12555 (TEST_COPY_MODELS)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `253`
- Product: `Developer`
- Source files: `253.JSON`, `253.html`

</details>

**Description**

Original TestLodge ID: TC298

<details>
<summary>Setup</summary>

1. Install Keyman Developer latest stable build from Keyman.com site.(or state.keyman.com)
2. Created a new folder under the Download folder.
3. Created a new folder under the Temp folder.

</details>

<details>
<summary>Action</summary>

1. Navigate to the folder path that has GitHub's Lexical-models keyboard.
(Get the latest changes from GitHub's in the local machine)
2. Open the command prompt.
3. Enter the below command in the window's prompt
kmc copy "keyboard source file path\folder_name" -o "Keyboard destination folder path"
E.g. kmc copy C:\Documents\GitHub\lexical-models\release\sil\sil.cmo.bw -o C:\Downloads\TestingKeyboard\LexicalModelKMCTesting1
kmc copy "keyboard source file path\*.kpj" -o "Keyboard destination folder path"
E.g kmc copy C:\Documents\GitHub\lexical-models\release\sil\sil.cmo.bw\sil.cmo.bw.kpj -o C:\Downloads\TestingKeyboard\LexicalModelKMCTesting
4. Verify that the keyboard folder or *.kpj file was copied into a new folder.
5. Verify that the keyboard file was renamed in the new folder.

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the search bar (in the task bar)
2. Verify that the ‘Control Panel’ appears in the menu list.
3. Click the Control Panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman Developer version from the list.
8. Click the Uninstall option.
9. Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_251**

fix(developer): handle KM_CORE_IT_INVALIDATE_CONTEXT in debugger #11488

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `251`
- Product: `Developer`
- Source files: `251.JSON`, `251.html`

</details>

**Description**

Original TestLodge ID: TC217

<details>
<summary>Setup</summary>

1. Download the Keyman Developer build from https://keyman.com/windows/site to the desktop.
2. Install the Amharic keyboard project.
3. Install the Keyman build.

</details>

<details>
<summary>Action</summary>

1. Open the Keyman Developer application.
2. Click the Keyboards tab.
3. Verify the "gff_amharic.kmn" link button appears in the middle pane.
4. Click the ‘gff_amharic.kmn’ link button.
5. Verify the gff_amharic.kmn page opens.
6. Press the F7 button.
7. Verify the project is compiled successfully and the compiled message appears in the green color at the bottom of the page.
8. Press the F5 button.
9. Verify the "Debugger Mode" view opens.
10. Click the Platform tab.
11. Select "Windows" from the Operating System drop-down list.
12. Enter some text in an empty text box using the Amharic keyboard.
13. Verify that the "Amharic" text appeared in the text box.
14. Press the CTRL+A and then CTRL+C keys.
15. Verify that the text is selected in the text box.
16. Press CTRL+V.
17. Press CTRL+Z.
18. Verify that the text disappeared in the text box.
19. Verify that no error message box appeared or no crash dialog appeared

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build.
2. Uninstall Keyman Developer.
3. Uninstall Amharic Keyboard Project.
4. Restart system.
5. Install Updates.
6. Clean up any broken installation artifacts (if they are available in the following folders)
%ProgramFiles(x86)%\Keyman or %ProgramFiles%\Keyman
%CommonProgramFiles(x86)%\Keyman or %CommonProgramFiles%\Keyman
%ProgramData%\Keyman
%AppData%\Keyman
%LocalAppData%\Keyman
HKCU\Software\Keyman
HKLM\Software\Wow6432Node\Keyman
7. Manually remove any remaining Keyman input methods from Windows Languages

</details>

---

**TEST_256**

fix(developer): handle paste of TSV into Wordlist grid #12594(TEST_PASTE_SIMPLE_STRING)

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `256`
- Product: `Developer`
- Source files: `256.JSON`, `256.html`

</details>

**Description**

Original TestLodge ID: TC301

<details>
<summary>Setup</summary>

1. Install Keyman Developer latest stable build from Keyman.com site.(or state.keyman.com)
2. Create a new "Wordlist Lexical Model" project.
3. Enter an appropriate text in the "New Wordlist Lexical Model" project dialog.
4. Project appeared in the new dialog.
5. Navigate to the "Models" tab.
6. Click the *.model.ts link.
7. Navigate to the "wordlist.tsv" tab.

</details>

<details>
<summary>Action</summary>

1. Copy a word from a notepad file or clipboard.
2. Click in the "Add word..." box.
3. Paste the text by pressing the CTRL+V key.
4. Verify the word appears in the "Add word..." box.
5. Verify that no error appears after pasting a text in the "Add word..." box.
6. Verify the new "Add word..." row added

</details>

<details>
<summary>Cleanup</summary>

1. Type ‘Control panel’ in the search bar (in the taskbar).
2. Verify that the ‘Control Panel’ appears in the menu list.
3. Click the Control Panel option.
4. Verify that the Control Panel dialog appears.
5. Click the ‘Uninstall a Program’ option.
6. Verify that the Programs and Features dialog box opens.
7. Select the installed Keyman Developer version from the list.
8. Click the Uninstall option.
9. Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_362**

Remove redundant Name and RTL fields from .kps LexicalModel_TEST_NEW_PROJECT

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `362`
- Product: `Developer`
- Source files: `362.JSON`, `362.html`

</details>

**Description**

TC - fix(developer): remove redundant Name and RTL fields from .kps LexicalModel_TEST_NEW_PROJECT #13640

<details>
<summary>Setup</summary>

1.    Download Keyman Developer build from https://keyman.com/windows/ Site to the Desktop.
2.    Install KeymanDeveloper build.
3.    Install the Keyman build.

</details>

<details>
<summary>Action</summary>

1.    Launch the KeymanDeveloper.
2.    Open an LDML keyboard project from the GitHub\lexical-models\release folder path.
3.    Create a new LDML keyboard project by clicking the project menu --> new project.
4.    Build the entire project on the packaging.
5.    Verify that the project build folder has files(inside the build folder)
6.    Compile the changes by clicking the keyboard menu --> Compile keyboard.
7.    Open the .kps file in Notepad from the source folder.
8.    Verify that the name does not appear in the *.kps file

</details>

<details>
<summary>Cleanup</summary>

1.    Type ‘Control panel’ in the search bar (in the taskbar).
2.    Verify that the ‘Control Panel’ appears in the menu list.
3.    Click the Control Panel option.
4.    Verify that the Control Panel dialog appears.
5.    Click the ‘Uninstall a Program’ option.
6.    Verify that the Programs and Features dialog box opens.
7.    Select the installed Keyman Developer version from the list.
8.    Click the Uninstall option.
9.    Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_314**

TC - feat(developer): improve compiler messages and user interface_TEST_MESSAGE_RIGHT-CLICK

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `314`
- Product: `Developer`
- Source files: `314.JSON`, `314.html`

</details>

**Description**

TC - feat(developer): improve compiler messages and user interface_TEST_MESSAGE_RIGHT-CLICK #13156

<details>
<summary>Setup</summary>

1.    Download Keyman Developer build from https://keyman.com/windows/ Site to the Desktop.
2.    Install "gautami_devanagari" keyboard project.
3.    Install Keyman build.

</details>

<details>
<summary>Action</summary>

1.    Open the "gautami_devanagari" keyboard project loaded in the keyman developer.
2.    Verify  that the project is loaded in the keyman developer window.
3.    Compile the keyboard by clicking the keyboard menu --> compile keyboard.
4.    Verify that the compile success message appears
5.    Right-click on the compiled message.
6.    Select the "Open documentation on selected message" option from the menu.
7.    Verify that the navigation redirects to the browser.
8.    Verify that the link appears on the browser.
9.    Verify the message details appears on the "help.keyman.com" website.

</details>

<details>
<summary>Cleanup</summary>

1.    Type ‘Control panel’ in the search bar (in the taskbar).
2.    Verify that the ‘Control Panel’ appears in the menu list.
3.    Click the Control Panel option.
4.    Verify that the Control Panel dialog appears.
5.    Click the ‘Uninstall a Program’ option.
6.    Verify that the Programs and Features dialog box opens.
7.    Select the installed Keyman Developer version from the list.
8.    Click the Uninstall option.
9.    Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_317**

TC - fix(developer): support &displayMap font in web debugger_TEST_DISPLAYMAP_FONT

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `317`
- Product: `Developer`
- Source files: `317.JSON`, `317.html`

</details>

**Description**

TC - fix(developer): support &displayMap font in web debugger_TEST_DISPLAYMAP_FONT #12929

<details>
<summary>Setup</summary>

1.    Download Keyman Developer build from https://keyman.com/windows/ Site to the Desktop.
2.    Install KeymanDeveloper build.
3.    Install Keyman build.

</details>

<details>
<summary>Action</summary>

1.    Open the "Khmer Angkor" keyboard project loaded in the keyman developer.
2.    Click the "*.knm" link and then a new tab opens.
3.    Verify that the new tab appears with *.kmn
4.    Click on the "Layout" tab.
5.    Open the Font by clicking the Keyboard --> Fonts.
6.    Verify that the "DisplayMap target font" has the "kbdKhmr" font.
7.    Close the font dialog by clicking the "Ok" button.
8.    Click on the "Build" tab. Click the "Compile Keyboard" button.
9.    Click on the "Test Keyboard on Web" button.
10.    Select the "http://localhost:8008" in the server-listed pane.
11.    Click on the "Open in browser" button.
12.    Verify that the web keyboard appears on the browser.
13.    Select the "Khmer_Angkor" keyboard and then select Windows by clicking the device button.
14.    Press on the SHIFT+A
15.    Verify that the "A" key shows the single dot khmer letter.
16.    Change the device from Windows to Mobile by clicking the "Device" button.
17.    Long press the "A" key.
18.    Verify that the "A" subkey is showing the single dot khmer letter.

</details>

<details>
<summary>Cleanup</summary>

1.    Type ‘Control panel’ in the search bar (in the taskbar).
2.    Verify that the ‘Control Panel’ appears in the menu list.
3.    Click the Control Panel option.
4.    Verify that the Control Panel dialog appears.
5.    Click the ‘Uninstall a Program’ option.
6.    Verify that the Programs and Features dialog box opens.
7.    Select the installed Keyman Developer version from the list.
8.    Click the Uninstall option.
9.    Verify that the Keyman Uninstallation was completed successfully.

</details>

---

**TEST_243**

Test case for Debugger_Pause,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `243`
- Product: `Developer`
- Source files: `243.JSON`, `243.html`

</details>

**Description**

Original TestLodge ID: TC154

<details>
<summary>Setup</summary>

1. Download Keyman Developer build from https://keyman.com/windows/ Site to the Desktop.
2. Install Amharic keyboard project.
3. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Open Keyman Developer application.
2. Click the Keyboards tab.
3. Verify ‘gff_amharic.kmn’ link button appears in the middle pane.
4. Click the ‘gff_amharic.kmn’ link button.
5. Verify the gff_amharic.kmn page opens.
6. Press the F7 button.
7. Verify the project is compiled successfully and the compiled message appears in the green color at the bottom of the page.
8. Press the F5 button.
9. Verify the ‘Debugger Mode’ view opens.
10. Press the Pause button (||) in the Debugger toolbar.
11. Verify that no input is possible in the debugger window.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build.
2. Uninstall Keyman Developer.
3. Uninstall Amharic Keyboard project.
4. Restart system.
5. Install Updates.
6. Clean up any broken installation artifacts (if they are available in the following folders)
%ProgramFiles(x86)%\Keyman or %ProgramFiles%\Keyman
%CommonProgramFiles(x86)%\Keyman or %CommonProgramFiles%\Keyman
%ProgramData%\Keyman
%AppData%\Keyman
%LocalAppData%\Keyman
HKCU\Software\Keyman
HKLM\Software\Wow6432Node\Keyman
8. Manually remove any remaining Keyman input methods from Windows Languages

</details>

---

**TEST_241**

Test case for Debugger_Starts ,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `241`
- Product: `Developer`
- Source files: `241.JSON`, `241.html`

</details>

**Description**

Original TestLodge ID: TC152

<details>
<summary>Setup</summary>

1. Download Keyman Developer build from https://keyman.com/windows/ Site to the Desktop.
2. Install Amharic Keyboard project .
3. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Open Keyman Developer application.
2. Click the Keyboards tab.
3. Verify ‘gff_amharic.kmn’ link button appears in the middle pane.
4. Click the ‘gff_amharic.kmn’ link button.
5. Verify the gff_amharic.kmn page opens.
6. Press the F7 button.
7. Verify the project is compiled successfully and the compiled message appears in the green color at the bottom of the page.
8. Press the F5 button.
9. Verify the ‘Debugger Mode’ view opens.
10.  Type with the keyboard in the debugger window.
11. Verify any unexpected behavior or visual oddities.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build.
2. Uninstall Keyman Developer.
3. Uninstall Amharic keyboard Project.
4. Restart system.
5. Install Updates.
6. Clean up any broken installation artifacts (if they are available in the following folders)
%ProgramFiles(x86)%\Keyman or %ProgramFiles%\Keyman
%CommonProgramFiles(x86)%\Keyman or %CommonProgramFiles%\Keyman
%ProgramData%\Keyman
%AppData%\Keyman
%LocalAppData%\Keyman
HKCU\Software\Keyman
HKLM\Software\Wow6432Node\Keyman
7. Manually remove any remaining Keyman input methods from Windows Languages

</details>

---

**TEST_248**

Test Case for Server_Keep_Alive,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `248`
- Product: `Developer`
- Source files: `248.JSON`, `248.html`

</details>

**Description**

Original TestLodge ID: TC159

<details>
<summary>Setup</summary>

1. Download Keyman Developer build from https://keyman.com/windows/ Site to the Desktop.
2. Install Amharic keyboard project.
3. Install Keyman Stable build.

</details>

<details>
<summary>Action</summary>

1. Open Keyman Developer application.
2. Click the Keyboards tab.
3. Verify ‘gff_amharic.kmn’ link button appears in the middle pane.
4. Click the ‘gff_amharic.kmn’ link button.
5. Verify the gff_amharic.kmn page opens.
6. Click the Tools menu.
7. Click Options.
8. Verify the Options dialog box opens.
9. Click the Server tab.
10. Check the ‘List Local URLs for Server’ option.
11. Click the OK button.
12. Click the Build tab.
13. Click the ‘Test Keyboard on web’ button.
14. Verify that the URL ‘ http://localhost:8008’ link appear under the web addresses pane.
15. Click the ‘Open in browser’ in button.
16. Verify that it opens in a local browser window.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build.
2. Uninstall Keyman Developer.
3. Uninstall Amharic keyboard project.
4. Restart system.
5. Install Updates.
6. Clean up any broken installation artifacts (if they are available in the following folders)
%ProgramFiles(x86)%\Keyman or %ProgramFiles%\Keyman
%CommonProgramFiles(x86)%\Keyman or %CommonProgramFiles%\Keyman
%ProgramData%\Keyman
%AppData%\Keyman
%LocalAppData%\Keyman
HKCU\Software\Keyman
HKLM\Software\Wow6432Node\Keyman
7. Manually remove any remaining Keyman input methods from Windows Languages

</details>

---

**TEST_246**

Test Case for Server_Starts,

Status: `Active`

<details>
<summary>Metadata</summary>

- Source ID: `246`
- Product: `Developer`
- Source files: `246.JSON`, `246.html`

</details>

**Description**

Original TestLodge ID: TC157

<details>
<summary>Setup</summary>

1. Download Keyman Developer build from https://keyman.com/windows/ Site to the Desktop.
2. Install Amharic keyboard project.
3. Install Keyman build.

</details>

<details>
<summary>Action</summary>

1. Open Keyman Developer application.
2. Click the Keyboards tab.
3. Verify ‘gff_amharic.kmn’ link button appears in the middle pane.
4. Click the ‘gff_amharic.kmn’ link button.
5. Verify the gff_amharic.kmn page opens.
6. Verify that the Server starts by examining the tool tray area.
7. Click the ‘Build’ tab which appears in the left most column.
8. Click the ‘Test Keyboard on web’ button under ‘Web and Mobile Targets’ pane.
9. Verify that ‘http://localhost:8008’ link appears under the web address pane.
10.  Select http://localhost:8008 link.
11. Click the ‘Open in browser’ button.
12. Verify that the localhost page is open in the browser.

</details>

<details>
<summary>Cleanup</summary>

1. Uninstall Keyman build.
2. Uninstall Keyman Developer.
3. Uninstall Amharic keyboard project.
4. Restart system.
5. Install Updates.
6. Clean up any broken installation artifacts (if they are available in the following folders)
%ProgramFiles(x86)%\Keyman or %ProgramFiles%\Keyman
%CommonProgramFiles(x86)%\Keyman or %CommonProgramFiles%\Keyman
%ProgramData%\Keyman
%AppData%\Keyman
%LocalAppData%\Keyman
HKCU\Software\Keyman
HKLM\Software\Wow6432Node\Keyman
7. Manually remove any remaining Keyman input methods from Windows Languages

</details>
