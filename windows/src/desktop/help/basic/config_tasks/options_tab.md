---
title: Keyman Configuration - Options Tab {#basic_options_tab}
---

The Options tab of Keyman Configuration includes the Keyman application
options. You can use the Options tab to set general, startup, Keyman
Toolbox On Screen Keyboard and advanced options.

![](desktop_images/tab-options2.png)

**Opening the Options Tab**

To open the Options tab of Keyman Configuration:

1.  Click on the Keyman icon ![](desktop_images/icon-keyman.png), on the
    Windows Taskbar near the clock.

2.  From the Keyman menu, select Configuration....

3.  Select the Options tab.

    **Tip:**
    Keyman Configuration opens in the same tab you last closed it in.

**Using General Options**

![](desktop_images/options-general.png)

-   Keyboard hotkeys toggle keyboard activation

    You can set unique hotkeys for quick access to any Keyman keyboard.
    Tick this option to allow a Keyman keyboard hotkey to toggle a
    keyboard on and off. Untick this option and a Keyman keyboard hotkey
    will only turn the Keyman keyboard on.

    **Tip:**
    Set hotkeys in the
    Hotkeys tab
    of Keyman Configuration.

-   Simulate AltGr with Ctrl+Alt

    Some hardware keyboards do not have a designated AltGr key (Right
    Alt key). Some Keyman keyboards require AltGr. Tick this option to
    allow Ctrl+Alt to be used for AltGr. Untick this option and Ctrl+Alt
    will not be used for AltGr.

-   Select keyboard layout for all applications

    Unlike Windows default behaviour, Keyman allows you to select one
    Windows language and Keyman keyboard for all open applications and
    text fields across your entire system. Tick this option to select
    one Windows language and Keyman keyboard across your entire system.
    Untick this option to select Windows language and Keyman keyboards
    independently for different programs.

    On Windows 8, 8.1, 10 and later versions, this checkbox is disabled
    because Windows has this functionality built in. The setting can be
    changed in Windows, with the following steps:

    -   Open Control Panel
    -   Select Language, Advanced Settings
    -   Check the \"Let me set a different input method for each app
        window\" checkbox.

-   Show hint messages

    You can turn on and off the beginner hint messages which appear
    while using Keyman. Tick this option to show all hint messages.
    Untick this option to hide all hint messages.

    **Note:**
    You can also hide hint messages on a case-by-case bases by ticking
    at the bottom of each hint.

-   Reset Hints

    Click the Reset Hints button to switch all hint messages on again,
    even those that you have switched off on a case-by-case basis.

**Using Startup Options**

![](desktop_images/options-startup.png)

-   Start when Windows starts

    You can have Keyman start when Windows starts. Tick this option to
    have Keyman start when Windows starts. Untick this option to start
    Keyman manually.

-   Show splash screen

    You can turn on and off the Keyman splash screen that appears when
    Keyman starts. Tick this option to show the splash screen on
    startup. Untick this option to remove the splash screen.

-   Automatically check www.keyman.com weekly for updates.

    Keyman can automatically check for application and keyboard updates
    once a week. No personally identifiable information is sent to
    Keyman in this process. Tick this option to have Keyman
    automatically check weekly for updates to Keyman and the keyboards
    you have installed. Untick this option to check for updates
    manually.

    **Note:**
    Keyman can be updated manually through the Support tab in the
    Configuration menu.

-   Test for conflicting applications when Keyman starts

    Some applications are not written according to the Windows software
    development standards and can cause problems with Keyman keyboard
    input. Tick this option to have Keyman detect problem applications
    and work around them. Untick this option to prevent Keyman from
    working around problem applications.

**Using On Screen Keyboard Options**

![](desktop_images/options-osk.png)

-   Release Shift/Ctrl/Alt on On Screen Keyboard after clicking a key

    By default, the On Screen Keyboard of the Keyman Toolbox uses
    \'sticky\' Ctrl, Shift or Alt keys. This means Ctrl, Shift, and Alt
    stay down on the On Screen Keyboard until they are clicked again or
    until you press the actual keys on your hardware keyboard. Tick this
    option to disable the default sticky key behaviour. Untick this
    option to enable the default sticky key behaviour.

-   Always show On Screen Keyboard Window when Keyman keyboard is
    selected

    You can have the On Screen Keyboard view of the Keyman Toolbox
    automatically display whenever you turn on a Keyman keyboard. Tick
    this option to have the On Screen Keyboard view for your keyboard
    automatically display. Untick this option to prevent this behaviour.

-   Switch to appropriate On Screen Keyboard/Help automatically when a
    keyboard is selected

    Each time you turn on a Keyman keyboard, Keyman can automatically
    detect and switch to the Keyman Toolbox tool which is best for that
    Keyman keyboard: either the On Screen Keyboard or Keyboard Usage.
    Tick this option to have Keyman automatically detect and display the
    best view for your Keyman keyboard. Untick this option to prevent
    Keyman from switching its display automatically.

**Using Advanced Options**

![](desktop_images/options-advanced.png)

-   Debugging

    If you are having issues with Keyman, you can create a debug log for
    Keyman. A debug log contains information to help Keyman Technical
    Support resolve compatibility issues and improve Keyman. Tick this
    option to begin a debug log. You should not need to use this option
    unless you are requested to do so by Keyman Technical Support in
    relation to an issue you are experiencing.

    The debug log files are created in %LOCALAPPDATA%\\Keyman\\Diag, and
    called system\#.etl, where \# is a number from 0 to 15. Each time
    you start Keyman, a new log file will be created, up to
    system15.etl, after which it will wrap around and start again at
    system0.etl. Untick this option to stop collecting a debug log.

    Debug log files will be sent to Keyman Technical Support when you
    send a diagnostic report to us with the Keyman System Information
    tool.

    Debug log files are in binary format and can be imported into
    Windows Event Viewer for viewing, or Microsoft Message Analyzer
    (recommended).

    **Note:**
    It is highly recommended that you switch off this option after
    obtaining the debug log. The log files can grow very large very
    quickly!

    ::: {.warning}
    Privacy Warning - The debug log records every keystroke you type,
    even if a Keyman keyboard is not active. This includes logins and
    passwords! For this reason, we recommend only switching the debug
    log on for short and isolated tests as specifically requested by
    Keyman Technical Support.
    :::

**Related Topics**

-   [???](#basic_config_menu)

-   [???](#basic_hotkeys_tab)
