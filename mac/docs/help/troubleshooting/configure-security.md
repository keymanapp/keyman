---
title: How to configure macOS security options for Keyman
---

Keyman requires access to your keyboard in order to translate input. In macOS Catalina, there
are three different Privacy settings you will need to configure in order for Keyman to work
correctly:

* **Privacy** / **Input Monitoring**
* **Privacy** / **Accessibility**
* **Privacy** / **Files and Folders**

The Keyman installer will configure these privacy settings for you, and prompt you to allow access for Keyman. If you do not allow access at that time, you will need to follow the steps here in order to use Keyman.

Symptoms that you may experience if Security & Privacy is not configured correctly include:

* Some key combinations may not work
* You may find that some letters are duplicated or not deleted as you expect

**Note:** Keyman may not appear under these settings until it has been run at least one
time. If Keyman does not appear in the relevant panes in System Preferences, select Keyman
from the Input Sources menu and try typing in an app.

In some situations, even then, Keyman won't appear in the Accessibility setting. In this case,
macOS should prompt you the first time that Keyman needs access to Accessibility.

## Enabling Keyman in macOS Privacy System Preferences

1. Open **System Preferences**:

   ![System Preferences menu](../mac_images/apple_menu.png)

2. Select **Security & Privacy**, and choose the **Privacy** tab:

   ![System Preferences](../mac_images/system_preferences.png)

3. Select **Input Monitoring** and unlock the settings with the lock icon,

   ![Input Monitoring - not checked](../mac_images/privacy_input_monitoring_locked.png)

4. Keyman should appear in the list. Tick the Keyman item:

   ![Input Monitoring](../mac_images/privacy_input_monitoring.png)

5. Select **Accessibility**, and ensure that Keyman is ticked there as well:

   ![Accessibility](../mac_images/privacy_accessibility.png)

6. Select **Files and Folders**, and ensure that Keyman has access to the **Documents** folder there:

   ![Files and Folders](../mac_images/privacy_files_and_folders.png)

**Note:** You may be prompted to shut down Keyman in order for the changes to take
effect. If so, follow the prompts to allow the system to close Keyman. You'll need
to select an alternate input source from the Input Sources, and then switch back to
Keyman, to start using Keyman again.

## Upgrading macOS

You should only have to undertake this procedure once. However, if you upgrade your
version of macOS, and Keyman stops working, follow these steps to re-enable Keyman.
