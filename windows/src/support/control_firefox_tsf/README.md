# Keyman Compatibility Mode Utilities

## Introduction

These two small helper utility programs have been created to make it simple for end users to control how Keyman integrates with Chrome and Firefox. As of 21 October 2020, there is a compatibility issue with both of these browsers which prevents normal use of Keyman. When Keyman is run in a compatibility mode, it avoids this compatibility problem.

The Keyman team is working with the Chrome and Firefox teams to resolve this compatibility issue. We recommend re-enabling the default integration once the issue is resolved; see the links at the bottom of this document to follow progress on this.

## Usage

1. Download the `control_chrome_tsf.exe` or `control_firefox_tsf.exe` program and run it.
2. You will be prompted to elevate to Administrator, and you will need to allow this to continue.
3. Read the instructions in the dialog that is presented to you; select **Yes** to enable the default integration, or select **No** to disable the default integration and use the compatibility mode.
4. Once this tool has run, it will close. There will be no further feedback.
5. You may need to restart your browser for the changes to take effect. You do not need to run the tool again unless you want to change the compatibility mode.

## Downloads

* Google Chrome helper: https://downloads.keyman.com/tools/control_tsf/control_chrome_tsf.exe
* Mozilla Firefox helper: https://downloads.keyman.com/tools/control_tsf/control_firefox_tsf.exe

## Applies To

* Keyman Desktop for Windows 11.0-13.0
* Keyman for Windows 14.0 and later versions

## Background

This utility adds/removes a setting at `HKLM\Software\(Wow6432Node\)Keyman\Keyman Engine\App Integration`, called `firefox.exe` or `chrome.exe` (type `REG_DWORD`):
* if set to `0`, this disables the default integration and uses compatibility mode for that executable.
* if set to `1`, this enables the default integration and uses compatibility mode for that executable.
* if not present, this defaults to the global setting (normally, enabled).

The global setting is `HKLM\Software\(Wow6432Node\)Keyman\Keyman Engine`, value `deep tsf integration` (type `REG_DWORD`). This uses the same values as above. You can opt to set the compatibility mode for just one application or for the whole system. We recommend setting compatibility mode just for the application for which you are experiencing a problem.

## References

* https://community.software.sil.org/t/backspace-chrome-edge-86-and-keyman/3948
* https://help.keyman.com/kb/94
