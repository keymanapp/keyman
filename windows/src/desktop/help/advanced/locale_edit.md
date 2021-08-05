---
title: Translating the Keyman for Windows User Interface
---

## Online User Interface Translation Editor

Localization of the user interface is possible by using the online
interface at [translate.keyman.com](https://translate.keyman.com/).

## Notes on editing translations

Nearly all strings have identifiers (e.g. SKButtonOK for the OK button).
We have three different types of strings in the file - plain, HTML and
format strings:

-   Format strings often include `%#:s` or `%#:d` markers that are
    placeholders for parameters, and should be included in a translated
    string. The order of the markers in the translated string is not
    important.

    Example:
    `<String Type="FormatString" Id="SKPackageAlreadyInstalled">A package with the name %0:s is already installed.  Do you want to uninstall it and install the new one?</String>`

-   Plain strings must be just that - plain text. These are all
    referenced by the XSL templates that make up the bulk of the Keyman
    for Windows user interface. You should not place any HTML tags in
    these entries as they will be stripped before display.

    Example:
    `<String Type="PlainText" Id="S_ShortProductName">Keyman for Windows</String>`

-   HTML strings can include tags - there are only a few of these. The
    most obvious of these is in the Welcome screen text.

    Example:
    `<String Type="HTML" Id="S_Splash_Button_Start" AccessKey="S"><u>S</u>tart</String>`

The &amp; entity is used to mark hotkeys in menu items, labels and
buttons. It can be used only with strings identified as formatted
messages (these mostly start with SK) and with the Menu strings.

## Hints on editing a translation

First, edit the translation's language information - SKUILanguageName,
SKUILanguageNameWithEnglish, and SKLanguageCode. The SKLanguageCode
should be the same as the language code you chose earlier. You will also
see a String with id SKDefaultLanguageCode. For Keyman for Windows, this
should remain "en" for all translations. When developing a custom
product using the Keyman Engine for Windows, you may change your
product's default language, and this would then entail changing
SKDefaultLanguageCode.

**Note:** Any entries missing from the translation will be retrieved from the
default file.

## Related Topics

-   [How To - Set the Language for Keyman Menus](../start/locale)
