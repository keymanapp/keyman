---
title: What's New
---

Here are some of the new features we have added to Keyman 14.0 for Windows:

-   We renamed the program from **Keyman Desktop** to **Keyman for Windows**.
    Now that Keyman runs on so many platforms, it didn't make sense for
    us to use a special, different name for Keyman on Windows.
-   Keyman keyboards are no longer hidden from the Windows language
    picker when you exit Keyman. Rather, if you select a Keyman
    keyboard, Keyman will be restarted for you automatically.
-   Simpler and Smoother Keyboard Search (\#3326)
-   Localizable user interface through
    [translate.keyman.com](https://translate.keyman.com)
-   Added user interface for configuring all Keyman system-level options
    (\#3733)
-   Refreshed user interface no longer depends on Internet Explorer
    (\#1720); now uses Chromium to host all web-based user interface
    (e.g. Keyman Configuration)
-   Smoother and more reliable installation of keyboard languages
    (\#3509)
-   Choose associated language when keyboard is installed (\#3524)
-   Improved BCP 47 tag support (\#3529)
-   Much improved initial download and installation experience including
    bundled keyboards (\#3304)
-   Keyman Configuration changes now apply instantly (\#3753)
-   Improved user experience when many keyboards are installed (\#3626,
    \#3627)
-   Improved error reporting with Sentry.io, so we can learn about
    and fix bugs more quickly

## Breaking Changes
-   Keyman Engine no longer supports the keyboard usage page
    (usage.htm). This functionality reduction arises because it is
    not safe to host a web page within the Keyman Toolbox, which runs
    at a higher level of privilege. We are looking for alternatives
    to restore similar functionality in a future update.

## Related Topics

-   [Version History](history)
-   [New Ways to Use Keyman](../basic/new-ways-to-use-keyman)
