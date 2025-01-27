---
title: Guide: build a system keyboard app for Android with Keyman Engine
---

In [part 1](../in-app/) of this series, we looked at the steps involved
in creating a basic Android app which included a Keyman in-app keyboard.
In this post, we'll work with the second sample included in the Keyman
Engine for Android package, which is a very basic System Keyboard app.
This post builds on concepts and setup from the first post, so be sure
you are familiar with that before you start on this one.

### 1. Basic configuration

If **KMSample2.zip** exists from the downloaded **Keyman Engine for
Android**, extract it to a new folder. Otherwise, the KMSample2 project
can be found at **Samples/KMSample2**

### 2. Run the sample app

The KMSample2 app includes the Tamil 99 Basic keyboard as an example
keyboard. Without any further modifications, the app should run and
you'll be able to configure your device to use your app as the system
keyboard.

[<img src="/cdn/dev/img/engine/android/14.0/guides/system-keyboard/emulator-800wi.png" title="Emulator" alt="Emulator">](/cdn/dev/img/engine/android/14.0/guides/system-keyboard/emulator-800wi.png)

### 3. Extending the app

From here, you will no doubt want to replace the keyboard with your own
one; again, follow the instructions from Part 1 to make this change.

You can also customise the look and feel of the on screen keyboard by
including a custom CSS file in your keyboard through Keyman Developer.

[<img src="/cdn/dev/img/engine/android/10.0/guides/system-keyboard/add-embedded-css.png" title="AddEmbeddedCSS" alt="AddEmbeddedCSS">](/cdn/dev/img/engine/android/10.0/guides/system-keyboard/add-embedded-css.png)

The CSS rules required to style your keyboard are beyond the scope of
this post, but the **kmwosk.css** file included in the Keyman Developer
16.0 distribution and in the [KeymanWeb 16.0
source](https://github.com/keymanapp/keyman/blob/master/web/source/resources/osk/kmwosk.css) is
a good place to start. See this
[reference](/developer/engine/web/15.0/reference/osk/classes) for more
information about the On-Screen Keyboard.

Do take the time to read through the source of **KMSample2**, as it
includes some boilerplate code required to link Keyman Engine through to
the Android Input Method services and vice versa.

Finally, you will want to improve the style and branding of the main
activity in KMSample2. The sample includes just two buttons to link to
the Android Input Method Settings and Input Method Menu, as a pointer to
the two configuration steps that your users will need to undertake in
order to start using your keyboard.

[<img src="/cdn/dev/img/engine/android/14.0/guides/system-keyboard/main-activity-layout-800wi.png" title="MainActivityLayout" alt="MainActivityLayout">](/cdn/dev/img/engine/android/14.0/guides/system-keyboard/main-activity-layout-800wi.png)

You will probably want to make that a lot prettier, and include more
detailed instructions!

That's all there is to creating a System Keyboard with Keyman Engine for
Android. We've taken care of all the complex details of keyboarding in
the Keyman Engine, so you can focus on the look and feel and the layout
of your keyboard.

### Further links

-   [Part 1 of this series](../in-app/)
-   [Keyman Developer Documentation](/developer/16.0/)
-   [Android Developer Home](http://developer.android.com/index.html)
