---
title: How to test your keyboard layout — touch and desktop
---

Keyman Developer includes full touch layout editing tools. In the image
below I am editing a Khmer Angkor touch layout sample.

[![Touch Editor -
Khmer](../../images/testing/touch-editor-khmer-800wi.png "Touch Editor - Khmer")](../../images/testing/touch-editor-khmer.png)

Once you have created your keyboard layout, you need to test it. Your
keyboard layout may cover desktops as well as touch devices. The
**Build** tab of the keyboard editor gives you access to testing and
debugging commands for all platforms.

[![Touch Editor - Build
tab](../../images/testing/touch-editor-build-800wi.png "Touch Editor - Build tab")](../../images/testing/touch-editor-build.png)

Testing is easy on Windows: press the **Start Debugging** button on the
**Build** tab to test the rules in your keyboard layout with a [fully
interactive debugger](../../context/debug). This allows you to step
through complex rules, inspecting the contents of the stores referenced
in each rule, and examine the context, deadkey state and output at each
point.

After validating your rules, you will want to test the [on screen
keyboard](../../context/keyboard-editor#toc-on-screen-tab) and how your
keyboard feels within other applications in Windows.  We recommend
[creating a package](../distribute/tutorial) in order to simplify the
installation of both the on screen keyboard and the keyboard (which are
stored as two separate files). Then click the **Install** button in
**Build** tab of the package wizard and you are done.

## Preparing to test

But how do you test your touch layout on your iPhone, iPad or Android
device? We've thought about this as well. Keyman Developer includes a
web server specifically for testing and installing keyboard layouts on
these devices.

To start the web server and debugging session, on the **Build** tab of
your keyboard editor, click **Test Keyboard on web** (we may rename
these buttons in a later release to reflect the cross-platform targets
here).

You may need to open up the host port on your local computer firewall;
most firewall software will prompt you at this point. The default port
that Keyman Developer uses is 8008. You can change this in the
**Tools**/**Options**/**Debugger** dialog if necessary.

You will see a list of web addresses in the list box below the **Test
Keyboard on web** label. This lists all the computer and domain names
and IP addresses that Keyman Developer is listening on that we can find.
Why so many? Because of the variety of network setups, different users
will need to use different addresses; Keyman Developer leaves that up to
you! We call the list the **Debug Host List**.

![KeymanWeb Debug
Host](../../images/testing/startdebugging-kd10.png "KeymanWeb Debug Host")

## Testing your web keyboard on your desktop

You can test your desktop layout by selecting an address in the debug
host list and clicking **Open debugger in local browser**.  This will
load the KeymanWeb Debug Host page in your browser, and you should be
able to select your keyboard from the list and start using it.

## Testing your keyboard on a touch device

To start testing on your mobile device, your device needs to be on the
same network as your PC. Then open your web browser on the device. Which
browser? On iOS, use Safari.  On Android devices, we recommend using
Chrome. We don't recommend the Android Browser, because its capabilities
vary dramatically depending on the Android version and the brand of your
device.

Then you will need to pick an address from the debug host list in Keyman
Developer that your device can find. An IP address on the same network
is almost always going to work, but you may find that a canonical domain
name works as well, if you are in an environment with a name server. The
localhost and 127.0.0.1 addresses are local to your PC -- so don't try
those.

Enter the address into the URL bar on your browser; don't forget to type
the **:8008** port at the end of the address you choose (on most devices
you can skip the **http://** at the start):

Alternatively, you can click the **Send addresses to email...** button.
It sends a list of the debug host addresses to an email address of your
choice, so you can then just click the link in your email on your target
device.

![Enter the debug host
URL](../../images/testing/frame/android-enter-debug-host.png "Enter the debug host URL")

All going well, you should be presented with a page like this:

![Viewing the debug host on
iPhone](../../images/testing/frame/android-debug-host.png "Viewing the debug host on iPhone")

If you get a Host Not Found error, try different addresses from the
**KeymanWeb Debug Host** list, check your computer's firewall settings,
and make sure that both devices are on the same network.

Now you can instantly test your layout in KeymanWeb Touch, by clicking
in the blank edit box: the touch layout will appear. If you make changes
in Keyman Developer, just recompile (**Compile Keyboard** button, or
F7), and reload on the touch device -- you don't need to click the
**Test Keyboard on web** button again.

![Testing a keyboard on
iPhone](../../images/testing/frame/android-debug-keyboard.png "Testing a keyboard on iPhone")

## Loading your keyboard into the native Keyman apps

If you do not have Keyman currently installed on your target device, you
can click **Install Keyman for Android** or **Install Keyman for iOS**
to bring up the appropriate store for installing Keyman.

Finally, you can also install your keyboard layout directly into Keyman
for Android or Keyman for iPhone and iPad by clicking the **Add keyboard
to Keyman for Android** button or **Add keyboard to Keyman for iOS**
button on the test page on your device.

![Installing the keyboard into native
app](../../images/testing/frame/installing-native-keyboard-1.png "Installing the keyboard into native app")

![Installing the keyboard into native
app](../../images/testing/frame/installing-native-keyboard-2.png "Installing the keyboard into native app")

Now your keyboard layout is installed and available in the Keyman App,
and if you have the Keyman version with the system keyboard support
installed, across all apps.

You can test multiple keyboards just by clicking **Test Keyboard on
web** in each keyboard editor.

## Related articles

Other articles on developing touch layouts:

-   [Creating a touch keyboard layout for Amharic - part
    1](../develop/creating-a-touch-keyboard-layout-for-amharic)
-   [Creating a touch keyboard layout for Amharic - the nitty
    gritty](../develop/creating-a-touch-keyboard-layout-for-amharic-the-nitty-gritty)

You can distribute your keyboard to other users by following the
instructions in this article:

-   [Distribute keyboards to Keyman
    applications](../distribute/packages)
