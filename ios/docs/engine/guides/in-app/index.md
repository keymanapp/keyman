---
title: Guide: build an in-app keyboard for iPhone and iPad with Keyman Engine
---

Keyman Engine for iPhone and iPad allows you to use any Keyman touch
keyboard in your iOS app, or even to create your own system keyboard app
for purchase in the App Store.  
This guide will walk you through the steps for creating your first iOS
app with Keyman Engine for iPhone and iPad.

If you are not familiar with iOS development, you will find the ["Start Developing iOS Apps" online guide by Apple](https://developer.apple.com/tutorials/app-dev-training/) an invaluable resource, and working through some of their tutorials first will help you with the rest of this guide.

### 1. Install Free Tools

1.  Install [Keyman Developer 10 or later](https://keyman.com/developer/) on a Windows machine.
    -   Alternatively, it is possible to compile keyboards with the command-line tool [kmc](https://help.keyman.com/developer/current-version/reference/kmc/cli/reference), but we do not yet support a GUI interface for keyboard development on macOS.
2.  Install
    [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12).

### 2. Download the Keyman Engine for iOS and Sample Project

1.  Keyman v10+ users can download the engine from downloads.keyman.com

    Download the latest [keyman-engine-ios.zip](https://downloads.keyman.com/ios/stable/) and extract the files to a new folder.

The archive includes two sample projects and an iOS-targetting .framework file. This guide will use the first example project, **KMSample1**.

1.  The KMSample1 project can be found at **samples/KMSample1**.
2.  Double-click **KMSample1.xcodeproj** to begin, or open it via **File &gt; Open** to get started.

### 3. Create a keyboard layout

Use Keyman Developer to build a touch layout. The following documents walk through some of the development and testing for creating a touch keyboard layout:

-   [Creating a Touch Keyboard Layout for Amharic with Keyman Developer 12](/developer/current-version/guides/develop/creating-a-touch-keyboard-layout-for-amharic)
-   [Creating A Touch Keyboard Layout For Amharic — The Nitty Gritty](/developer/current-version/guides/develop/creating-a-touch-keyboard-layout-for-amharic-the-nitty-gritty)
-   [How to test your keyboard layout with Keyman Developer — touch and desktop](/developer/current-version/guides/test/keyboard-touch-and-desktop)

When your keyboard is ready, you should have a compiled keyboard package. The example below shows the Thamizha Tamil99 touch layout.

![Keyman-developer](/cdn/deploy/img/engine/ios/16.0/guides/in-app/keyboard-layout-in-developer-800wi.png "Keyman-developer")

Open the package tab for your keyboard and compile it. Then, open the folder containing your package

![Keyman-developer-open-containing-folder](/cdn/deploy/img/engine/ios/16.0/guides/in-app/find-package-via-developer-800wi.png "Keyman-developer-open-containing-folder")

![Compiled-keyboard-file](/cdn/deploy/img/engine/ios/16.0/guides/in-app/package-file-800wi.png "Compiled-keyboard-file")

### 4. Add your keyboard package to the project

Copy your compiled keyboard package (in this example **ekwtamil99uni.kmp**) to your macOS machine within the **KMSample1\Keyboards** folder.

![Copy-keyboard-file](/cdn/deploy/img/engine/ios/16.0/guides/in-app/package-import.png "Copy-keyboard-file")

When you switch back into Xcode, use the right-click context menu to "Add Files to 'KMSample1'":

![Android-studio-assets](/cdn/deploy/img/engine/ios/16.0/guides/in-app/xcode-package-import-800wi.png "Android-studio-assets")

Next, edit `ViewController.viewDidLoad()` to add the keyboard (ekwtamil99uni) with `Manager.shared` from KeymanEngine.framework.

``` swift
  // Add a custom keyboard
  let kmpFileURL = Bundle.main.url(forResource: "ekwtamil99uni", withExtension: "kmp")!
  let keyboardID = FullKeyboardID(keyboardID: "ekwtamil99uni", languageID: "ta")

  do {
    let package = try ResourceFileManager.shared.prepareKMPInstall(from: kmpFileURL) as! KeyboardKeymanPackage
    try ResourceFileManager.shared.install(resourceWithID: keyboardID, from: package)

    _ = Manager.shared.setKeyboard(withFullID: keyboardID)
  } catch {
    print("Error preloading: \(error)")
  }
```

![Android-studio-adding-keyboard](/cdn/deploy/img/engine/ios/16.0/guides/in-app/fully-imported-800wi.png "Android-studio-adding-keyboard")

### 5. Build and run the app

You can run your app through Xcode's built-in Simulator, which can
emulate a number of iOS devices. Simply choose a target device to test
against from the drop-down toward the top-left. For example, in the
previous image, you can see "iPod Touch (7th generation)" selected.

![Android-emulator-tamil](/cdn/deploy/img/engine/ios/16.0/guides/in-app/ios-simulator-tamil.png "Android-emulator-tamil")

And there you have it: your first Keyman Engine for iPhone and iPad app!

## See Also

-   [Guide: Build a system keyboard app](../system-keyboard/)
-   [Keyman Developer Documentation](/developer/current-version/)
-   [Keyman Engine for iPhone and iOS Documentation](../../)
-   [Apple Developer Home](https://developer.apple.com/)
