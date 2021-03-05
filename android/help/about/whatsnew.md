---
title: What's New
---

Here are some of the new features we have added to Keyman for Android 14.0:

* Improved UI for installing keyboard packages (#3498)
* Select a language during keyboard package installation (#3481)

![](../android_images/select_language.png)

* Added new menu to add languages for an installed keyboard package (#3255)
* Consolidated install menus for installing keyboards (#3245)
* Fix slow input in the embedded browser (#3768)
* Add system globe action `GLOBE_KEY_ACTION_SHOW_SYSTEM_KEYBOARDS ` to show system keyboards (#3197)
* Improved corrections and predictions (#3555)
* Match user input capital letters when offering suggestions (#3845)
* Keyman now works more reliably with WeChat and Telegram (#4254)
* Added new Settings menu to [Change Display Language](../basic/config/index#Change-Display-Language): (#4261)
    * French
    * German
    * Khmer
    * Obolo

## Keyman Engine for Android Breaking Changes: ##
* Moved minimum Android SDK to from 19 to 21 (Android 5.0 Lollipop) with corresponding Chromium minimum release M37 (#2993)
* Keyboards now strictly installed from .kmp keyboard packages. 3rd party apps should include their "default" keyboard and lexical model packages at the project's `assets/` level. KMManager will extract them into `assets/packages/` and `assets/models/` respectively. (No longer append version string to .js keyboard file and manually copy to `assets/cloud/` folder)
* KMEA no longer includes sil_euro_latin as a default installed keyboard

* Replace Fabric/Crashlytics with Sentry for crash reporting
* build.gradle changes for 3rd party keyboard apps:
```gradle
android {
  // Don't compress kmp files so they can be copied via AssetManager
  aaptOptions {
     noCompress "kmp"
  }

  compileOptions {
    sourceCompatibility = JavaVersion.VERSION_1_8
    targetCompatibility = JavaVersion.VERSION_1_8
  }
}

dependencies {
    implementation 'io.sentry:sentry-android:3.1.0'
    implementation 'androidx.preference:preference:1.1.1'
}
```
* Deprecated KMManager APIs
    * `KMManager.showLanguageList()` - KMEA no longer uses cloud keyboard catalog, so showLanguageList() no longer displays. (Embedded browser used to search for keyboards)
    * `KMManager.KMKey_CustomKeyboard` - "Custom" property for keyboards no longer tracked
    * `KMManager.KMKey_CustomModel` - "Custom" property for lexical models no longer tracked
    * `addKeyboard` - Deprecate syntax using `HashMap<string, string>`. Replace with `<Keyboard>`
