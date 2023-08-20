# Keyman for Android & Keyman Engine for Android

## Prerequisites
See [build configuration](../docs/build/index.md) for details on how to configure your build environment.

## Keyman Minimum Android Requirements
Keyman for Android has a minSdkVersion of 21 for [Android 5.0 Lollipop](https://developer.android.com/about/versions/lollipop)

## Keyman for Android Development
Keyman for Android (formerly named KMAPro) can be built from a command line (preferred) or Android Studio.

Building Keyman Web is a precursor for compiling KMEA, so verify your system has all the [Minimum Web Compilation Requirements](../web/README.md#minimum-web-compilation-requirements)

### Crash Reporting
Keyman for Android uses [Sentry](https://sentry.io) for crash reporting at a server https://sentry.keyman.com. The analytics for Debug are associated with an App Bundle ID `com.tavultesoft.kmapro.debug`.
 
### Compiling From Command Line
1. Launch a command prompt and cd to the directory **keyman/android**
2. Run the top level build script `./build.sh -debug` which will:
    * Compile KMEA (and its KMW dependency)
    * Download default keyboard and dictionary resources as needed
    * Compile KMAPro
    * Note: to force an update to the latest keyboard and dictionary packages, use the `-download-resources` flag.

3. The APK will be found in **keyman/android/KMAPro/kMAPro/build/outputs/apk/debug/kMAPro-debug.apk**

### Compiling From Android Studio
1. Ensure that [Keyman Engine for Android](#how-to-build-keyman-engine-for-android) is built.
2. Launch Android Studio and import the Gradle project **keyman/android/KMAPro/build.gradle**
3. From the project view, *configure* anything that Gradle reports.
4. Create a run configuration for kMAPro
    1. Select Run --> Edit Configurations...
    2. Select Add New Configuration (+) --> Android App
    3. Change Module to kMAPro
    4. Name your run configuration kMAPro (or as desired)
5. Run your new configuration: Select Run --> Run 'kMAPro'
6. Select a physical device or create a new virtual device to match your target API version
    For Ubuntu 18.04, you will need to add your user to the `kvm` group for permission accessing the emulator.
```
    sudo apt install qemu-kvm
    sudo adduser <username> kvm
```

### Running From Command Line
1. Launch a command prompt
2. Ensure that a physical device is connected or an emulator is running.
    1. To list physical devices: `$ANDROID_HOME/platform-tools/adb.exe devices`
    2. To list created emulator devices: `$ANDROID_HOME/tools/emulator.exe -list-avds`
    3. [Create a new virtual device](https://developer.android.com/studio/run/managing-avds.html) if there are no
    existing devices.    
    4. Start an emulator: `$ANDROID_HOME/tools/emulator.exe @nameofemulator`
3. Load the APK: `$ANDROID_HOME/platform-tools/adb.exe install -r path/to/apk.apk`
    * If multiple devices are connected you may need `$ANDROID_HOME/platform-tools/adb.exe install -r -s SERIAL path/to/apk.apk`. Replace `SERIAL` with
       the device serial number listed in step 2.

### Compiling the app's offline help
Keyman for Android help is maintained in the Markdown files in android/help/.
The script `build-help.sh` uses the `pandoc` tool to convert the Markdown files into html.

```bash
./build-help.sh htm
```

This script is automatically called when Keyman for Android is built.

### Sample Projects

There are two included sample projects that can be modified to test a keyboard.

**android/Samples/KMSample1** app runs a bare Keyman app for testing a keyboard.

**android/Samples/KMSample2** app provides prompts for setting KMSample2 as a system level keyboard.
Both sample apps include a default Tamil keyboard.

Building these projects follow the same steps as KMAPro:

1. Build KMEA
2. cd to the desired KMSample directory
3. `./build.sh`
4. Open Android Studio to run the app

### Tests: KeyboardHarness

**android/Tests/KeyboardHarness** app is a test harness for developers to troubleshoot keyboards.

1. Copy the keyboard js file and applicable ttf fonts to *android/Tests/KeyboardHarness/app/src/main/assets/*.
2. In Keyman Developer
  * Open the `keyboardharness.kpj` project and add your keyboard files to the keyboard package.
  * Build the keyboardharness.kmp keyboard package
3. Add the keyboard in *android/Tests/KeyboardHarness/app/src/main/java/com/keyman/android/tests/keyboardHarness/MainActivity.java*
4. cd to android/Tests/KeyboardHarness/
5. `./build.sh`
6. Open Android Studio to run the app

--------------------------------------------------------------

## How to Build Keyman Engine for Android
1. Open a terminal or Git Bash prompt and go to Keyman Engine for Android project folder (e.g. `cd ~/keyman/android/KMEA/`)
2. Run `./build.sh`

Keyman Engine for Android library (**keyman-engine.aar**) is now ready to be imported in any project.

## How to Use Keyman Engine for Android Library

1. Add **keyman-engine.aar** into **[Your project folder]/app/libs/** folder.
    a. We recommend [downloading](https://keyman.com/downloads/#android-engine) the the latest stable release of Keyman Engine and extracting the .aar file.
    b. If you choose to use your own build of the Keyman Engine, get the library from **android/Samples/KMSample1/app/libs/keyman-engine.aar**
2. Open your project in Android Studio.
3. Open **build.gradle** (Module: app) in "Gradle Scripts".
4. Check that the `android{}` object, includes the following:
```gradle
android {
    compileSdkVersion 33

    // Don't compress kmp files so they can be copied via AssetManager
    aaptOptions {
        noCompress "kmp"
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
    }
```
5. After the `android {}` object, include the following:
````gradle
repositories {
    flatDir {
        dirs 'libs'
    }
    google()
    mavenCentral()
}

dependencies {
    implementation fileTree(dir: 'libs', include: ['*.jar'])
    implementation 'androidx.appcompat:appcompat:1.3.0-rc01'
    implementation 'com.google.android.material:material:1.3.0'
    api (name:'keyman-engine', ext:'aar')
    implementation 'io.sentry:sentry-android:4.3.0'
    implementation 'androidx.preference:preference:1.1.1'

    // Include this if you want to have QR Codes displayed on Keyboard Info
    implementation ('com.github.kenglxn.QRGen:android:2.6.0') {
        transitive = true
    }
}

````
5. include `import com.tavultesoft.kmea.*;` to use Keyman Engine in a class.
