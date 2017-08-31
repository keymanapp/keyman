# Keyman for Android & Keyman Engine for Android

## Minimum Android Requirements
Keyman for Android has a minSdkVersion of 15 for [Android 4.0.3 Ice Cream Sandwich](https://developer.android.com/about/versions/android-4.0.3.html)

## Setup Android Studio

1. Download [Android Studio](https://developer.android.com/studio/index.html) and install with these [instructions](https://developer.android.com/studio/install.html).
2. For Windows users, set environment variable **ANDROID_HOME** to the location of your Android SDK. The default installation location is **C:\Users\\[USER]\AppData\Local\Android\sdk** where [USER] is your username. You may need to log out and log back in to take effect.

For MacOS/Linux users, add the following to **~/.bashrc** or **~/.bash_profile**
```
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools
```
3. For Windows users, from a Git Bash Prompt window, cd to the **sdk/tools/bin** folder and accept all the SDK license agreements
```
yes | ./sdkmanager.bat --licenses
```
4. If you plan to test on a physical device via USB, install the appropriate [OEM USB drivers](https://developer.android.com/studio/run/oem-usb.html)
5. Install [Java SE Development Kit](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)

## Keyman for Android Development
Keyman for Android (formerly named KMAPro) can be built from a command line (preferred) or Android Studio.

### Compiling From Command Line
1. Launch a command prompt
2. Change to one of these directories depending on what you want to compile:
    * For compiling KMEA and KMAPro, cd to the directory **keyman/android**
    * For compiling only KMAPro, cd to the directory **keyman/android/KMAPro**
3. `./build.sh`
4. The APK will be found in **KMAPro/kMAPro/build/outputs/apk/kMAPro-*.apk**

### Compiling From Android Studio
1. Ensure that [Keyman Engine for Android](#how-to-build-keyman-engine-for-android) is built.
2. Launch Android Studio and import the Gradle project **keyman/android/KMAPro/build.gradle**
3. From the project view, *configure* anything that Gradle reports.
4. Create a run configuration for kMAPro
    i. Select Run --> Edit Configurations...
    ii. Select Add New Configuration (+) --> Android App
    iii. Change Module to kMAPro
    iv. Name your run configuration kMAPro (or as desired)
5. Run your new configuration: Select Run --> Run 'kMAPro'
6. Select a physical device or create a new virtual device to match your target API version

### Running From Command Line
1. Launch a command prompt
2. cd to the **$ANDROID_HOME/tools** directory
3. Ensure that a physical device is connected or an emulator is running.
    i. To list physical devices: `adb devices`
    ii. To list created emulator devices: `emulator -list-avds`
    iii. [Create a new virtual device](https://developer.android.com/studio/run/managing-avds.html) if there are no
    existing devices.
    iv. Start an emulator: `emulator @nameofemulator`
4. Load the APK: `adb install -r path/to/apk.apk`
    i. If multiple devices are connected you may need `adb install -r -s SERIAL path/to/apk.apk`. Replace `SERIAL` with
       the device serial number listed in step 3.

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

--------------------------------------------------------------

## How to Build Keyman Engine for Android
1. Open a terminal or Git Bash prompt and go to Keyman Engine for Android project folder (e.g. `cd ~/keyman/android/KMEA/`)
2. Run `./build.sh`

Keyman Engine for Android library (**keyman-engine.aar**) is now ready to be imported in any project.
A copy can be found at **android/Samples/KMSample1/app/libs/keyman-engine.aar**.

## How to Use Keyman Engine for Android Library

1. Copy **keyman-engine.aar** into **[Your project folder]/app/libs/** folder.
2. Open your project in Android Studio.
3. Open **build.gradle** (Module: app) in "Gradle Scripts".
4. include `compile(name:'keyman-engine', ext:'aar')` in dependencies.
5. after dependencies, insert
````
    repositories {
        mavenCentral()
        flatDir {
            dirs 'libs'
        }
    }
````
6. include `import com.tavultesoft.kmea.*;` to use Keyman Engine in a class.
