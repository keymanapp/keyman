Keyman Android Pro & Keyman Engine for Android
==============================================

Build Prerequisites
===================
Keyman for Android has a minSdkVersion of 15 for [Android 4.0.4 IceCream Sandwich](https://developer.android.com/about/versions/android-4.0.3.html)

1. Download [Android Studio](https://developer.android.com/studio/index.html) and install with these [instructions](https://developer.android.com/studio/install.html). 
2. For Windows users, set environment variable **ANDROID_HOME** to the location of your Android SDK. The default installation location is *C:\Users\[USER]\AppData\Local\Android\sdk* where [USER] is your username. You may need to log out and log back in to take effect.

For Linux users, add the following to ~/.bashrc
```
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools
```
3. From a Git Bash Prompt window, cd to the *sdk/tools/bin* folder and accept all the SDK license agreements
```
yes | ./sdkmanager.bat --licenses
```
4. If you plan to test on a physical device via USB, install the appropriate [OEM USB drivers](https://developer.android.com/studio/run/oem-usb.html)
5. Install [Java SE Development Kit](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)

# Keyman Android Pro (KMAPro) Development
KMAPro can be built from a command line (preferred) or Android Studio.  Before proceeding to the steps below, make sure you can build the Keyman Engine for Android first, as it is a dependency for KMAPro.  That will ensure all the needed SDK files are downloaded.

## Compiling from Command line
1. Launch a command prompt
2. Change to one of these directories depending on what you want to compile:

    * For compiling KMEA and KMAPro, cd to the directory **keyman/android**

    * For compiling only KMAPro, cd to the directory **keyman/android/KMAPro**

3. `./build.sh`
4. APK will be found in `KMAPro/kMAPro/build/outputs/apk/kMAPro-*.apk`

## Compiling from Android Studio
1. Launch Android Studio and open the directory **keyman/android/KMAPro**
2. From the project view, *configure* anything that Gradle reports.
3. With **kMAPro** selected, click Build --> Make Module 'kMAPro'
4. Select Run --> Run 'kmAPro'
5. Create a new virtual device to match your target API version

# Using Keyman Engine for Android as a library

1. Copy `keyman-engine.aar` into `[Your project folder]/app/libs/` folder.
2. Open your project in Android Studio.
3. Open `build.gradle` (Module: app) in "Gradle Scripts".
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
