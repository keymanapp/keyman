# Keyman for Android configuration notes

WARNING: these are old configuration notes. See [index.md](../index.md) for current steps.

* Android Studio 4.1+
* Java SE Development Kit 8
* [Node.js](https://nodejs.org/) 14.17+ (for building KeymanWeb)
* [Pandoc](#Install-Pandoc) for generating offline help

## Install Java
It is recommended to use openJDK because of oracle license issues.
Tested with latest release for openJDK 8 from
https://github.com/ojdkbuild/ojdkbuild

1. Download and unpack the zip archive
2. On on windows: use the default java path C:\Program Files\Java to avoid error message "Error 0x80010135 Path Too Long".
3. Aso set an environment variable for JAVA_HOME e.g C:\Program Files\Java\openjdk-1.8.0.232-1

## Setup Android Studio

1. Download [Android Studio](https://developer.android.com/studio/index.html) and install with these [instructions](https://developer.android.com/studio/install.html).
2. For Windows users, set environment variable **ANDROID_HOME** to the location of your Android SDK. The default installation location is **C:\Users\\[USER]\AppData\Local\Android\sdk** where [USER] is your username. You may need to log out and log back in to take effect.

For MacOS/Linux users, add the following to **~/.bashrc** or **~/.bash_profile**
```bash
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools
```
For MacOS users, add the following (adjusted appropriately) to **~/.bashrc** or **~/.bash_profile** if your Java version is too strange for gradlew to understand (e.g., 11.0.2)
```bash
export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
echo $JAVA_HOME #should output: /Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/Contents/Home
```
3. For Windows users, from a Git Bash Prompt window, cd to the **sdk/tools/bin** folder and accept all the SDK license agreements
```
yes | ./sdkmanager.bat --licenses
```
For MacOS users, from a Terminal window, cd to the **~/Library/Android/sdk/tools/bin** folder and accept all the SDK license agreements
```bash
yes | ./sdkmanager --licenses
```
4. If you plan to test on a physical device via USB, install the appropriate [OEM USB drivers](https://developer.android.com/studio/run/oem-usb.html)
5. Install [Java SE Development Kit](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)

### Install JQ
jq 1.6+ is used during the build process to determine the latest versions of the default keyboard (sil_euro_latin.kmp) and lexical-model (en.nrc.mtnt.model.kmp) packages to download. For builds on Windows, jq is already included in `/resources/build/`

On Linux
`sudo apt install jq`

### Install Pandoc
Pandoc is used during the build process to generate the app's offline help.
Install from https://pandoc.org/installing.html
