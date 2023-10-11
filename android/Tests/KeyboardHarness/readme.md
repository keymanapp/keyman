# KeyboardHarness Test App #

This app is is based on Samples/KMSample1 and targets keyboard functionality and reproducing bugs with keyboards rather than integration functionality. Use as system keyboard is currently not supported.

### keyboardharness package source
The path is ./app/src/main/assets/keyboardharness.kps

and consists of these engineering keyboards:
* android/Tests/KeyboardHarness/app/src/main/assets/longpress.js
* web/testing/chirality/chirality.js
* web/testing/platform/platformtest.js

test9469.kmp is another engineering keyboard to show non-printing characters on the OSK.

### Compiling From Command Line
1. Launch a command prompt to the `android/` folder
2. Compile KMEA. This will build and copy `keyman-engine.aar` to the Samples and Test projects
    ```
    cd KMEA
    ./build.sh
    ```
3. Compile KeyboardHarness
    ```
    cd ../Tests/KeyboardHarness
    ./build.sh
    ```

## Version History ##

## 2017-09-26 1.0
* Initial creation

