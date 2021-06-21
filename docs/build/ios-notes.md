# Keyman for iOS configuration notes

## Prerequisites
* Xcode 11
* iOS 9+
* SwiftLint (`brew install swiftlint`)
* Carthage 0.37+ (`brew install carthage`)
* Pandoc (`brew install pandoc`)
  - Used solely to generate offline help.
* Coreutils (`brew install coreutils`)
* sentry-cli (`brew install getsentry/tools/sentry-cli`) to utilize Sentry-based error reporting
* jq (`brew install jq`)
* `bash` (`brew install bash`)
* Building Keyman Web is a precursor for compiling KMEI, so verify your system has all the [Minimum Web Compilation Requirements](../web/README.md#minimum-web-compilation-requirements), including (but not limited to):
  - [Node.js](https://nodejs.org/) 8.9+ (for building the embedded KeymanWeb engine)
  - [Java 7+](https://adoptopenjdk.net/releases.html)

Note that the `brew` command mentioned above is Homebrew, which may be found at https://brew.sh/.
While not strictly necessary, it certainly simplifies installing the prerequisites above.
