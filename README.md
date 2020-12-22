![Keyman](https://keyman.com/cdn/dev/img/logo2.png)

[Keyman](https://keyman.com) makes it possible for you to type in any language on Windows, macOS, Linux, iPhone, iPad, Android tablets and phones, and even instantly in your web browser. Create keyboard layouts with Keyman Developer and share them with the community in the [keyboards repository](https://github.com/keymanapp/keyboards). The [Keyman Community](https://community.software.sil.org/c/keyman) have already contributed keyboard layouts for over 1,500 languages!

Keyman is an open source project distributed under the MIT license.

# Get Involved

[Get involved](https://keyman.com/about/get-involved)

[comment]: # (CI Build statuses)

Development Status on [status.keyman.com](https://status.keyman.com)

| Platform  | Alpha | Beta | Stable |
| --------  | :---: | :--: | :----: |
| Windows   | ![Alpha][win-master] | ![Beta][win-beta]| ![Stable][win-stable] |
| macOS     | ![Alpha][mac-master] | ![Beta][mac-beta]| ![Stable][mac-stable] |
| Web       | ![Alpha][web-master] | ![Beta][web-beta]| ![Stable][web-stable] |
| Android   | ![Alpha][android-master] | ![Beta][android-beta]| ![Stable][android-stable] |
| iOS       | ![Alpha][ios-master] | ![Beta][ios-beta]| ![Stable][ios-stable] |
| Linux     | ![Alpha][linux-master] | ![Beta][linux-beta]| ![Stable][linux-stable] |

# Getting Started

## Desktop Platforms ##
* [Windows](./windows/src/README.md)
* [macOS](./mac/README.md)
* [Linux](./linux/README.md)

## Web & Mobile Platforms ##
* [Web](./web/README.md)
* [Android](./android/README.md)
* [iOS](./ios/README.md)

# Release Types

Each platform maintains multiple types of releases:

* Stable: A released version. On a `stable` or `stable-<version>` branch.
* Beta: A version that is nearing full "stable" release. On the `beta` branch.
* Alpha: Contains the very latest code. These versions are published daily whenever the code changes. On `master` branch.

# License

Copyright (c) 2018-2021 SIL International. All rights reserved.

Licensed under the [MIT](./windows/src/LICENSE.md) License.

[Keyman for Linux](./linux) is licensed under the [MIT](./windows/src/LICENSE.md) License apart from [ibus-kmfl](./linux/ibus-kmfl) which is licensed under the GNU General Public License as published by the Free Software Foundation; either [version 2](./linux/ibus-kmfl/COPYING) of the License, or (at your option) any later version.

[comment]: # (CI Build status links)

  [win-master]:https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_Build)/statusIcon
  [win-beta]:https://build.palaso.org/app/rest/builds/buildType:(id:KeymanDesktop_Beta)/statusIcon
  [win-stable]:https://build.palaso.org/app/rest/builds/buildType:(id:KeymanDesktop_Stable),branch:(default:false)/statusIcon
  [mac-master]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanMac_Master)/statusIcon
  [mac-beta]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanMac_Beta)/statusIcon
  [mac-stable]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanMac_Stable),branch:(default:false)/statusIcon
  [web-master]: https://build.palaso.org/app/rest/builds/buildType:(id:Keymanweb_Build)/statusIcon
  [web-beta]: https://build.palaso.org/app/rest/builds/buildType:(id:Keymanweb_Beta)/statusIcon
  [web-stable]: https://build.palaso.org/app/rest/builds/buildType:(id:Keymanweb_Stable),branch:(default:false)/statusIcon
  [android-master]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanAndroid_Build)/statusIcon
  [android-beta]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanAndroid_Beta)/statusIcon
  [android-stable]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanAndroid_Stable),branch:(default:false)/statusIcon
  [ios-master]: https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_iOS_Master)/statusIcon
  [ios-beta]: https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_iOS_Beta)/statusIcon
  [ios-stable]: https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_iOS_Stable),branch:(default:false)/statusIcon
  [linux-master]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanLinux_Master)/statusIcon
  [linux-beta]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanLinux_Beta)/statusIcon
  [linux-stable]: https://build.palaso.org/app/rest/builds/buildType:(id:KeymanLinux_Stable),branch:(default:false)/statusIcon

# Notes for Contributors

When cloning this repo for local development on a Windows machine, take care not to place it overly deep in your file system.
Some of the paths for compilation can push character lengths around 160 characters long, while certain operations on Windows systems may be limited to paths of 260 characters or less.
For example, [`git clean` on Windows with msys](https://stackoverflow.com/questions/22575662/filename-too-long-in-git-for-windows/22575737#22575737) is limited due to dependence on older Windows APIs.