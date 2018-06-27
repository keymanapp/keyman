![Keyman](https://keyman.com/cdn/dev/img/logo2.png) 

Open Source

[comment]: # (CI Build statuses)

| Platform  | Master | Nightly | Beta | Stable |
| --------  | :----: | :-----: | :--: | :----: |
| Windows   | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_Build)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanDesktop_Nightly)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanDesktop_Beta)/statusIcon)| ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanDesktop_Stable),branch:(default:false)/statusIcon) |
| macOS    | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanMac_Master)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanMac_Nightly)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanMac_Beta)/statusIcon)| ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanMac_Stable),branch:(default:false)/statusIcon) |
| Web       | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keymanweb_Build)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keymanweb_Nightly)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keymanweb_Beta)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keymanweb_Stable),branch:(default:false)/statusIcon)|
| Android   | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanAndroid_Build)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanAndroid_NightlyAlpha)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanAndroid_Beta)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanAndroid_Stable),branch:(default:false)/statusIcon) |
| iOS   | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_iOS_Master)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_iOS_Nightly)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_iOS_Beta)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:Keyman_iOS_Stable),branch:(default:false)/statusIcon) |
| Linux   | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanLinux_Master)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanLinux_Nightly)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanLinux_Beta)/statusIcon) | ![Build Status](https://build.palaso.org/app/rest/builds/buildType:(id:KeymanLinux_Stable),branch:(default:false)/statusIcon) |

[Keyman](https://keyman.com) makes it possible for you to type in over 1,000 languages on Windows, iPhone, iPad, Android tablets and phones, and even instantly in your web browser.

# Getting Started

## Desktop Platforms ##
[Windows](./windows/src/README.md)
[macOS](./mac/README.md)
[Linux](./linux/README.md)

## Web & Mobile Platforms ##
[Web](./web/README.md)
[Android](./android/README.md)
[iOS](./ios/README.md)

# Release Types

Each platform maintains multiple types of releases:

* Stable: A released version.
* Beta: A version that is nearing full "stable" release.
* Nightly: Contains the very latest code. These versions are published daily whenever the code changes.

# License

Copyright (c) 2018 SIL International. All rights reserved.

Licensed under the [MIT](./windows/src/LICENSE.md) License.

[Keyman for Linux](./linux) is licensed under the [MIT](./windows/src/LICENSE.md) License apart from [ibus-kmfl](./linux/ibus-kmfl) which is licensed under the GNU General Public License as published by the Free Software Foundation; either [version 2](./linux/ibus-kmfl/COPYING) of the License, or (at your option) any later version.

