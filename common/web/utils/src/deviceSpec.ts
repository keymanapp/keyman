/**
 * This class provides an abstract version of com.keyman.Device that is core-friendly,
 * containing only the information needed by web-core for text processing use, devoid
 * of any direct references to the DOM.
 */
export class DeviceSpec {
  readonly browser: DeviceSpec.Browser;
  readonly formFactor: DeviceSpec.FormFactor;
  readonly OS: DeviceSpec.OperatingSystem;
  readonly touchable: boolean;

  constructor(browser: string, formFactor: string, OS: string, touchable: boolean) {
    switch(browser.toLowerCase() as DeviceSpec.Browser) {
      case DeviceSpec.Browser.Chrome:
      case DeviceSpec.Browser.Edge:
      case DeviceSpec.Browser.Firefox:
      case DeviceSpec.Browser.Native:
      case DeviceSpec.Browser.Opera:
      case DeviceSpec.Browser.Safari:
        this.browser = browser.toLowerCase() as DeviceSpec.Browser;
        break;
      default:
        this.browser = DeviceSpec.Browser.Other;
    }

    switch(formFactor.toLowerCase() as DeviceSpec.FormFactor) {
      case DeviceSpec.FormFactor.Desktop:
      case DeviceSpec.FormFactor.Phone:
      case DeviceSpec.FormFactor.Tablet:
        this.formFactor = formFactor.toLowerCase() as DeviceSpec.FormFactor;
        break;
      default:
        throw ("Invalid form factor specified for device: " + formFactor);
    }

    switch(OS.toLowerCase() as DeviceSpec.OperatingSystem) {
      case DeviceSpec.OperatingSystem.Windows.toLowerCase():
      case DeviceSpec.OperatingSystem.macOS.toLowerCase():
      case DeviceSpec.OperatingSystem.Linux.toLowerCase():
      case DeviceSpec.OperatingSystem.Android.toLowerCase():
      case DeviceSpec.OperatingSystem.iOS.toLowerCase():
        this.OS = OS.toLowerCase() as DeviceSpec.OperatingSystem;
        break;
      default:
        this.OS = DeviceSpec.OperatingSystem.Other;
    }

    this.touchable = touchable;
  }
}

// Namespaces these under DeviceSpec, as each is primarily used with it.
export namespace DeviceSpec {
  export enum Browser {
    Chrome = 'chrome',
    Edge = 'edge',
    Firefox = 'firefox',
    Native = 'native', // Used by embedded mode
    Opera = 'opera',
    Safari = 'safari',
    Other = 'other'
  }

  export enum OperatingSystem {
    Windows = 'windows',
    macOS = 'macosx',
    Linux = 'linux',
    Android = 'android',
    iOS = 'ios',
    Other = 'other'
  }

  export enum FormFactor {
    Desktop = 'desktop',
    Phone = 'phone',
    Tablet = 'tablet'
  }
}

export function physicalKeyDeviceAlias(device: DeviceSpec) {
  return new DeviceSpec(device.browser, DeviceSpec.FormFactor.Desktop, device.OS, false);
}

export default DeviceSpec;