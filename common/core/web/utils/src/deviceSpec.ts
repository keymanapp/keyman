namespace com.keyman.utils {
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

  /**
   * This class provides an abstract version of com.keyman.Device that is core-friendly, 
   * containing only the information needed by web-core for text processing use, devoid
   * of any direct references to the DOM.
   */
  export class DeviceSpec {
    readonly browser: Browser;
    readonly formFactor: FormFactor;
    readonly OS: OperatingSystem;
    readonly touchable: boolean;

    constructor(browser: string, formFactor: string, OS: string, touchable: boolean) {
      switch(browser.toLowerCase() as Browser) {
        case Browser.Chrome:
        case Browser.Edge:
        case Browser.Firefox:
        case Browser.Native:
        case Browser.Opera:
        case Browser.Safari:
          this.browser = browser.toLowerCase() as Browser;
          break;
        default:
          this.browser = Browser.Other;
      }

      switch(formFactor.toLowerCase() as FormFactor) {
        case FormFactor.Desktop:
        case FormFactor.Phone:
        case FormFactor.Tablet:
          this.formFactor = formFactor.toLowerCase() as FormFactor;
          break;
        default:
          throw ("Invalid form factor specified for device: " + formFactor);
      }

      switch(OS.toLowerCase() as OperatingSystem) {
        case OperatingSystem.Windows.toLowerCase():
        case OperatingSystem.macOS.toLowerCase():
        case OperatingSystem.Linux.toLowerCase():
        case OperatingSystem.Android.toLowerCase():
        case OperatingSystem.iOS.toLowerCase():
          this.OS = OS.toLowerCase() as OperatingSystem;
          break;
        default:
          this.OS = OperatingSystem.Other;
      }
      
      this.touchable = touchable;
    }
  }
}