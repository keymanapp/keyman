namespace com.keyman.text {
  /**
   * Defines common behaviors associated with system stores.
   */
  export abstract class SystemStore {
    public readonly id: number;

    constructor(id: number) {
      this.id = id;
    }

    abstract matches(value: string): boolean;

    set(value: string): void {
      throw new Error("System store with ID " + this.id + " may not be directly set.");
    }
  }

  /**
   * A handler designed to receive feedback whenever a system store's value is changed.
   * @param source    The system store being mutated, before the value change occurs.
   * @param newValue  The new value being set
   * @returns         `false` / `undefined` to allow the change, `true` to block the change.
   */
  export type SystemStoreMutationHandler = (source: MutableSystemStore, newValue: string) => boolean;

  export class MutableSystemStore extends SystemStore {
    private _value: string;
    handler?: SystemStoreMutationHandler = null;

    constructor(id: number, defaultValue: string) {
      super(id);
      this._value = defaultValue;
    }

    get value() {
      return this._value;
    }

    matches(value: string) {
      return this._value == value;
    }

    set(value: string) {
      if(this.handler) {
        if(this.handler(this, value)) {
          return;
        }
      }

      this._value = value;
    }
  }

  /**
   * Handles checks against the current platform.
   */
  export class PlatformSystemStore extends SystemStore {
    private readonly kbdInterface: KeyboardInterface;

    constructor(keyboardInterface: KeyboardInterface) {
      super(KeyboardInterface.TSS_PLATFORM);

      this.kbdInterface = keyboardInterface;
    }

    matches(value: string) {
      var i,constraint,constraints=value.split(' ');
      let device = this.kbdInterface.activeDevice;

      for(i=0; i<constraints.length; i++) {
        constraint=constraints[i].toLowerCase();
        switch(constraint) {
          case 'touch':
          case 'hardware':
            if(device.touchable != (constraint == 'touch')) {
              return false;
            }
            break;

          case 'macos':
          case 'mac':
            constraint = 'macosx';
            // fall through
          case 'macosx':
          case 'windows':
          case 'android':
          case 'ios':
          case 'linux':
            if(device.OS != constraint) {
              return false;
            }
            break;

          case 'tablet':
          case 'phone':
          case 'desktop':
            if(device.formFactor != constraint) {
              return false;
            }
            break;

          case 'web':
            if(device.browser == 'native') {
              return false; // web matches anything other than 'native'
            }
            break;
            
          case 'native':
            // This will return true for embedded KeymanWeb
          case 'ie':
          case 'chrome':
          case 'firefox':
          case 'safari':
          case 'edge':
          case 'opera':
            if(device.browser != constraint) {
              return false;
            }
            break;
            
          default:
            return false;
        }
      }

      // Everything we checked against was valid and had matches - it's a match!
      return true;
    }
  }
}