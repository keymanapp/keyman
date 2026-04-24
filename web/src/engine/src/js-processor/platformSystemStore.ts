/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * &platform system store implementation (.js keyboards)
 */


import { SystemStore, SystemStoreIDs, type KeyboardHarness } from 'keyman/engine/keyboard';

/**
 * Handles checks against the current platform.
 */
export class PlatformSystemStore extends SystemStore {
  private readonly kbdInterface: KeyboardHarness;

  constructor(keyboardInterface: KeyboardHarness) {
    super(SystemStoreIDs.TSS_PLATFORM);

    this.kbdInterface = keyboardInterface;
  }

  matches(value: string) {
    const constraints=value.split(' ');
    const device = this.kbdInterface.activeDevice;

    for(let i=0; i<constraints.length; i++) {
      let constraint=constraints[i].toLowerCase();
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
