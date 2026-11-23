// Keyman is copyright (C) SIL International. MIT License.

import { type MainModule as KMXCoreModule } from './import/core/keymancore.js';

// Unfortunately embind has an open issue with enums and typescript where it
// only generates a type for the enum, but not the values in a usable way.
// So we have to re-define the enum here.
// See https://github.com/emscripten-core/emscripten/issues/18585
// NOTE: Keep in sync with core/include/keyman/keyman_core_api.h#L311
export enum KM_CORE_STATUS {
  OK = 0,
  NO_MEM = 1,
  IO_ERROR = 2,
  INVALID_ARGUMENT = 3,
  KEY_ERROR = 4,
  INSUFFICENT_BUFFER = 5,
  INVALID_UTF = 6,
  INVALID_KEYBOARD = 7,
  NOT_IMPLEMENTED = 8,
  OS_ERROR = 0x80000000
}

export enum KM_CORE_OPTION_SCOPE {
  OPT_UNKNOWN = 0,
  OPT_KEYBOARD = 1,
  OPT_ENVIRONMENT = 2,
  OPT_MAX_SCOPES
}

export enum KM_CORE_KMX_ENV {
  PLATFORM = "platform",
  BASELAYOUT = "baseLayout",
  BASELAYOUTALT = "baseLayoutAlt",
  SIMULATEALTGR = "simulateAltgr",
  CAPSLOCK = "capsLock",
  BASELAYOUTGIVESCTRLRALTFORRALT = "baseLayoutGivesCtrlRAltForRAlt",
}

export enum KM_CORE_CT {
  END = 0,
  CHAR = 1,
  MARKER = 2,
}

export enum KM_CORE_EVENT_FLAG {
  DEFAULT = 0,
  TOUCH = 1,
};

export class KM_Core {
  private static km_core: KMXCoreModule = null;

  static get instance(): KMXCoreModule {
    return this.km_core;
  }

  private static isNode() {
    // from https://stackoverflow.com/a/31456668
    return (typeof process !== "undefined" && process?.versions?.node);
  }

  public static async createCoreProcessor(baseurl: string): Promise<KMXCoreModule> {
    const coreModuleName = this.isNode() ? 'km-core-node.mjs' : 'km-core.js';
    const module = await import(`${baseurl}/${coreModuleName}`);
    const createCoreProcessor = module.default;
    const km_core = createCoreProcessor({
      locateFile: function (path: string, scriptDirectory: string) {
        return baseurl + '/' + path;
      }
    });
    km_core.then((core: KMXCoreModule) => {
      this.km_core = core;
    });
    return km_core;
  }
}