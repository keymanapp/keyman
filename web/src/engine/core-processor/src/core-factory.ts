// Keyman is copyright (C) SIL International. MIT License.

import { type MainModule } from './import/core/keymancore.js';

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

export class CoreFactory {
  public static async createCoreProcessor(baseurl: string): Promise<MainModule> {
    try {
      const module = await import(baseurl + '/km-core.js');
      const createCoreProcessor = module.default;
      return await createCoreProcessor({
        locateFile: function (path: string, scriptDirectory: string) {
          return baseurl + '/' + path;
        }
      });
    } catch (e: any) {
      console.log('got execption in CoreFactory.createCoreProcessor', e);
      return null;
    }
  }
}