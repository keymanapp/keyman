/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Load Keyman Developer's options from the standard Options location. This
 * small loader is duplicated in Keyman Developer Server, because we do not have
 * a shared node-aware module at this time.
 */

import * as os from 'node:os';
import * as fs from 'node:fs';
import * as path from 'node:path';
import { KeymanDeveloperOption, KeymanDeveloperOptions, KeymanDeveloperOptionsPath, optionsManager } from '@keymanapp/developer-utils';

let optionsLoaded: boolean = false;

export async function loadOptions(): Promise<boolean> {
  if(optionsLoaded) {
    return true;
  }
  optionsLoaded = true;

  try {
    const optionsFile = path.join(os.homedir(), ...KeymanDeveloperOptionsPath);
    if(fs.existsSync(optionsFile)) {
      for(let i = 0; i < 5; i++) {
        try {
          const data = fs.readFileSync(optionsFile) as Uint8Array;
          return optionsManager.load(data);
        } catch(e: any) {
          if(e?.code == 'EBUSY') {
            await new Promise(resolve => setTimeout(resolve, 500));
          } else {
            // give up, nothing to report here
            break;
          }
        }
      }
    }
  } catch(e) {
    // Nothing to report here, sadly -- because we cannot rely on Sentry at this
    // low level.
  }

  optionsManager.clear();
  return false;
}

export function getOption<T extends KeymanDeveloperOption>(valueName: T): KeymanDeveloperOptions[T] {
  return optionsManager.get(valueName);
}

/**
 * unit tests will clear options before running, for consistency
 */
export function clearOptions() {
  optionsLoaded = false;
  return optionsManager.clear();
}