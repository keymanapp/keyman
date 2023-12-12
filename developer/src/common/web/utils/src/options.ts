import * as os from 'os';
import * as fs from 'fs';
import * as path from 'path';

export interface KeymanDeveloperOptions {
  "use tab char"?: boolean;
  "link font sizes"?: boolean;
  "indent size"?: number;
  "use old debugger"?: boolean;
  "editor theme"?: string;
  "debugger break when exiting line"?: boolean;
  "debugger single step after break"?: boolean;
  "debugger show store offset"?: boolean;
  "debugger recompile with debug info"?: boolean;
  "debugger auto reset before compilng"?: boolean;
  "auto save before compiling"?: boolean;
  "osk auto save before importing"?: boolean;
  "web host port"?: number;
  "server keep alive"?: boolean;
  "server use local addresses"?: boolean;
  "server ngrok token"?: string;
  "server ngrok region"?: string;
  "server use ngrok"?: boolean;
  "server show console window"?: boolean;
  "char map disable database lookups"?: boolean;
  "char map auto lookup"?: boolean;
  "open keyboard files in source view"?: boolean;
  "display theme"?: string;
  "external editor path"?: string;
  "smtp server"?: string;
  "test email addresses"?: string;
  "web ladder length"?: number;
  "default project path"?: string;
  "automatically report errors"?: boolean;
  "automatically report usage"?: boolean;
  "toolbar visible"?: boolean;
  "active project"?: string;
  "prompt to upgrade projects"?: boolean;
};

type KeymanDeveloperOption = keyof KeymanDeveloperOptions;

// Default has no options set, and unit tests will use the defaults (won't call
// `loadOptions()`)
let options: KeymanDeveloperOptions = {};

// We only load the options from disk once on first use
let optionsLoaded = false;

export async function loadOptions(): Promise<KeymanDeveloperOptions> {
  if(optionsLoaded) {
    return options;
  }

  options = {};
  try {
    const optionsFile = path.join(os.homedir(), '.keymandeveloper', 'options.json');
    if(fs.existsSync(optionsFile)) {
      for(let i = 0; i < 5; i++) {
        try {
          const data = JSON.parse(fs.readFileSync(optionsFile, 'utf-8'));
          if(typeof data == 'object') {
            options = data;
          }
          break;
        } catch(e) {
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
    options = {};
  }
  optionsLoaded = true;
  return options;
}

export function getOption<T extends KeymanDeveloperOption>(valueName: T, defaultValue: KeymanDeveloperOptions[T]): KeymanDeveloperOptions[T] {
  return options[valueName] ?? defaultValue;
}

/**
 * unit tests will clear options before running, for consistency
 */
export function clearOptions() {
  options = {};
  optionsLoaded = true;
}