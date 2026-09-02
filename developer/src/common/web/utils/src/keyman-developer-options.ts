/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * User options for Keyman Developer. These are stored in options.json in the
 * user profile; the location varies by operating system or may be stored in
 * browser storage on web sites.
 *
 * The node-based loader is implemented in both kmc and Keyman Developer Server,
 * in order to keep node dependencies out of the developer-utils module.
 */

/**
 * The standard path under the user profile where options.json is stored; use
 * `path.join(os.homedir(), ...KeymanDeveloperOptionsPath)` or similar
 */
export const KeymanDeveloperOptionsPath = [/* '~', */ '.keymandeveloper', 'options.json'];

/**
 * The set of standard user options for Keyman Developer. Corresponds to
 * TKeymanDeveloperOptions in `developer/src/tike/main/KeymanDeveloperOptions.pas`
 */
export interface KeymanDeveloperOptions {
  "use tab char": boolean;
  "link font sizes": boolean;
  "indent size": number;
  "use old debugger": boolean;
  "editor theme": string;
  "debugger break when exiting line": boolean;
  "debugger single step after break": boolean;
  "debugger show store offset": boolean;
  "debugger recompile with debug info": boolean;
  "debugger auto reset before compilng": boolean;
  "auto save before compiling": boolean;
  "osk auto save before importing": boolean;
  "web host port": number;
  "server keep alive": boolean;
  "server use local addresses": boolean;
  "server ngrok token": string;
  "server ngrok region": string;
  "server use ngrok": boolean;
  "server show console window": boolean;
  "char map disable database lookups": boolean;
  "char map auto lookup": boolean;
  "open keyboard files in source view": boolean;
  "display theme": string;
  "external editor path": string;
  "smtp server": string;
  "test email addresses": string;
  "web ladder length": number;
  "default project path": string;
  "automatically report errors": boolean;
  "automatically report usage": boolean;
  "toolbar visible": boolean;
  "active project": string;
  "prompt to upgrade projects": boolean;
};

/**
 * A single Keyman Developer user option.
 */
export type KeymanDeveloperOption = keyof KeymanDeveloperOptions;

const DEFAULT_OPTIONS: KeymanDeveloperOptions = {
  // Corresponds to TKeymanDeveloperOptions.Read in KeymanDeveloperOptions.pas
  "use tab char": false,
  "link font sizes": true,
  "indent size": 4,
  "use old debugger": false,
  "editor theme": '',
  "debugger break when exiting line": true,
  "debugger single step after break": false,
  "debugger show store offset": false,
  "debugger recompile with debug info": false,
  "debugger auto reset before compilng": false,
  "auto save before compiling": false,
  "osk auto save before importing": false,
  "web host port": 8008,
  "server keep alive": false,
  "server use local addresses": true,
  "server ngrok token": '',
  "server ngrok region": '',
  "server use ngrok": false,
  "server show console window": false,
  "char map disable database lookups": false,
  "char map auto lookup": true,
  "open keyboard files in source view": false,
  "display theme": 'Windows10',
  "external editor path": '',
  "smtp server": '',
  "test email addresses": '',
  "web ladder length": 100,
  "default project path": '', // Note: this diverges from Delphi code, which uses CSIDL_PERSONAL on Windows, but it is not used in Server
  "automatically report errors": true,
  "automatically report usage": true,
  "toolbar visible": true,
  "active project": '',
  "prompt to upgrade projects": true,
}


class KeymanDeveloperOptionsManager {
  private options: KeymanDeveloperOptions = {...DEFAULT_OPTIONS};
  constructor() {}

  public load(blob: Uint8Array | null) {
    this.options = {...DEFAULT_OPTIONS};
    if(blob !== null && blob !== undefined) {
      const data = JSON.parse(new TextDecoder('utf-8').decode(blob));
      if(typeof data == 'object') {
        // TODO: verify fields in options
        this.options = {...DEFAULT_OPTIONS, ...data};
        return true;
      }
    }
    return false;
  }

  public get<T extends KeymanDeveloperOption>(valueName: T): KeymanDeveloperOptions[T] {
    return this.options[valueName];
  }

  public clear() {
    this.options = {...DEFAULT_OPTIONS};
  }
}

export const optionsManager = new KeymanDeveloperOptionsManager();

