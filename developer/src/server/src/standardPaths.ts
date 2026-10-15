/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Path and URL constants (in some cases calculated)
 */
import { mkdirSync } from 'node:fs';

class StandardPaths {
  public readonly appDataPath: string;
  public readonly cachePath: string;
  public readonly cacheStateFilename: string;
  public readonly lockFilename: string;
  public readonly pidFilename: string;

  /* ngrok Configuration */

  public ngrokEndpoint: string = '';

  constructor() {

    this.appDataPath = (process.env.APPDATA ||
      (process.platform == 'darwin' ? process.env.HOME + '/Library/Preferences' : process.env.HOME + "/.local/share")) +
      '/Keyman/Keyman Developer/Server/';
    this.cachePath = this.appDataPath + 'cache/';
    this.cacheStateFilename = this.appDataPath + 'cache.json';
    this.lockFilename = this.appDataPath + 'lock.json';
    this.pidFilename = this.appDataPath + 'pid.json';

    mkdirSync(this.cachePath, {recursive: true});
  }
};

export const standardPaths = new StandardPaths();
