import { mkdirSync } from 'fs';
import { loadJsonFile } from './load-json-file.js';

export class Configuration {
  public readonly appDataPath: string;
  public readonly cachePath: string;
  public readonly cacheStateFilename: string;
  public readonly lockFilename: string;
  public readonly pidFilename: string;
  public readonly configFilename: string;
  public readonly ngrokBinPath: string;

  /* Configuration values - set in config.json by TIKE */

  public readonly port: number;

  /* ngrok Configuration */

  public readonly useNgrok: boolean;
  public readonly ngrokControlPort: number;
  public readonly ngrokToken: string;
  public readonly ngrokVisible: boolean;

  public ngrokEndpoint: string = '';

  constructor() {

    this.appDataPath = (process.env.APPDATA ||
      (process.platform == 'darwin' ? process.env.HOME + '/Library/Preferences' : process.env.HOME + "/.local/share")) +
      '/Keyman/Keyman Developer/Server/';
    this.cachePath = this.appDataPath + 'cache/';
    this.cacheStateFilename = this.appDataPath + 'cache.json';
    this.lockFilename = this.appDataPath + 'lock.json';
    this.pidFilename = this.appDataPath + 'pid.json';
    this.configFilename = this.appDataPath + 'config.json';

    mkdirSync(this.cachePath, { recursive: true});

    const cfg = loadJsonFile(this.configFilename);

    this.port = cfg?.port ?? 8008;

    // ngrok configuration
    this.useNgrok = cfg?.useNgrok ?? false;
    this.ngrokBinPath = this.appDataPath + 'bin/';
    this.ngrokControlPort = cfg?.ngrokControlPort ?? 8009;
    this.ngrokToken = cfg?.ngrokToken ?? '';
    this.ngrokVisible = cfg?.ngrokVisible ?? false;
  }
};

export const configuration = new Configuration();