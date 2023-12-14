import { writeFileSync } from 'fs';
import { configuration } from './config.js';
import { loadJsonFile } from './load-json-file.js';

export interface DebugObject {
  id: string;
  filename: string;
  sha256: string;
  lastUse: Date;
  filenameFromId(id: string): string;
};

export class DebugKeyboard implements DebugObject {
  id: string;
  filename: string;
  sha256: string;
  lastUse: Date;
  path: string;
  version: string;
  oskFontFace?: string;
  fontFace?: string;

  filenameFromId(id: string): string {
    return id + '.js';
  }

  toRegistrationBlob() {
    let o = {
      KN: this.id,
      KI: 'Keyboard_'+this.id,
      KL: this.id,
      KLC: 'en',
      KR: 'Europe',
      KRC: 'eu',
      KFont: this.fontFace ? { family: this.fontFace } : undefined,
      KOskFont: this.oskFontFace ? { family: this.oskFontFace } : undefined,
      KF: this.id+'.js'
    };

    return JSON.stringify(o, null, 2) + '/*'+this.sha256+'*/';
  };
};

export class DebugModel implements DebugObject {
  id: string;
  filename: string;
  sha256: string;
  lastUse: Date;

  filenameFromId(id: string): string {
    return id + '.model.js';
  }

  toRegistrationBlob() {
    return JSON.stringify(this.id) + ', ' + JSON.stringify(this.id + '.model.js') + '/*'+this.sha256+'*/';
  }
};

export class DebugPackage implements DebugObject {
  id: string;
  filename: string;
  sha256: string;
  lastUse: Date;
  name: string;

  filenameFromId(id: string): string {
    return id + '.kmp';
  }
};

export class DebugFont implements DebugObject {
  id: string;
  filename: string;
  sha256: string;
  lastUse: Date;
  facename: string;

  filenameFromId(id: string): string {
    return simplifyId(id) + '.ttf';
  }
};

export class SiteData {
  keyboards: { [ id: string ]: DebugKeyboard } = {};
  models: { [ id: string ]: DebugModel } = {};
  packages: { [ id: string ]: DebugPackage } = {};
  fonts: { [ id: string ]: DebugFont } = {};

  constructor() {
    this.loadState();
  }

  private loadDebugObject(type: any, source: any, dest: any) {
    for(let id in source) {
      dest[id] = Object.assign(new type(), source[id]);
    }
  }

  private loadState() {
    const state = loadJsonFile(configuration.cacheStateFilename);
    this.loadDebugObject(DebugKeyboard, state?.keyboards, this.keyboards);
    this.loadDebugObject(DebugModel, state?.models, this.models);
    this.loadDebugObject(DebugFont, state?.fonts, this.fonts);
    this.loadDebugObject(DebugPackage, state?.packages, this.packages);
    // todo cleanup of missing files
  }

  public saveState() {
    writeFileSync(configuration.cacheStateFilename, JSON.stringify(this, null, 2), 'utf-8');
  }
};

export let data: SiteData = new SiteData();

export function isValidId(id: string) {
  return !id.match(/\.\.|\/|\\/);
}

export function simplifyId(id: string) {
  return id.replace(/[^_a-zA-Z0-9-]/g, '_');
}

