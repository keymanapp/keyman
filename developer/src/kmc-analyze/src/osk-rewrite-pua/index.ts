import { CompilerCallbacks, KeymanFileTypes, KvksFile, KvksFileReader, KvksFileWriter, TouchLayout, TouchLayoutFileReader, TouchLayoutFileWriter } from "@keymanapp/common-types";
import { CompilerMessages } from '@keymanapp/kmc-kmn';
import { getOskFromKmnFile } from "../util/get-osk-from-kmn-file.js";
import { AnalyzerMessages } from "../messages.js";
import { StringResult } from "../osk-character-use/index.js";

type PuaMap = {[index:string]: string};

export interface AnalyzeOskRewritePuaOptions {
  stripDottedCircle: boolean;
  mappingFile: string;
}

const defaultOptions: AnalyzeOskRewritePuaOptions = {
  stripDottedCircle: false,
  mappingFile: undefined,
}

export class AnalyzeOskRewritePua {
  private options: AnalyzeOskRewritePuaOptions;
  private _data: {[index:string]: Uint8Array} = {};

  constructor(private callbacks: CompilerCallbacks, options?: AnalyzeOskRewritePuaOptions) {
    this.options = {...defaultOptions, ...options};
  }

  public clear() {
    this._data = {};
  }

  //
  // Analyze a single file
  //

  public async analyze(file: string, mapping: StringResult[]): Promise<boolean> {
    let map: PuaMap = {};
    for(let item of mapping) {
      map[item.str] = String.fromCharCode(parseInt(item.pua,16));
    }

    switch(KeymanFileTypes.sourceTypeFromFilename(file)) {
      case KeymanFileTypes.Source.VisualKeyboard:
        this._data[file] = this.rewriteVisualKeyboard(file, map);
        break;
      case KeymanFileTypes.Source.TouchLayout:
        this._data[file] = this.rewriteTouchLayout(file, map);
        break;
      case KeymanFileTypes.Source.Project:
        throw new Error('Passing a project to analyze is not permitted');
        break;
      case KeymanFileTypes.Source.KeymanKeyboard:
        // The cleanest way to do this is to compile the .kmn to find the .kvks
        // and .keyman-touch-layout from the &VISUALKEYBOARD and &LAYOUTFILE
        // system stores
        if(!await this.analyzeKmnKeyboard(file, map)) {
          return false;
        }
        break;
      case KeymanFileTypes.Source.LdmlKeyboard:
        // TODO: await this.analyzeLdmlKeyboard(file);
        // note, will need to skip .xml files that are not ldml keyboards
        break;
      default:
        // Ignore any other file types; this way we can analyze project files/folders
        // easily also
    }
    return true;
  }

  public get data() {
    return this._data;
  }

  private async analyzeKmnKeyboard(filename: string, map: PuaMap): Promise<boolean> {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'keyboard source', name:filename}));

    let osk = await getOskFromKmnFile(this.callbacks, filename);

    if(osk.kvksFilename) {
      this._data[osk.kvksFilename] = this.rewriteVisualKeyboard(osk.kvksFilename, map);
    }
    if(osk.touchLayoutFilename) {
      this._data[osk.touchLayoutFilename] = this.rewriteTouchLayout(osk.touchLayoutFilename, map);
    }

    return true;
  }

  private cleanString(s: string): string {
    if(this.options.stripDottedCircle) {
      s = s.replace(/\u25cc/g, '');
    }
    return s.trim();
  }

  //
  // On Screen Keyboard file scanning
  //

  private rewriteVisualKeyboard(filename: string, map: PuaMap): Uint8Array {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'visual keyboard', name:filename}));
    const reader = new KvksFileReader();
    let source: KvksFile.default;
    try {
      source = reader.read(this.callbacks.loadFile(filename));
    } catch(e) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidKvksFile({filename, e}));
      return null;
    }
    let invalidKeys: string[] = [];
    const vk = reader.transform(source, invalidKeys);
    if(!vk) {
      this.callbacks.reportMessage(CompilerMessages.Error_InvalidKvksFile({filename, e:null}));
      return null;
    }
    let dirty = false;
    for(let key of vk.keys) {
      if(!key.text) {
        continue;
      }
      let text = this.cleanString(key.text);
      if(map[text]) {
        key.text = map[text];
        dirty = true;
      }
    }
    if(!dirty) {
      return null;
    }
    console.dir(vk);
    const writer = new KvksFileWriter();
    const data = writer.write(vk);
    // TODO: the encoder probably needs to go into KvksFileWRiter
    const enc = new TextEncoder();
    return enc.encode(data);
  }

  private rewriteTouchLayout(filename: string, map: PuaMap): Uint8Array {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'touch layout', name:filename}));
    let dirty = false;
    const reader = new TouchLayoutFileReader();
    const source = reader.read(this.callbacks.loadFile(filename));

    const scanKey = (key: TouchLayout.TouchLayoutKey | TouchLayout.TouchLayoutSubKey) => {
      if(!key.text) {
        return;
      }
      if(key.text.length > 2 && key.text[0] == '*' && key.text[key.text.length-1] == '*') {
        // Don't touch '*special*' key captions
        return;
      }
      let text = this.cleanString(key.text);
      if(map[text]) {
        key.text = map[text];
        dirty = true;
      }
    }

    const scanPlatform = (platform: TouchLayout.TouchLayoutPlatform) => {
      if(!platform) {
        return;
      }
      for(let layer of platform.layer) {
        for(let row of layer.row) {
          for(let key of row.key) {
            scanKey(key);
            let f: keyof TouchLayout.TouchLayoutFlick;
            for(f in key.flick ?? {}) {
              scanKey(key.flick[f]);
            }
            for(let sk of key.sk ?? []) {
              scanKey(sk);
            }
            for(let mt of key.multitap ?? []) {
              scanKey(mt);
            }
          }
        }
      }
    }
    scanPlatform(source.desktop);
    scanPlatform(source.phone);
    scanPlatform(source.tablet);

    if(!dirty) {
      return null;
    }
    const writer = new TouchLayoutFileWriter({formatted: true});
    const data = writer.write(source);
    return data;
  }
}