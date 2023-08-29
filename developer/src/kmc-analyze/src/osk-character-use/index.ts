import { CompilerCallbacks, KeymanFileTypes, KvksFile, KvksFileReader, Osk, TouchLayout, TouchLayoutFileReader } from "@keymanapp/common-types";
import { CompilerMessages } from '@keymanapp/kmc-kmn';
import { getOskFromKmnFile } from "../util/get-osk-from-kmn-file.js";
import { AnalyzerMessages } from "../messages.js";


type StringRefUsageMap = {[index:string]: Osk.StringRefUsage[]};

export interface AnalyzeOskCharacterUseOptions {
  puaBase?: number;
  stripDottedCircle?: boolean;
  includeCounts?: boolean;
}

const defaultOptions: AnalyzeOskCharacterUseOptions = {
  puaBase: 0xF100,
  stripDottedCircle: false,
  includeCounts: false,
}

export class AnalyzeOskCharacterUse {
  private _strings: StringRefUsageMap = {};
  private options: AnalyzeOskCharacterUseOptions;

  constructor(private callbacks: CompilerCallbacks, options?: AnalyzeOskCharacterUseOptions) {
    this.options = {...defaultOptions, ...options};
  }

  public clear() {
    this._strings = {};
  }

  //
  // Analyze a single file
  //

  public async analyze(file: string): Promise<boolean> {
    switch(KeymanFileTypes.sourceTypeFromFilename(file)) {
      case KeymanFileTypes.Source.VisualKeyboard: {
        let strings = this.scanVisualKeyboard(file);
        if(!strings) {
          return false;
        }
        this.addStrings(strings, file);
        break;
      }
      case KeymanFileTypes.Source.TouchLayout: {
        let strings = this.scanTouchLayout(file);
        if(!strings) {
          return false;
        }
        this.addStrings(strings, file);
        break;
      }
      case KeymanFileTypes.Source.Project:
        throw new Error('Passing a project to analyze is not permitted');
        break;
      case KeymanFileTypes.Source.KeymanKeyboard:
        // The cleanest way to do this is to compile the .kmn to find the .kvks
        // and .keyman-touch-layout from the &VISUALKEYBOARD and &LAYOUTFILE
        // system stores
        if(!await this.analyzeKmnKeyboard(file)) {
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

  private async analyzeKmnKeyboard(filename: string): Promise<boolean> {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'keyboard source', name:filename}));

    let osk = await getOskFromKmnFile(this.callbacks, filename);

    if(osk.kvksFilename) {
      let strings = this.scanVisualKeyboard(osk.kvksFilename);
      if(!strings) {
        return false;
      }
      this.addStrings(strings, osk.kvksFilename);
    }
    if(osk.touchLayoutFilename) {
      let strings = this.scanTouchLayout(osk.touchLayoutFilename);
      if(!strings) {
        return false;
      }
      this.addStrings(strings, osk.touchLayoutFilename);
    }

    return true;
  }

  private addStrings(strings: string[], filename: string) {
    // Reduce all references to get usage count per file
    let reducedStrings: Osk.StringRef[] = [...new Set(strings)].map(e =>({
      str:e,
      usages: [{filename: this.callbacks.path.basename(filename), count: strings.filter(n => n===e).length }]
    }));

    // Merge result with existing found strings
    for(let e of reducedStrings) {
      this._strings[e.str] = [].concat(this._strings[e.str] ?? [], ...e.usages);
    }
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

  private scanVisualKeyboard(filename: string): string[] {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'visual keyboard', name:filename}));
    let strings: string[] = [];
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
    for(let key of vk.keys) {
      if(key.text) {
        strings.push(this.cleanString(key.text));
      }
    }
    return strings;
  }

  private scanTouchLayout(filename: string): string[] {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'touch layout', name:filename}));
    let strings: string[] = [];
    const reader = new TouchLayoutFileReader();
    const source = reader.read(this.callbacks.loadFile(filename));

    const scanKey = (key: TouchLayout.TouchLayoutKey | TouchLayout.TouchLayoutSubKey) => {
      if(!key.text) {
        return;
      }
      if(key.text.length > 2 && key.text[0] == '*' && key.text[key.text.length-1] == '*') {
        // Don't add '*special*' key captions
        return;
      }
      strings.push(this.cleanString(key.text));
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
    return strings;
  }

  //
  // Results reporting
  //

  private prepareResults(strings: StringRefUsageMap): Osk.StringResult[] {
    let result: Osk.StringResult[] = [];
    let pua = this.options.puaBase;
    for(let str of Object.keys(strings)) {
      result.push({
        pua: pua.toString(16).toUpperCase(),
        str,
        unicode: AnalyzeOskCharacterUse.stringToUnicodeSequence(str, false),
        usages: this.options.includeCounts ? strings[str] : strings[str].map(item => item.filename)
      });
      pua++;
    }
    return result;
  }

  public getStrings(format?: '.txt'|'.md'|'.json'): string[] {
    const final = this.prepareResults(this._strings);
    switch(format) {
      case '.md':
        return AnalyzeOskCharacterUse.getStringsAsMarkdown(final);
      case '.json':
        return AnalyzeOskCharacterUse.getStringsAsJson(final);
      }
    return AnalyzeOskCharacterUse.getStringsAsText(final);
  }

  // Following functions are static so that we can keep them pure
  // and potentially refactor into separate reporting class later

  private static getStringsAsText(strings: Osk.StringResult[]) {
    // Text result only returns PUA, unicode sequence, and plain string
    let lines: string[] = [];
    for(let s of strings) {
      const ux = this.stringToUnicodeSequence(s.str);
      lines.push('U+'+s.pua + '\t' + ux + '\t' + s.str);
    }
    return lines;
  }

  private static getStringsAsMarkdown(strings: Osk.StringResult[]) {
    // Markdown result only returns PUA, unicode sequence, and plain string
    let lines: string[] = [];
    lines.push('PUA    | Code Points | Key Caps');
    lines.push('-------|-------------|---------');
    for(let s of strings) {
      const ux = this.stringToUnicodeSequence(s.str);
      lines.push('U+'+s.pua + ' | ' + ux + ' | ' + this.escapeMarkdownChar(s.str));
    }
    return lines;
  }

  private static getStringsAsJson(strings: Osk.StringResult[]) {
    // For future expansion, we wrap the array in a 'map' property
    let map = { "map": strings };
    return JSON.stringify(map, null, 2).split('\n');
  }

  private static escapeMarkdownChar(s: string) {
    // note: could replace with a common lib but too much baggage to be worth it for now
    // commonmark 2.4: all punct can be escaped
    // const punct = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~';
    s = s.replace(/[!"#$%&'()*+,-./:;<=>?@\[\\\]^_`{|}~]/g, '\\$0');
    // replace whitepsace
    s = s.replace(/[\n]/g, '\\n');
    s = s.replace(/[\r]/g, '\\r');
    s = s.replace(/[\t]/g, '\\t');
    s = s.replace(/ /g, '&#x20;');
    s = s.replace(/\u00a0/g, '&#xa0;');
    return s;
  }

  private static stringToUnicodeSequence(s: string, addUPlusPrefix: boolean = true): string {
    let result = [];
    for(let ch of s) {
      let c = ch.codePointAt(0).toString(16).toUpperCase();
      if(c.length < 4) c = '0'.repeat(4 - c.length) + c;
      result.push((addUPlusPrefix ? 'U+' : '') + c);
    }
    return result.join(' ');
  }

}