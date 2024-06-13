import { CompilerCallbacks, KeymanFileTypes, TouchLayout, TouchLayoutFileReader } from "@keymanapp/common-types";
import { CompilerMessages, Osk } from '@keymanapp/kmc-kmn';
import { escapeMarkdownChar, KvksFile, KvksFileReader } from '@keymanapp/developer-utils';
import { getOskFromKmnFile } from "../util/get-osk-from-kmn-file.js";
import { AnalyzerMessages } from "../messages.js";


type StringRefUsageMap = {[index:string]: Osk.StringRefUsage[]};

/**
 * @public
 * Options for character analysis
 */
export interface AnalyzeOskCharacterUseOptions {
  /**
   * First character to use in PUA for remapping with &displayMap, defaults to
   * U+F100
   */
  puaBase?: number;
  /**
   * If true, strips U+25CC from the key cap before further analysis
   */
  stripDottedCircle?: boolean;
  /**
   * If true, reports number of references to each character found in each
   * source file
   */
  includeCounts?: boolean;
}

const defaultOptions: AnalyzeOskCharacterUseOptions = {
  puaBase: 0xF100,
  stripDottedCircle: false,
  includeCounts: false,
}

/**
 * @public
 * Analyze the characters used in On Screen Keyboard files (.kvks,
 * .keyman-touch-layout) for use with `&displayMap`.
 */
export class AnalyzeOskCharacterUse {
  private _strings: StringRefUsageMap = {};
  private options: AnalyzeOskCharacterUseOptions;

  constructor(private callbacks: CompilerCallbacks, options?: AnalyzeOskCharacterUseOptions) {
    this.options = {...defaultOptions, ...options};
  }

  /**
   * Clears analysis data collected from previous calls to
   * {@link AnalyzeOskCharacterUse.analyze}
   */
  public clear() {
    this._strings = {};
  }

  //
  // Analyze a single file
  //

  /**
   * Analyzes a single source file for Unicode character usage. Can parse .kmn,
   * .kvks, .keyman-touch-layout file formats. Can be called multiple times to
   * collect results from more than one file. Use
   * {@link AnalyzeOskCharacterUse.getStrings} to retrieve results.
   *
   * Note: `analyze()` collects key cap data, so calling this for a .kmn file
   * will retrieve the key caps from the .kvks and .keyman-touch-layout files
   * that it references, rather than key cap data from the .kmn file itself.
   *
   * @param   file - relative or absolute path to a Keyman source file
   * @returns        true if the file is successfully loaded and parsed
   */
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

  /**
   * Returns the collected results from earlier calls to
   * {@link AnalyzeOskCharacterUse.analyze}. This generates a mapping from a key
   * cap (one or more characters) to a PUA code, for use with `&displayMap`.
   *
   * Three output formats are supported:
   *
   * - .txt: tab-separated string format, with three columns: PUA, Key Cap, and
   *   plain string. The PUA and Key Cap columns are formatted as Unicode Scalar
   *   Values, e.g. U+0061, and the plain string is the original key cap string.
   *
   * - .md: formatted for documentation purposes. Generates a Markdown table
   *   (GFM) with PUA, Key Cap, and plain string. The PUA and Key Cap columns
   *   are formatted as Unicode Scalar Values, e.g. U+0061, and the plain string
   *   is the original key cap string.
   *
   * - .json: returns the final aggregated data as an array of strings, which
   *   can be joined to form a JSON blob of an object with a single member,
   *   `map`, which is an array of {@link Osk.StringResult} objects.
   *
   * @param    format - file format to return - can be '.txt', '.md', or '.json'
   * @returns  an array of strings, formatted according to the `format`
   *           parameter.
   */
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
      lines.push('U+'+s.pua + ' | ' + ux + ' | ' + escapeMarkdownChar(s.str, true));
    }
    return lines;
  }

  private static getStringsAsJson(strings: Osk.StringResult[]) {
    // For future expansion, we wrap the array in a 'map' property
    let map = { "map": strings };
    return JSON.stringify(map, null, 2).split('\n');
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