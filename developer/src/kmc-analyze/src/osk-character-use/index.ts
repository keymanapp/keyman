import { CompilerCallbacks, KeymanDeveloperProject, KeymanFileTypes, KMX, KmxFileReader, KPJFileReader, KvksFileReader, TouchLayout, TouchLayoutFileReader } from "@keymanapp/common-types";
import { KmnCompiler } from '@keymanapp/kmc-kmn';
import { AnalyzerMessages } from "../messages.js";

export class AnalyzeOskCharacterUse {
  private _strings: string[] = [];

  constructor(private callbacks: CompilerCallbacks) {
  }

  public clear() {
    this._strings = [];
  }

  //
  // Analyze a set of files
  //

  public async analyze(files: string[], analyzeProjects: boolean = true) {
    for(let file of files) {
      switch(KeymanFileTypes.sourceTypeFromFilename(file)) {
        case KeymanFileTypes.Source.VisualKeyboard:
          this.addStrings(this.scanVisualKeyboard(file));
          break;
        case KeymanFileTypes.Source.TouchLayout:
          this.addStrings(this.scanTouchLayout(file));
          break;
        case KeymanFileTypes.Source.Project:
          if(analyzeProjects) {
            await this.analyzeProject(file);
          }
          break;
        case KeymanFileTypes.Source.KeymanKeyboard:
          // The cleanest way to do this is to compile the .kmn to find the .kvks
          // and .keyman-touch-layout from the &VISUALKEYBOARD and &LAYOUTFILE
          // system stores
          await this.analyzeKmnKeyboard(file);
          break;
        case KeymanFileTypes.Source.LdmlKeyboard:
          // TODO: await this.analyzeLdmlKeyboard(file);
          // note, will need to skip .xml files that are not ldml keyboards
          break;
        default:
          // Ignore any other file types; this way we can analyze project files/folders
          // easily also
      }
    }
  }

  private async analyzeProject(filename: string): Promise<void> {
    const reader = new KPJFileReader(this.callbacks);
    const source = reader.read(this.callbacks.loadFile(filename));
    const project = reader.transform(filename, source);
    let files = project.files.map(file => this.callbacks.resolveFilename(filename, file.filePath));
    await this.analyze(files, false); // false because we don't want get into a recursive loop for projects
  }

  public async analyzeProjectFolder(folder: string) {
    // TODO: consider reworking slightly alongside kmc build BuildProject, with
    //       common file vs folder logic to be refactored to be a shared helper
    //       function, probably with the KpjFileReader?

    let kpjFile = this.callbacks.path.join(folder, this.callbacks.path.basename(folder) + KeymanFileTypes.Source.Project);

    if(this.callbacks.fs.existsSync(kpjFile)) {
      this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'project', name:kpjFile}));
      await this.analyzeProject(kpjFile);
    } else {
      this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'project folder', name:folder}));
      const project = new KeymanDeveloperProject(kpjFile, '2.0', this.callbacks);
      project.populateFiles();
      let files = project.files.map(file => this.callbacks.resolveFilename(kpjFile, file.filePath));


      await this.analyze(files, false); // false because we don't want get into a recursive loop for projects
    }
  }

  private async analyzeKmnKeyboard(filename: string): Promise<void> {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'keyboard source', name:filename}));

    const kmnCompiler = new KmnCompiler();
    if(!await kmnCompiler.init(this.callbacks)) {
      // TODO: error handling
      console.error('kmx compiler failed to init');
      process.exit(1);
    }

    // Note, output filename here is just to provide path data,
    // as nothing is written to disk
    let result = kmnCompiler.runCompiler(filename, filename + '.tmp', {
      shouldAddCompilerVersion: false,
      saveDebug: false,
      target: 'js'
    });

    if(!result) {
      //TODO: error handling
      process.exit(1);
    }

    if(result.data.kvksFilename) {
      this.addStrings(this.scanVisualKeyboard(this.callbacks.resolveFilename(filename, result.data.kvksFilename)));
    }

    const reader = new KmxFileReader();
    const keyboard: KMX.KEYBOARD = reader.read(result.kmx.data);
    const touchLayoutStore = keyboard.stores.find(store => store.dwSystemID == KMX.KMXFile.TSS_LAYOUTFILE);

    if(touchLayoutStore) {
      this.addStrings(this.scanTouchLayout(this.callbacks.resolveFilename(filename, touchLayoutStore.dpString)));
    }
  }

  private addStrings(strings: string[]) {
    this._strings = this._strings.concat(...strings);
  }

  //
  // On Screen Keyboard file scanning
  //

  private scanVisualKeyboard(filename: string): string[] {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'visual keyboard', name:filename}));
    let strings: string[] = [];
    const reader = new KvksFileReader();
    const source = reader.read(this.callbacks.loadFile(filename));
    let invalidKeys: string[] = [];
    const vk = reader.transform(source, invalidKeys);
    // TODO check vk, invalidKeys
    for(let key of vk.keys) {
      if(key.text) {
        strings.push(key.text);
      }
    }
    return strings;
  }

  private scanTouchLayout(filename: string): string[] {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'touch layout', name:filename}));
    let strings: string[] = [];
    const reader = new TouchLayoutFileReader();
    const source = reader.read(this.callbacks.loadFile(filename));
    // TODO: handle errors
    const scanKey = (key: TouchLayout.TouchLayoutKey | TouchLayout.TouchLayoutSubKey) => {
      if(!key.text) {
        return;
      }
      if(key.text.length > 2 && key.text[0] == '*' && key.text[key.text.length-1] == '*') {
        // Don't add '*special*' key captions
        return;
      }
      strings.push(key.text);
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

  public get strings(): string[] {
    // Sort and remove duplicates
    const strings = [...new Set(this._strings)];
    strings.sort();
    return strings;
  }

  public getStrings(format?: 'text'|'markdown'|'json'): string[] {
    switch(format) {
      case 'markdown':
        return AnalyzeOskCharacterUse.getStringsAsMarkdown(this.strings);
      case 'json':
        return AnalyzeOskCharacterUse.getStringsAsJson(this.strings);
      }
    return AnalyzeOskCharacterUse.getStringsAsText(this.strings);
  }

  // Following functions are static so that we can keep them pure
  // and potentially refactor into separate reporting class later

  private static getStringsAsText(strings: string[]) {
    let lines: string[] = [];
    for(let s of strings) {
      let ux = this.stringToUnicodeSequence(s);
      lines.push(ux + '\t' + s);
    }
    return lines;
  }

  private static getStringsAsMarkdown(strings: string[]) {
    let lines: string[] = [];
    lines.push('Code Points | Key Caps');
    lines.push('------------|---------');
    for(let s of strings) {
      let ux = this.stringToUnicodeSequence(s);
      lines.push(ux + ' | ' + this.escapeMarkdownChar(s));
    }
    return lines;
  }

  private static getStringsAsJson(strings: string[]) {
    let data: string[] = [];
    for(let s of strings) {
      data.push(s);
    }
    return JSON.stringify(data, null, 2).split('\n');
  }

  // TODO: this only works for single-character strings -- bad bad bad
  private static escapeMarkdownChar(s: string) {
    // commonmark 2.4: all punct can be escaped
    const punct = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~';

    if(punct.includes(s)) {
      return '\\' + s;
    }

    // TODO: there are other chars, use a library or regex match?
    switch(s) {
      case '\n': return '\\\\n';
      case '\r': return '\\\\r';
      case '\t': return '\\\\t';
      case ' ':  return '&#x20;';
      case String.fromCharCode(0xa0): return '&#xa0;';
    }
    return s;
  }

  private static stringToUnicodeSequence(s: string): string {
    let result = [];
    for(let ch of s) {
      let c = ch.codePointAt(0).toString(16).toUpperCase();
      if(c.length < 4) c = '0'.repeat(4 - c.length) + c;
      result.push(`U+${c}`);
    }
    return result.join(' ');
  }

}