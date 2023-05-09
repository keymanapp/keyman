import { CompilerCallbacks, KeymanDeveloperProject, KMX, KmxFileReader, KPJFileReader, KvksFileReader, KVKSParseError, TouchLayout, TouchLayoutFileReader } from "@keymanapp/common-types";
import { Compiler } from '@keymanapp/kmc-kmn';

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

  public async analyze(files: string[]) {
    for(let file of files) {
      // TODO: refactor with helper function for file and folder types
      // See kmc for another instance
      if(file.match(/\.kvks$/i)) {
        this.addStrings(this.scanVisualKeyboard(file));
      }
      else if(file.match(/\.keyman-touch-layout$/i)) {
        this.addStrings(this.scanTouchLayout(file));
      }
      else if(file.match(/\.kpj$/i)) {
        await this.analyzeProject(file);
        continue;
      }
      else if(file.match(/\.kmn$/i)) {
        // The cleanest way to do this is to compile the .kmn to find the .kvks
        // and .keyman-touch-layout from the &VISUALKEYBOARD and &LAYOUTFILE
        // system stores
        await this.analyzeKmnKeyboard(file);
      }
      else if(file.match(/\.xml$/i)) {
        // TODO: await this.analyzeLdmlKeyboard(file);
        // note, will need to skip .xml files that are not ldml keyboards
      }
      else {
        // Ignore any other file types; this way we can analyze project files/folders
        // easily also
      }
    }
  }

  private async analyzeProject(filename: string): Promise<void> {
    // TODO: this.callbacks.reportMessage(...)  console.log(`Scanning project ${filename}`);
    const reader = new KPJFileReader(this.callbacks);
    const source = reader.read(this.callbacks.loadFile(filename));
    const project = reader.transform(filename, source);
    let files = project.files.map(file => this.callbacks.resolveFilename(filename, file.filePath));
    // TODO: ensure no .kpj in files so we don't potentially end up in a loop
    await this.analyze(files);
  }

  public async analyzeProjectFolder(folder: string) {
    // TODO: consider reworking slightly alongside kmc build BuildProject, with
    //       common file vs folder logic to be refactored to be a shared helper
    //       function, probably with the KpjFileReader?

    let kpjFile = this.callbacks.path.join(folder, this.callbacks.path.basename(folder) + '.kpj');

    if(this.callbacks.fs.existsSync(kpjFile)) {
      await this.analyzeProject(kpjFile);
    } else {
      const project = new KeymanDeveloperProject(kpjFile, '2.0', this.callbacks);
      project.populateFiles();
      let files = project.files.map(file => this.callbacks.resolveFilename(kpjFile, file.filePath));
      // TODO: ensure no .kpj in files so we don't potentially end up in a loop
      await this.analyze(files);
    }
  }

  private async analyzeKmnKeyboard(filename: string): Promise<void> {
    // let ...

    const kmxCompiler = new Compiler();
    if(!await kmxCompiler.init()) {
      // TODO: error handling
      console.error('kmx compiler failed to init');
      process.exit(1);
    }

    // TODO: this belongs in kmxCompiler.run, or better, by fixing kmxCompiler to use callbacks.loadFile
    filename = filename.replace(/\\/g, '/');

    // TODO: runToMemory, add option to kmxCompiler to store debug-data for conversion to .js (e.g. store metadata, group readonly metadata, etc)
    if(!kmxCompiler.run(filename, filename + '.tmp', this.callbacks, {
      shouldAddCompilerVersion: false,
      saveDebug: false,
      target: 'js'
    })) {
      //TODO: error handling
    }

    const reader = new KmxFileReader();
    const keyboard: KMX.KEYBOARD = reader.read(this.callbacks.loadFile(filename + '.tmp'));

    const kvkStore = keyboard.stores.find(store => store.dwSystemID == KMX.KMXFile.TSS_VISUALKEYBOARD);
    const touchLayoutStore = keyboard.stores.find(store => store.dwSystemID == KMX.KMXFile.TSS_LAYOUTFILE);

    if(kvkStore) {
      this.addStrings(this.scanVisualKeyboard(this.callbacks.resolveFilename(filename, kvkStore.dpString)));
    }

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
    let strings: string[] = [];
    // TODO: this.callbacks.reportMessage(...)  console.log(`Scanning visual keyboard ${filename}`);
    const reader = new KvksFileReader();
    const source = reader.read(this.callbacks.loadFile(filename));
    let errors: KVKSParseError[] = [];
    const vk = reader.transform(source, errors);
    // TODO check vk, errors
    for(let key of vk.keys) {
      if(key.text) {
        strings.push(key.text);
      }
    }
    return strings;
  }

  private scanTouchLayout(filename: string): string[] {
    let strings: string[] = [];
    // TODO: this.callbacks.reportMessage(...)  console.log(`Scanning touch layout ${filename}`);
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