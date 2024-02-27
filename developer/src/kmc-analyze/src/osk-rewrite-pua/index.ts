import { CompilerCallbacks, KeymanFileTypes, KvksFile, KvksFileReader, KvksFileWriter, TouchLayoutFileReader, TouchLayoutFileWriter } from "@keymanapp/common-types";
import { Osk } from '@keymanapp/developer-utils';
import { CompilerMessages } from '@keymanapp/kmc-kmn';
import { getOskFromKmnFile } from "../util/get-osk-from-kmn-file.js";
import { AnalyzerMessages } from "../messages.js";

export class AnalyzeOskRewritePua {
  private _data: {[index:string]: Uint8Array} = {};

  constructor(private callbacks: CompilerCallbacks) {
  }

  public clear() {
    this._data = {};
  }

  //
  // Analyze a single file
  //

  public async analyze(file: string, mapping: Osk.StringResult[]): Promise<boolean> {
    let map: Osk.PuaMap = Osk.parseMapping(mapping);

    switch(KeymanFileTypes.sourceTypeFromFilename(file)) {
      case KeymanFileTypes.Source.VisualKeyboard:
        this._data[file] = this.analyzeVisualKeyboard(file, map);
        break;
      case KeymanFileTypes.Source.TouchLayout:
        this._data[file] = this.analyzeTouchLayout(file, map);
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

  private async analyzeKmnKeyboard(filename: string, map: Osk.PuaMap): Promise<boolean> {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'keyboard source', name:filename}));

    let osk = await getOskFromKmnFile(this.callbacks, filename);

    if(osk.kvksFilename) {
      this._data[osk.kvksFilename] = this.analyzeVisualKeyboard(osk.kvksFilename, map);
    }
    if(osk.touchLayoutFilename) {
      this._data[osk.touchLayoutFilename] = this.analyzeTouchLayout(osk.touchLayoutFilename, map);
    }

    return true;
  }


  //
  // On Screen Keyboard file scanning
  //


  private analyzeVisualKeyboard(filename: string, map: Osk.PuaMap): Uint8Array {
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
    const dirty = Osk.remapVisualKeyboard(vk, map);
    if(!dirty) {
      return null;
    }
    const writer = new KvksFileWriter();
    const data = writer.write(vk);
    // TODO: the encoder probably needs to go into KvksFileWRiter
    const enc = new TextEncoder();
    return enc.encode(data);
  }



  private analyzeTouchLayout(filename: string, map: Osk.PuaMap): Uint8Array {
    this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'touch layout', name:filename}));
    const reader = new TouchLayoutFileReader();
    const source = reader.read(this.callbacks.loadFile(filename));

    let dirty = Osk.remapTouchLayout(source, map);

    if(!dirty) {
      return null;
    }

    const writer = new TouchLayoutFileWriter({formatted: true});
    const data = writer.write(source);
    return data;
  }
}