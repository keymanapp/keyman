import { KmpJsonFile, CompilerCallbacks, KmxFileReader, KmxFileReaderError, KMX, KeymanFileTypes } from '@keymanapp/common-types';
import { getCompiledKmxKeyboardMetadata } from './kmx-keyboard-metadata.js';
import { PackageCompilerMessages } from './package-compiler-messages.js';
import { getCompiledWebKeyboardMetadata, WebKeyboardMetadata } from './web-keyboard-metadata.js';

export type KeyboardMetadata = {
  keyboard: KmpJsonFile.KmpJsonFileKeyboard;
  isJavascript: boolean;
  data: WebKeyboardMetadata;
};

export type KeyboardMetadataCollection = {[id:string]: KeyboardMetadata};

export class PackageMetadataCollector {

  constructor(private callbacks: CompilerCallbacks) {}

  public collectKeyboardMetadata(kpsFilename: string, kmp: KmpJsonFile.KmpJsonFile): KeyboardMetadataCollection {
    let result: KeyboardMetadataCollection = {};

    if(!kmp.keyboards) {
      return result;
    }

    for(let keyboard of kmp.keyboards) {
      result[keyboard.id] = this.getKeyboardFileData(kpsFilename, kmp, keyboard);
      if(!result[keyboard.id]) {
        // Warnings or errors will have been raised by getKeyboardFileData
        return null;
      }
    }

    return result;
  }

  private getKeyboardFileData(
    kpsFilename: string,
    kmp: KmpJsonFile.KmpJsonFile,
    keyboard: KmpJsonFile.KmpJsonFileKeyboard
  ): KeyboardMetadata {

    let isJavascript = false;
    let file = kmp.files.find(file => this.callbacks.path.basename(file.name ?? '', KeymanFileTypes.Binary.Keyboard) == keyboard.id);
    if(!file) {
      isJavascript = true;
      file = kmp.files.find(file => this.callbacks.path.basename(file.name ?? '', KeymanFileTypes.Binary.WebKeyboard) == keyboard.id);
      if(!file) {
        this.callbacks.reportMessage(PackageCompilerMessages.Error_KeyboardContentFileNotFound({id:keyboard.id}));
        return null;
      }
    }

    if(!file.name) {
      this.callbacks.reportMessage(PackageCompilerMessages.Error_FileRecordIsMissingName({description: file.description ?? '(no description)'}));
      return null;
    }

    const filename = this.callbacks.resolveFilename(kpsFilename, file.name);
    if(!this.callbacks.fs.existsSync(filename)) {
      this.callbacks.reportMessage(PackageCompilerMessages.Error_KeyboardFileNotFound({filename}));
      return null;
    }

    //
    // load the data from file
    //
    let fileData;
    try {
      fileData = this.callbacks.loadFile(filename);
    } catch(e) {
      this.callbacks.reportMessage(PackageCompilerMessages.Error_FileCouldNotBeRead({filename, e}));
      return null;
    }

    if(isJavascript) {
      const js = new TextDecoder().decode(fileData);
      return {keyboard, isJavascript, data: getCompiledWebKeyboardMetadata(js)};
    } else {
      const kmxReader: KmxFileReader = new KmxFileReader();
      let kmx: KMX.KEYBOARD;
      try {
        kmx = kmxReader.read(fileData);
      } catch(e) {
        if(e instanceof KmxFileReaderError) {
          // The file couldn't be read, it might not be a .kmx file
          this.callbacks.reportMessage(PackageCompilerMessages.Error_KeyboardFileNotValid({filename, e}));
          return null;
        } else {
          // Unknown error, bubble it up
          throw e;
        }
      }
      return {keyboard, isJavascript, data: getCompiledKmxKeyboardMetadata(kmx)};
    }
  }

}
