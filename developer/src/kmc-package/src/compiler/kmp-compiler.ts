import * as xml2js from 'xml2js';
import JSZip from 'jszip';
import KEYMAN_VERSION from "@keymanapp/keyman-version";

import { CompilerCallbacks, KeymanFileTypes, KvkFile } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { KmpJsonFile, KpsFile } from '@keymanapp/common-types';
import { PackageVersionValidation } from './package-version-validation.js';
import { KmpInfWriter } from './kmp-inf-writer.js';
import { transcodeToCP1252 } from './cp1252.js';

const KMP_JSON_FILENAME = 'kmp.json';
const KMP_INF_FILENAME = 'kmp.inf';

export class KmpCompiler {

  constructor(private callbacks: CompilerCallbacks) {
  }

  public transformKpsToKmpObject(kpsFilename: string): KmpJsonFile.KmpJsonFile {
    // Load the KPS data from XML as JS structured data.
    const buffer = this.callbacks.loadFile(kpsFilename);
    if(!buffer) {
      this.callbacks.reportMessage(CompilerMessages.Error_FileDoesNotExist({filename: kpsFilename}));
      return null;
    }
    const data = new TextDecoder().decode(buffer);

    const kpsPackage = (() => {
        let a: KpsFile.KpsPackage;
        let parser = new xml2js.Parser({
          tagNameProcessors: [xml2js.processors.firstCharLowerCase],
          explicitArray: false
        });
        // TODO: add unit test for xml errors parsing .kps file
        parser.parseString(data, (e: unknown, r: unknown) => { if(e) throw e; a = r as KpsFile.KpsPackage });
        return a;
    })();

    let kps: KpsFile.KpsFile = kpsPackage.package;

    //
    // To convert to kmp.json, we need to:
    //
    //   1. Unwrap arrays (and convert to array where single object)
    //   2. Fix casing on `iD`
    //   3. Rewrap info, keyboard.languages, lexicalModel.languages, startMenu.items elements
    //   4. Remove options.followKeyboardVersion, file.fileType
    //   5. Convert file.copyLocation to a Number
    //   6. Filenames need to be basenames (but this comes after processing)
    //

    // Start to construct the kmp.json file from the .kps file

    let kmp: KmpJsonFile.KmpJsonFile = {
      system: {
        fileVersion: FILEVERSION_KMP_JSON,
        keymanDeveloperVersion: KEYMAN_VERSION.VERSION
      },
      options: {}
    };

    //
    // Fill in additional fields
    //

    let keys: [keyof KpsFile.KpsFileOptions, keyof KmpJsonFile.KmpJsonFileOptions][] = [
      ['executeProgram','executeProgram'],
      ['graphicFile', 'graphicFile'],
      ['msiFileName','msiFilename'],
      ['msiOptions', 'msiOptions'],
      ['readMeFile', 'readmeFile']
    ];
    if(kps.options) {
      for (let [src,dst] of keys) {
        if (kps.options[src]) {
          if(dst == 'graphicFile' || dst == 'readmeFile') {
            kmp.options[dst] = /[/\\]?([^/\\]*)$/.exec(kps.options[src])[1];
          } else {
            kmp.options[dst] = kps.options[src];
          }
        }
      }
    }

    //
    // Add basic metadata
    //

    if(kps.info) {
      kmp.info = this.kpsInfoToKmpInfo(kps.info);
    }

    //
    // Add file metadata
    //

    if(kps.files && kps.files.file) {
      kmp.files = this.arrayWrap(kps.files.file).map((file: KpsFile.KpsFileContentFile) => {
        return {
          name: file.name.trim(),
          description: file.description.trim(),
          copyLocation: parseInt(file.copyLocation, 10) || undefined
          // note: we don't emit fileType as that is not permitted in kmp.json
        };
      });
    }
    kmp.files = kmp.files ?? [];

    // Keyboard packages also include a legacy kmp.inf file (this will be removed,
    // one day)
    if(kps.keyboards && kps.keyboards.keyboard) {
      kmp.files.push({
        name: KMP_INF_FILENAME,
        description: "Package information"
      });
    }

    // Add the standard kmp.json self-referential to match existing implementations
    kmp.files.push({
      name: KMP_JSON_FILENAME,
      description: "Package information (JSON)"
    });

    //
    // Add keyboard metadata
    //

    if(kps.keyboards && kps.keyboards.keyboard) {
      kmp.keyboards = this.arrayWrap(kps.keyboards.keyboard).map((keyboard: KpsFile.KpsFileKeyboard) => ({
        displayFont: keyboard.displayFont ? this.callbacks.path.basename(keyboard.displayFont) : undefined,
        oskFont: keyboard.oSKFont ? this.callbacks.path.basename(keyboard.oSKFont) : undefined,
        name:keyboard.name.trim(),
        id:keyboard.iD.trim(),
        version:keyboard.version.trim(),
        rtl:keyboard.rTL == 'True' ? true : undefined,
        languages: keyboard.languages ?
          this.kpsLanguagesToKmpLanguages(this.arrayWrap(keyboard.languages.language) as KpsFile.KpsFileLanguage[]) :
          []
      }));
    }

    //
    // Add lexical-model metadata
    //

    if(kps.lexicalModels && kps.lexicalModels.lexicalModel) {
      kmp.lexicalModels = this.arrayWrap(kps.lexicalModels.lexicalModel).map((model: KpsFile.KpsFileLexicalModel) => ({
        name:model.name.trim(),
        id:model.iD.trim(),
        languages: model.languages ?
          this.kpsLanguagesToKmpLanguages(this.arrayWrap(model.languages.language) as KpsFile.KpsFileLanguage[]) : []
      }));
    }

    //
    // Verify version metadata; doing this in the transform
    // while we have access to the .kps metadata, and keeping the
    //

    const versionValidator = new PackageVersionValidation(this.callbacks);
    if(!versionValidator.validateAndUpdateVersions(kpsFilename, kps, kmp)) {
      return null;
    }

    //
    // Add Windows Start Menu metadata
    //

    if(kps.startMenu && kps.startMenu.items) {
      kmp.startMenu = {};
      if(kps.startMenu.addUninstallEntry) kmp.startMenu.addUninstallEntry = kps.startMenu.addUninstallEntry === '';
      if(kps.startMenu.folder) kmp.startMenu.folder = kps.startMenu.folder;
      if(kps.startMenu.items && kps.startMenu.items.item) kmp.startMenu.items = this.arrayWrap(kps.startMenu.items.item);
    }

    //
    // Add translation strings
    //

    if(kps.strings && kps.strings.string) {
      kmp.strings = this.arrayWrap(kps.strings.string);
    }

    kmp = this.stripUndefined(kmp) as KmpJsonFile.KmpJsonFile;

    return kmp;
  }

    // Helper functions

  private kpsInfoToKmpInfo(info: KpsFile.KpsFileInfo): KmpJsonFile.KmpJsonFileInfo {
    let ni: KmpJsonFile.KmpJsonFileInfo = {};

    const keys: [(keyof KpsFile.KpsFileInfo), (keyof KmpJsonFile.KmpJsonFileInfo)][] = [
      ['author','author'],
      ['copyright','copyright'],
      ['name','name'],
      ['version','version'],
      ['webSite','website']
    ];

    for (let [src,dst] of keys) {
      if (info[src]) {
        ni[dst] = {description: info[src]._ ?? (typeof info[src] == 'string' ? info[src].toString() : '')};
        if(info[src].$ && info[src].$.URL) ni[dst].url = info[src].$.URL;
      }
    }

    return ni;
  };

  private arrayWrap(a: unknown) {
    if (Array.isArray(a)) {
      return a;
    }
    return [a];
  };

  private kpsLanguagesToKmpLanguages(language: KpsFile.KpsFileLanguage[]): KmpJsonFile.KmpJsonFileLanguage[] {
    if(language.length == 0 || language[0] == undefined) {
      return [];
    }
    return language.map((element) => { return { name: element._, id: element.$.ID } });
  };



  private stripUndefined(o: any) {
    for(const key in o) {
      if(o[key] === undefined) {
        delete o[key];
      } else if(typeof o[key] == 'object') {
        o[key] = this.stripUndefined(o[key]);
      }
    }
    return o;
  }

  /**
   * Returns a Promise to the serialized data which can then be written to a .kmp file.
   *
   * @param kpsFilename - Filename of the kps, not read, used only for calculating relative paths
   * @param kmpJsonData - The kmp.json Object
   */
  public buildKmpFile(kpsFilename: string, kmpJsonData: KmpJsonFile.KmpJsonFile): Promise<string> {
    const zip = JSZip();


    // Make a copy of kmpJsonData, as we mutate paths for writing
    const data: KmpJsonFile.KmpJsonFile = JSON.parse(JSON.stringify(kmpJsonData));
    if(!data.files) {
      data.files = [];
    }

    const hasKmpInf = !!data.files.find(file => file.name == KMP_INF_FILENAME);

    let failed = false;
    data.files.forEach((value) => {
      // Get the path of the file
      let filename = value.name;

      // We add this separately after zipping all other files
      if(filename == KMP_JSON_FILENAME || filename == KMP_INF_FILENAME) {
        return;
      }

      if(this.callbacks.path.isAbsolute(filename)) {
        // absolute paths are not portable to other computers
        this.callbacks.reportMessage(CompilerMessages.Warn_AbsolutePath({filename: filename}));
      }

      filename = this.callbacks.resolveFilename(kpsFilename, filename);
      const basename = this.callbacks.path.basename(filename);

      if(!this.callbacks.fs.existsSync(filename)) {
        this.callbacks.reportMessage(CompilerMessages.Error_FileDoesNotExist({filename: filename}));
        failed = true;
        return;
      }

      let memberFileData;
      try {
        memberFileData = this.callbacks.loadFile(filename);
      } catch(e) {
        this.callbacks.reportMessage(CompilerMessages.Error_FileCouldNotBeRead({filename: filename, e: e}));
        failed = true;
        return;
      }

      this.warnIfKvkFileIsNotBinary(filename, memberFileData);

      zip.file(basename, memberFileData);

      // Remove path data from files before JSON save
      value.name = basename;
    });

    if(failed) {
      return null;
    }

    zip.file(KMP_JSON_FILENAME, JSON.stringify(data, null, 2));
    if(hasKmpInf) {
      zip.file(KMP_INF_FILENAME, this.buildKmpInf(data));
    }

    // Generate kmp file
    return zip.generateAsync({type: 'binarystring', compression:'DEFLATE'});
  }

  private buildKmpInf(data: KmpJsonFile.KmpJsonFile): Uint8Array {
    const writer = new KmpInfWriter(data);
    const s = writer.write();
    return transcodeToCP1252(s);
  }

  /**
   * Legacy .kmp compiler would transform xml-format .kvk files into a binary .kvk file; now
   * we want that to remain the responsibility of the keyboard compiler, so we'll warn the
   * few users who are still doing this
   */
  private warnIfKvkFileIsNotBinary(filename: string, data: Uint8Array) {
    if(!KeymanFileTypes.filenameIs(filename, KeymanFileTypes.Binary.VisualKeyboard)) {
      return;
    }

    if(data.byteLength < 4) {
      // TODO: Not a valid .kvk file; should we be reporting this?
      return;
    }

    if(data[0] != KvkFile.KVK_HEADER_IDENTIFIER_BYTES[0] ||
      data[1] != KvkFile.KVK_HEADER_IDENTIFIER_BYTES[1] ||
      data[2] != KvkFile.KVK_HEADER_IDENTIFIER_BYTES[2] ||
      data[3] != KvkFile.KVK_HEADER_IDENTIFIER_BYTES[3]) {
      this.callbacks.reportMessage(CompilerMessages.Warn_FileIsNotABinaryKvkFile({filename: filename}));
    }
  }
}
