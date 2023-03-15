import * as fs from 'fs';
import * as path from 'path';
import * as xml2js from 'xml2js';
import JSZip from 'jszip';
import KEYMAN_VERSION from "@keymanapp/keyman-version";

import type { KpsFile, KpsFileContentFile, KpsFileInfo, KpsFileKeyboard, KpsFileLanguage, KpsFileLexicalModel, KpsFileOptions, KpsPackage } from './kps-file.js';
import type { KmpJsonFile, KmpJsonFileInfo, KmpJsonFileLanguage, KmpJsonFileOptions } from './kmp-json-file.js';

export { type KmpJsonFile } from './kmp-json-file.js';

const FILEVERSION_KMP_JSON = '12.0';

export default class KmpCompiler {

  public transformKpsToKmpObject(kpsString: string, kpsPath: string): KmpJsonFile {

    // Load the KPS data from XML as JS structured data.

    let kpsPackage = (() => {
        let a: KpsPackage;
        let parser = new xml2js.Parser({
          tagNameProcessors: [xml2js.processors.firstCharLowerCase],
          explicitArray: false
        });
        parser.parseString(kpsString, (e: unknown, r: unknown) => { a = r as KpsPackage });
        return a;
    })();

    let kps: KpsFile = kpsPackage.package;

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

    let kmp: KmpJsonFile = {
      system: {
        fileVersion: FILEVERSION_KMP_JSON,
        keymanDeveloperVersion: KEYMAN_VERSION.VERSION
      },
      options: {}
    };

    //
    // Fill in additional fields
    //

    let keys: [keyof KpsFileOptions, keyof KmpJsonFileOptions][] = [
      ['executeProgram','executeProgram'],
      ['graphicFile', 'graphicFile'],
      ['msiFileName','msiFilename'],
      ['msiOptions', 'msiOptions'],
      ['readMeFile', 'readmeFile']
    ];
    for (let [src,dst] of keys) {
      if (kps.options[src]) {
        kmp.options[dst] = kps.options[src];
      }
    }

    //
    // Add basic metadata
    //

    if(kps.info) {
      kmp.info = this.kpsInfoToKmpInfo(kps.info);
    }

    // FollowKeyboardVersion support

    if(kps.options?.followKeyboardVersion !== undefined) {
      kmp.info.version = {
        description: this.extractKeyboardVersionFromKmx(kpsPath, kps)
      };
    }

    //
    // Add file metadata
    //

    if(kps.files && kps.files.file) {
      kmp.files = this.arrayWrap(kps.files.file).map((file: KpsFileContentFile) => {
        return {
          name: file.name,
          description: file.description,
          copyLocation: parseInt(file.copyLocation, 10) || undefined
          // note: we don't emit fileType as that is not permitted in kmp.json
        };
      });
    }
    kmp.files = kmp.files ?? [];

    // Add the standard kmp.json self-referential to match existing implementations
    kmp.files.push({
      name: "kmp.json",
      description: "Package information (JSON)"
    });

    //
    // Add keyboard metadata
    //

    if(kps.keyboards && kps.keyboards.keyboard) {
      kmp.keyboards = this.arrayWrap(kps.keyboards.keyboard).map((keyboard: KpsFileKeyboard) => {
        return {
          displayFont: keyboard.displayFont ? path.basename(keyboard.displayFont.replaceAll('\\', '/')) : undefined,
          oskFont: keyboard.oSKFont ? path.basename(keyboard.oSKFont.replaceAll('\\', '/')) : undefined,
          name:keyboard.name,
          id:keyboard.iD,
          version:keyboard.version,
          languages: this.kpsLanguagesToKmpLanguages(this.arrayWrap(keyboard.languages.language) as KpsFileLanguage[])
        };
      });
    }

    //
    // Add lexical-model metadata
    //

    if(kps.lexicalModels && kps.lexicalModels.lexicalModel) {
      kmp.lexicalModels = this.arrayWrap(kps.lexicalModels.lexicalModel).map((model: KpsFileLexicalModel) => {
        return { name:model.name, id:model.iD, languages: this.kpsLanguagesToKmpLanguages(this.arrayWrap(model.languages.language) as KpsFileLanguage[]) }
      });
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

    kmp = this.stripUndefined(kmp) as KmpJsonFile;

    return kmp;
  }

    // Helper functions

  private kpsInfoToKmpInfo(info: KpsFileInfo): KmpJsonFileInfo {
    let ni: KmpJsonFileInfo = {};

    const keys: [(keyof KpsFileInfo), (keyof KmpJsonFileInfo)][] = [
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

  private kpsLanguagesToKmpLanguages(language: KpsFileLanguage[]): KmpJsonFileLanguage[] {
    return language.map((element) => { return { name: element._, id: element.$.ID } });
  };

  private extractKeyboardVersionFromKmx(kpsPath: string, kps: KpsFile) {
    // TODO-LDML: #7340 this is incomplete; we need to read from first .kmx
    // When we do this, we'll need to update fixtures to have the correct
    // version string also
    if(!kps.keyboards || !kps.keyboards.keyboard) {
      // We don't read from the kps metadata because we want the keyboard
      // version data here;
      // TODO: currently model files don't support follow-version?
      return '0.0.0';
    }
    let k: KpsFileKeyboard[] = this.arrayWrap(kps.keyboards.keyboard);
    return k?.[0]?.version ?? '0.0.0';
  }

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
  public buildKmpFile(kpsFilename: string, kmpJsonData: KmpJsonFile): Promise<string> {
    const zip = JSZip();

    const kmpJsonFileName = 'kmp.json';

    const basePath = path.dirname(kpsFilename);

    // Make a copy of kmpJsonData, as we mutate paths for writing
    const data: KmpJsonFile = JSON.parse(JSON.stringify(kmpJsonData));
    if(!data.files) {
      data.files = [];
    }

    data.files.forEach(function(value) {
      // Get the path of the file
      let filename = value.name;

      // We add this separately after zipping all other files
      if(filename == 'kmp.json') {
        return;
      }

      if(path.isAbsolute(value.name)) {
        // absolute paths are not very cross-platform compatible -- we are going to have trouble
        // with path separators and roots
        // TODO: emit a warning
      } else {
        // Transform separators to platform separators -- kps files may use
        // either / or \, although older kps files were always \.
        if(path.sep == '/') {
          filename = filename.replace(/\\/g, '/');
        } else {
          filename = filename.replace(/\//g, '\\');
        }
        filename = path.resolve(basePath, filename);
      }
      const basename = path.basename(filename);
      let data = fs.readFileSync(filename);
      zip.file(basename, data);

      // Remove path data from files before JSON save
      value.name = basename;
    });

    zip.file(kmpJsonFileName, JSON.stringify(data, null, 2));

    // Generate kmp file
    return zip.generateAsync({type: 'binarystring', compression:'DEFLATE'});
  }
}
