/// <reference path="kps-file.ts" />
/// <reference path="kmp-json-file.ts" />

import * as fs from 'fs';
import * as path from 'path';
import * as xml2js from 'xml2js';
import * as JSZip from 'jszip';

let zip = JSZip();

export default class KmpCompiler {

  private kpsFilename: string;

  public transformKpsToKmpObject(kpsString: string, kpsFilename: string): KmpJsonFile {

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

    this.kpsFilename = kpsFilename;

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

    // Helper functions

    let kpsInfoToKmpInfo = function (info: KpsFileInfo): KmpJsonFileInfo {
      let ni: KmpJsonFileInfo = {};

      let keys: (keyof KpsFileInfo)[] = ['author', 'copyright', 'name', 'version', 'website'];
      for (let element of keys) {
        if (info[element]) {
          ni[element] = {description: info[element]._ || info[element].toString()};
          if(info[element].$ && info[element].$.URL) ni[element].url = info[element].$.URL;
        }
      }
      return ni;
    };

    let arrayWrap = function(a: unknown) {
      if (Array.isArray(a)) {
        return a;
      }
      return [a];
    };

    let kpsLanguagesToKmpLanguages = function(language: KpsFileLanguage[]): KmpJsonFileLanguage[] {
      return language.map((element) => { return { name: element._, id: element.$.ID } });
    };

    // Start to construct the kmp.json file from the .kps file

    let kmp: KmpJsonFile = {
      system: kps.system,
      options: {}
    };

    // Fill in additional fields

    let keys: (keyof KpsFileOptions & keyof KmpJsonFileOptions)[] = ['executeProgram', 'graphicFile', 'msiFilename', 'msiOptions', 'readmeFile'];
    for (let element of keys) {
      if (kps.options[element]) {
        kmp.options[element] = kps.options[element];
      }
    }

    if(kps.info) {
      kmp.info = kpsInfoToKmpInfo(kps.info);
    }

    if(kps.files && kps.files.file) {
      kmp.files = arrayWrap(kps.files.file).map((file: KpsFileContentFile) => {
        return {
          name: file.name,
          description: file.description,
          copyLocation: parseInt(file.copyLocation, 10)
          // note: we don't emit fileType as that is not permitted in kmp.json
        }
      });
    }

    if(kps.keyboards && kps.keyboards.keyboard) {
      kmp.keyboards = arrayWrap(kps.keyboards.keyboard).map((keyboard: KpsFileKeyboard) => {
        return { name:keyboard.name, id:keyboard.iD, version:keyboard.version, languages: kpsLanguagesToKmpLanguages(arrayWrap(keyboard.languages.language) as KpsFileLanguage[]) }
      });
    }

    if(kps.lexicalModels && kps.lexicalModels.lexicalModel) {
      kmp.lexicalModels = arrayWrap(kps.lexicalModels.lexicalModel).map((model: KpsFileLexicalModel) => {
        return { name:model.name, id:model.iD, languages: kpsLanguagesToKmpLanguages(arrayWrap(model.languages.language) as KpsFileLanguage[]) }
      });
    }

    if(kps.startMenu) {
      kmp.startMenu = {};
      if(kps.startMenu.addUninstallEntry) kmp.startMenu.addUninstallEntry = kps.startMenu.addUninstallEntry === '';
      if(kps.startMenu.folder) kmp.startMenu.folder = kps.startMenu.folder;
      if(kps.startMenu.items && kps.startMenu.items.item) kmp.startMenu.items = arrayWrap(kps.startMenu.items.item);
    }

    if(kps.strings && kps.strings.string) {
      kmp.strings = arrayWrap(kps.strings.string);
    }

    //let util = require('util');
    //console.log(util.inspect(kmp, false, null))
    //console.log(kps);

    return kmp;
  }

  /**
   * Returns a Promise to the serialized data which can then be written to a .kmp file.
   *
   * @param kmpJsonData - The kmp.json Object
   */
  public buildKmpFile(kmpJsonData: KmpJsonFile): Promise<any> {
    const kmpJsonFileName = 'kmp.json';

    if(!kmpJsonData.files) {
      kmpJsonData.files = [];
    }

    const kpsPath = path.dirname(this.kpsFilename);

    kmpJsonData.files.forEach(function(value) {
      // Make file path slashes compatible across platforms
      const filename = value.name.replace(/\\/g, "/");
      const fullFilename = path.resolve(kpsPath, filename);
      const baseFilename = path.basename(filename);

      const data = fs.readFileSync(fullFilename, 'utf8');
      zip.file(baseFilename, data);

      // Remove path data from files before save
      value.name = baseFilename;
    });

    zip.file(kmpJsonFileName, JSON.stringify(kmpJsonData, null, 2));

    // Generate kmp file
    return zip.generateAsync({type: 'binarystring', compression:'DEFLATE'});
  }
}
