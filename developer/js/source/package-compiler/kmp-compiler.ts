// <reference path="lexical-model.ts" />
/// <reference path="kps-file.ts" />
/// <reference path="kmp-json-file.ts" />

let fs = require('fs');
let path = require('path');
let xml2js = require('xml2js');
let zip = require('node-zip')();

export default class KmpCompiler {

  public transformKpsToKmpObject(kpsString: string): KmpJsonFile {

    // Load the KPS data from XML as JS structured data.

    let kpsPackage = (() => {
        let a;
        let parser = new xml2js.Parser({
          tagNameProcessors: [xml2js.processors.firstCharLowerCase],
          explicitArray: false
        });
        parser.parseString(kpsString, (e, r) => { a = r });
        return a;
    })();

    let kps: KpsFile = kpsPackage.package as KpsFile;

    //
    // To convert to kmp.json, we need to:
    //
    //   1. Unwrap arrays (and convert to array where single object)
    //   2. Fix casing on `iD`
    //   3. Rewrap info, keyboard.languages, lexicalModel.languages, startMenu.items elements
    //   4. Convert options.followKeyboardVersion to a bool
    //   5. Filenames need to be basenames (but this comes after processing)
    //

    // Helper functions

    let kpsInfoToKmpInfo = function (info: KpsFileInfo): KmpJsonFileInfo {
      let ni: KmpJsonFileInfo = {};

      ['author', 'copyright', 'name', 'version', 'website'].forEach(element => {
        if(info[element]) {
          ni[element] = {description: info[element]._ || info[element]};
          if(info[element].$ && info[element].$.URL) ni[element].url = info[element].$.URL;
        }
      });
      return ni;
    };

    let arrayWrap = function(a) {
      if(Array.isArray(a)) {
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
      options: {
        followKeyboardVersion: kps.options.followKeyboardVersion === ''
      }
    };

    // Fill in additional fields

    ['executeProgram', 'graphicFile', 'msiFilename', 'msiOptions', 'readmeFile'].forEach((element) => {
      if(kps.options[element]) kmp.options[element] = kps.options[element];
    });

    if(kps.info) {
      kmp.info = kpsInfoToKmpInfo(kps.info);
    }

    if(kps.files && kps.files.file) {
      kmp.files = arrayWrap(kps.files.file);
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

  public buildKmpFile(kmpJsonData: KmpJsonFile, kmpFileName: string) {
    const kmpJsonFileName = 'kmp.json';

    if(!kmpJsonData.files) {
      kmpJsonData.files = [];
    }

    kmpJsonData.files.forEach(function(value) {
      // Make file path slashes compatible across platforms
      let filename : string = value.name.replace(/\\/g, "/");

      let data = fs.readFileSync(path.join('../source', filename), 'utf8');
      zip.file(path.basename(filename), data);

      // Remove path data from files before save
      value.name = path.basename(filename);
    });

    zip.file(kmpJsonFileName, JSON.stringify(kmpJsonData, null, 2));

    // Generate kmp file
    var data = zip.generate({base64:false,compression:'DEFLATE'});
    fs.writeFileSync(kmpFileName, data, 'binary');

  }
}
