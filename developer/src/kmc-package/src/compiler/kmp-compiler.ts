import * as xml2js from 'xml2js';
import JSZip from 'jszip';
import KEYMAN_VERSION from "@keymanapp/keyman-version";

import { CompilerCallbacks, KeymanFileTypes, KvkFile } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { KmpJsonFile, KpsFile } from '@keymanapp/common-types';
import { PackageMetadataCollector } from './package-metadata-collector.js';
import { KmpInfWriter } from './kmp-inf-writer.js';
import { transcodeToCP1252 } from './cp1252.js';
import { MIN_LM_FILEVERSION_KMP_JSON, PackageVersionValidator } from './package-version-validator.js';
import { PackageKeyboardTargetValidator } from './package-keyboard-target-validator.js';
import { PackageMetadataUpdater } from './package-metadata-updater.js';

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
        fileVersion: null,
        keymanDeveloperVersion: KEYMAN_VERSION.VERSION
      },
      options: {}
    };

    //
    // Fill in additional fields
    //

    if(kps.options) {
      kmp.options.executeProgram = kps.options?.executeProgram || undefined;
      if(kps.options.graphicFile) {
        kmp.options.graphicFile = /[/\\]?([^/\\]*)$/.exec(kps.options.graphicFile)[1];
      }
      kmp.options.msiFilename = kps.options.msiFileName;
      kmp.options.msiOptions = kps.options.msiOptions;
      if(kps.options.readMeFile) {
        kmp.options.readmeFile = /[/\\]?([^/\\]*)$/.exec(kps.options.readMeFile)[1];
      }
    }

    // TODO: this.addFontMetadata();

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
          [],
        examples: keyboard.examples ?
          (this.arrayWrap(keyboard.examples.example) as KpsFile.KpsFileLanguageExample[]).map(
            e => ({id: e.$.ID, keys: e.$.keys, text: e.$.text, note: e.$.note})
          ) as KmpJsonFile.KmpJsonFileExample[] :
          undefined
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
    // Collect metadata from keyboards (and later models) in order to update
    // the kmp.json metadata for use downstream in apps. This will also be
    // used later to fill in .keyboard_info file data.
    //

    const collector = new PackageMetadataCollector(this.callbacks);
    const metadata = collector.collectKeyboardMetadata(kpsFilename, kmp);
    if(metadata == null) {
      return null;
    }

    //
    // Verify keyboard versions and update version metadata where appropriate
    //

    const versionValidator = new PackageVersionValidator(this.callbacks);
    if(!versionValidator.validateAndUpdateVersions(kps, kmp, metadata)) {
      return null;
    }

    if(kps.keyboards && kps.keyboards.keyboard) {
      kmp.system.fileVersion = versionValidator.getMinKeymanVersion(metadata);
    } else {
      kmp.system.fileVersion = MIN_LM_FILEVERSION_KMP_JSON;
    }

    //
    // Verify that packages that target mobile devices include a .js file
    //

    const targetValidator = new PackageKeyboardTargetValidator(this.callbacks);
    targetValidator.verifyAllTargets(kmp, metadata);

    //
    // Update assorted keyboard metadata from the keyboards in the package
    //

    const updater = new PackageMetadataUpdater();
    updater.updatePackage(metadata);

    //
    // Add Windows Start Menu metadata
    //

    if(kps.startMenu && (kps.startMenu.folder || kps.startMenu.items)) {
      kmp.startMenu = {};
      if(kps.startMenu.addUninstallEntry === '') kmp.startMenu.addUninstallEntry = true;
      if(kps.startMenu.folder) kmp.startMenu.folder = kps.startMenu.folder;
      if(kps.startMenu.items && kps.startMenu.items.item) {
        kmp.startMenu.items = this.arrayWrap(kps.startMenu.items.item);

        // Remove default values
        for(let item of kmp.startMenu.items) {
          if(item.icon == '') delete item.icon;
          if(item.location == 'psmelStartMenu') delete item.location;
          if(item.arguments == '') delete item.arguments;
          // Horrific case change between .kps and kmp.json:
          item.filename = (<any>item).fileName;
          delete (<any>item).fileName;
        }
      } else {
        kmp.startMenu.items = [];
      }
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
      ['webSite','website'],
      ['description','description'],
    ];

    for (let [src,dst] of keys) {
      if (info[src]) {
        ni[dst] = {description: (info[src]._ ?? (typeof info[src] == 'string' ? info[src].toString() : '').trim())};
        if(info[src].$ && info[src].$.URL) ni[dst].url = info[src].$.URL.trim();
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
