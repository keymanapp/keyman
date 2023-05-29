import * as xml2js from 'xml2js';
import JSZip from 'jszip';
import KEYMAN_VERSION from "@keymanapp/keyman-version";

import { CompilerCallbacks, KvkFile } from '@keymanapp/common-types';
import { CompilerMessages } from './messages.js';
import { KmpJsonFile, KpsFile, KMX, KmxFileReader } from '@keymanapp/common-types';

const FILEVERSION_KMP_JSON = '12.0';

export class KmpCompiler {

  constructor(private callbacks: CompilerCallbacks) {
  }

  public transformKpsToKmpObject(kpsFilename: string): KmpJsonFile.KmpJsonFile {
    // Load the KPS data from XML as JS structured data.
    const data = this.callbacks.fs.readFileSync(kpsFilename, 'utf-8');

    const kpsPackage = (() => {
        let a: KpsFile.KpsPackage;
        let parser = new xml2js.Parser({
          tagNameProcessors: [xml2js.processors.firstCharLowerCase],
          explicitArray: false
        });
        parser.parseString(data, (e: unknown, r: unknown) => { a = r as KpsFile.KpsPackage });
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
          kmp.options[dst] = kps.options[src];
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
      kmp.keyboards = this.arrayWrap(kps.keyboards.keyboard).map((keyboard: KpsFile.KpsFileKeyboard) => {
        return {
          displayFont: keyboard.displayFont ? this.callbacks.path.basename(keyboard.displayFont) : undefined,
          oskFont: keyboard.oSKFont ? this.callbacks.path.basename(keyboard.oSKFont) : undefined,
          name:keyboard.name,
          id:keyboard.iD,
          version:keyboard.version,
          languages: this.kpsLanguagesToKmpLanguages(this.arrayWrap(keyboard.languages.language) as KpsFile.KpsFileLanguage[])
        };
      });
    }

    //
    // Add lexical-model metadata
    //

    if(kps.lexicalModels && kps.lexicalModels.lexicalModel) {
      kmp.lexicalModels = this.arrayWrap(kps.lexicalModels.lexicalModel).map((model: KpsFile.KpsFileLexicalModel) => {
        return { name:model.name, id:model.iD, languages: this.kpsLanguagesToKmpLanguages(this.arrayWrap(model.languages.language) as KpsFile.KpsFileLanguage[]) }
      });
    }

    //
    // FollowKeyboardVersion support
    //

    if(kps.options?.followKeyboardVersion !== undefined) {
      kmp.info.version = {
        description: this.extractKeyboardVersionFromKmx(kpsFilename, kmp)
      };
      // TODO: compare the extracted version with other keyboards in the package
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
    return language.map((element) => { return { name: element._, id: element.$.ID } });
  };

  private extractKeyboardVersionFromKmx(kpsFilename: string, kmp: KmpJsonFile.KmpJsonFile) {
    // The DEFAULT_VERSION used to be '1.0', but we now use '0.0' to allow
    // pre-release 0.x keyboards to be considered later than a keyboard without
    // any version metadata at all.
    const DEFAULT_VERSION = '0.0';

    // Note: there is often version metadata in the .kps <Keyboard> element, but
    // we don't read from the metadata because we want to ensure we have the
    // most up-to-date keyboard version data here, from the compiled keyboard.

    // Lexical model packages do not allow FollowKeyboardVersion
    if(kmp.lexicalModels && kmp.lexicalModels.length) {
      this.callbacks.reportMessage(CompilerMessages.Error_FollowKeyboardVersionNotAllowedForModelPackages());
      return DEFAULT_VERSION;
    }

    if(!kmp.keyboards || !kmp.keyboards.length) {
      this.callbacks.reportMessage(CompilerMessages.Warn_FollowKeyboardVersionButNoKeyboards());
      return DEFAULT_VERSION;
    }

    // Reset the keyboard version to the default in the kmp.json metadata, for
    // warning/failure code paths in this file
    kmp.keyboards[0].version = DEFAULT_VERSION;

    const file = kmp.files.find(file => this.callbacks.path.basename(file.name, '.kmx') == kmp.keyboards[0].id);
    if(!file) {
      this.callbacks.reportMessage(CompilerMessages.Error_KeyboardFileNotFound({id:kmp.keyboards[0].id}));
      return DEFAULT_VERSION;
    }

    const filename = this.callbacks.resolveFilename(kpsFilename, file.name);
    if(!this.callbacks.fs.existsSync(filename)) {
      // The zip phase will emit an error later if the file is missing, so
      // we can just bail cleanly here
      // console.debug(`The file ${filename} was not found`);
      return DEFAULT_VERSION;
    }

    //
    // load the .kmx and extract the version number
    //
    const kmxFileData = this.callbacks.loadFile(filename);
    const kmxReader: KmxFileReader = new KmxFileReader();
    const kmx: KMX.KEYBOARD = kmxReader.read(kmxFileData);
    if(!kmx) {
      // The file couldn't be read, it might be invalid or locked
      this.callbacks.reportMessage(CompilerMessages.Error_KeyboardFileNotValid({filename}));
      return DEFAULT_VERSION;
    }

    const store = kmx.stores.find(store => store.dwSystemID == KMX.KMXFile.TSS_KEYBOARDVERSION);
    if(!store) {
      // We have no version number store, so use default version
      this.callbacks.reportMessage(CompilerMessages.Warn_KeyboardFileHasNoKeyboardVersion({filename}));
      return DEFAULT_VERSION;
    }

    kmp.keyboards[0].version = store.dpString;
    return store.dpString;
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
  public buildKmpFile(kpsFilename: string, kmpJsonData: KmpJsonFile.KmpJsonFile): Promise<string> {
    const zip = JSZip();

    const kmpJsonFileName = 'kmp.json';

    // Make a copy of kmpJsonData, as we mutate paths for writing
    const data: KmpJsonFile.KmpJsonFile = JSON.parse(JSON.stringify(kmpJsonData));
    if(!data.files) {
      data.files = [];
    }

    let failed = false;
    data.files.forEach((value) => {
      // Get the path of the file
      let filename = value.name;

      // We add this separately after zipping all other files
      if(filename == 'kmp.json') {
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

    zip.file(kmpJsonFileName, JSON.stringify(data, null, 2));

    // Generate kmp file
    return zip.generateAsync({type: 'binarystring', compression:'DEFLATE'});
  }

  /**
   * Legacy .kmp compiler would transform xml-format .kvk files into a binary .kvk file; now
   * we want that to remain the responsibility of the keyboard compiler, so we'll warn the
   * few users who are still doing this
   */
  private warnIfKvkFileIsNotBinary(filename: string, data: Buffer) {
    // TODO: Buffer is not available on web
    if(filename.match(/\.kvk$/) && data.compare(Buffer.from(KvkFile.KVK_HEADER_IDENTIFIER_BYTES), 0, 3, 0, 3) != 0) {
      this.callbacks.reportMessage(CompilerMessages.Warn_FileIsNotABinaryKvkFile({filename: filename}));
    }
  }
}
