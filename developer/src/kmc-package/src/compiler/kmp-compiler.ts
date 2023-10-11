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
import { markdownToHTML } from './markdown.js';

const KMP_JSON_FILENAME = 'kmp.json';
const KMP_INF_FILENAME = 'kmp.inf';

// welcome.htm: this is a legacy filename, as of 17.0 the welcome
// (documentation) filename can be any file, but we will fallback to detecting
// this filename for existing keyboard packages.
const WELCOME_HTM_FILENAME = 'welcome.htm';

export class KmpCompiler {

  constructor(private callbacks: CompilerCallbacks) {
  }

  public transformKpsToKmpObject(kpsFilename: string): KmpJsonFile.KmpJsonFile {
    const kps = this.loadKpsFile(kpsFilename);
    return this.transformKpsFileToKmpObject(kpsFilename, kps);
  }

  public loadKpsFile(kpsFilename: string): KpsFile.KpsFile {
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
          explicitArray: false
        });
        // TODO: add unit test for xml errors parsing .kps file
        parser.parseString(data, (e: unknown, r: unknown) => { if(e) throw e; a = r as KpsFile.KpsPackage });
        return a;
    })();

    const kps: KpsFile.KpsFile = kpsPackage.Package;
    return kps;
  }

  public transformKpsFileToKmpObject(kpsFilename: string, kps: KpsFile.KpsFile): KmpJsonFile.KmpJsonFile {

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

    if(kps.Options) {
      kmp.options.executeProgram = kps.Options.ExecuteProgram || undefined;
      kmp.options.graphicFile = kps.Options.GraphicFile || undefined;
      kmp.options.msiFilename = kps.Options.MsiFileName || undefined;
      kmp.options.msiOptions = kps.Options.MsiOptions || undefined;
      kmp.options.readmeFile = kps.Options.ReadMeFile || undefined;
      kmp.options.licenseFile = kps.Options.LicenseFile || undefined;
      kmp.options.welcomeFile = kps.Options.WelcomeFile || undefined;
    }

    //
    // Add basic metadata
    //

    if(kps.Info) {
      kmp.info = this.kpsInfoToKmpInfo(kps.Info);
    }

    //
    // Add related package metadata
    //

    if(kps.RelatedPackages) {
      // Note: 'relationship' field is required for kmp.json but optional for .kps, only
      // two values are supported -- deprecates or related.
      kmp.relatedPackages = (this.arrayWrap(kps.RelatedPackages.RelatedPackage) as KpsFile.KpsFileRelatedPackage[]).map(p =>
        ({id: p.$.ID, relationship: p.$.Relationship == 'deprecates' ? 'deprecates' : 'related'})
      );
    }

    //
    // Add file metadata
    //

    if(kps.Files && kps.Files.File) {
      kmp.files = this.arrayWrap(kps.Files.File).map((file: KpsFile.KpsFileContentFile) => {
        return {
          name: file.Name.trim(),
          description: file.Description.trim(),
          copyLocation: parseInt(file.CopyLocation, 10) || undefined
          // note: we don't emit fileType as that is not permitted in kmp.json
        };
      });
    }
    kmp.files = kmp.files ?? [];

    // Keyboard packages also include a legacy kmp.inf file (this will be removed,
    // one day)
    if(kps.Keyboards && kps.Keyboards.Keyboard) {
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

    if(kps.Keyboards && kps.Keyboards.Keyboard) {
      kmp.keyboards = this.arrayWrap(kps.Keyboards.Keyboard).map((keyboard: KpsFile.KpsFileKeyboard) => ({
        displayFont: keyboard.DisplayFont ? this.callbacks.path.basename(keyboard.DisplayFont) : undefined,
        oskFont: keyboard.OSKFont ? this.callbacks.path.basename(keyboard.OSKFont) : undefined,
        name:keyboard.Name.trim(),
        id:keyboard.ID.trim(),
        version:keyboard.Version.trim(),
        rtl:keyboard.RTL == 'True' ? true : undefined,
        languages: keyboard.Languages ?
          this.kpsLanguagesToKmpLanguages(this.arrayWrap(keyboard.Languages.Language) as KpsFile.KpsFileLanguage[]) :
          [],
        examples: keyboard.Examples ?
          (this.arrayWrap(keyboard.Examples.Example) as KpsFile.KpsFileLanguageExample[]).map(
            e => ({id: e.$.ID, keys: e.$.Keys, text: e.$.Text, note: e.$.Note})
          ) as KmpJsonFile.KmpJsonFileExample[] :
          undefined,
        webDisplayFonts: keyboard.WebDisplayFonts ?
          (this.arrayWrap(keyboard.WebDisplayFonts.Font) as KpsFile.KpsFileFont[]).map(
            e => (this.callbacks.path.basename(e.$.Filename))
          ) :
          undefined,
        webOskFonts: keyboard.WebOSKFonts ?
          (this.arrayWrap(keyboard.WebOSKFonts.Font) as KpsFile.KpsFileFont[]).map(
            e => (this.callbacks.path.basename(e.$.Filename))
          ) :
          undefined,
      }));
    }

    //
    // Add lexical-model metadata
    //

    if(kps.LexicalModels && kps.LexicalModels.LexicalModel) {
      kmp.lexicalModels = this.arrayWrap(kps.LexicalModels.LexicalModel).map((model: KpsFile.KpsFileLexicalModel) => ({
        name:model.Name.trim(),
        id:model.ID.trim(),
        languages: model.Languages ?
          this.kpsLanguagesToKmpLanguages(this.arrayWrap(model.Languages.Language) as KpsFile.KpsFileLanguage[]) : []
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

    if(kps.Keyboards && kps.Keyboards.Keyboard) {
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

    if(kps.StartMenu && (kps.StartMenu.Folder || kps.StartMenu.Items)) {
      kmp.startMenu = {};
      if(kps.StartMenu.AddUninstallEntry === '') kmp.startMenu.addUninstallEntry = true;
      if(kps.StartMenu.Folder) kmp.startMenu.folder = kps.StartMenu.Folder;
      if(kps.StartMenu.Items && kps.StartMenu.Items.Item) {
        kmp.startMenu.items = this.arrayWrap(kps.StartMenu.Items.Item).map((item: KpsFile.KpsFileStartMenuItem) => ({
          filename: item.FileName,
          name: item.Name,
          arguments: item.Arguments,
          icon: item.Icon,
          location: item.Location
        }));

        // Remove default values
        for(let item of kmp.startMenu.items) {
          if(item.icon == '') delete item.icon;
          if(item.location == 'psmelStartMenu') delete item.location;
          if(item.arguments == '') delete item.arguments;
        }
      } else {
        kmp.startMenu.items = [];
      }
    }

    kmp = this.stripUndefined(kmp) as KmpJsonFile.KmpJsonFile;

    return kmp;
  }

    // Helper functions

  private kpsInfoToKmpInfo(kpsInfo: KpsFile.KpsFileInfo): KmpJsonFile.KmpJsonFileInfo {
    let kmpInfo: KmpJsonFile.KmpJsonFileInfo = {};

    const keys: [(keyof KpsFile.KpsFileInfo), (keyof KmpJsonFile.KmpJsonFileInfo), boolean][] = [
      ['Author','author',false],
      ['Copyright','copyright',false],
      ['Name','name',false],
      ['Version','version',false],
      ['WebSite','website',false],
      ['Description','description',true],
    ];

    for (let [src,dst,isMarkdown] of keys) {
      if (kpsInfo[src]) {
        kmpInfo[dst] = {
          description: (kpsInfo[src]._ ?? (typeof kpsInfo[src] == 'string' ? kpsInfo[src].toString() : '')).trim()
        };
        if(isMarkdown) {
          kmpInfo[dst].description = markdownToHTML(kmpInfo[dst].description, false).trim();
        }
        if(kpsInfo[src].$?.URL) {
          kmpInfo[dst].url = kpsInfo[src].$.URL.trim();
        }
      }
    }

    return kmpInfo;
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

    // TODO #9477: transform .md to .htm

    // Remove path data from file references in options

    if(data.options.graphicFile) {
      data.options.graphicFile = this.callbacks.path.basename(data.options.graphicFile);
    }
    if(data.options.readmeFile) {
      data.options.readmeFile = this.callbacks.path.basename(data.options.readmeFile);
    }
    if(data.options.licenseFile) {
      data.options.licenseFile = this.callbacks.path.basename(data.options.licenseFile);
    }
    if(data.options.welcomeFile) {
      data.options.welcomeFile = this.callbacks.path.basename(data.options.welcomeFile);
    } else if(data.files.find(file => file.name == WELCOME_HTM_FILENAME)) {
      // We will, for improved backward-compatibility with existing packages, add a
      // reference to the file welcome.htm is it is present in the package. This allows
      // newer tools to avoid knowing about welcome.htm, if we assume that they work with
      // packages compiled with kmc-package (17.0+) and not kmcomp (5.x-16.x).
      data.options.welcomeFile = WELCOME_HTM_FILENAME;
    }

    if(data.options.msiFilename) {
      data.options.msiFilename = this.callbacks.path.basename(data.options.msiFilename);
    }

    // Write kmp.json and kmp.inf

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
