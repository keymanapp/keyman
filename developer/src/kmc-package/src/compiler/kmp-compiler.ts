/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import JSZip from 'jszip';

import { KpsFileReader } from '@keymanapp/developer-utils';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { KmpJsonFile, SchemaValidators, KeymanFileTypes, KvkFile } from '@keymanapp/common-types';
import { CompilerCallbacks, KpsFile, KeymanCompiler, CompilerOptions, KeymanCompilerResult, KeymanCompilerArtifacts, KeymanCompilerArtifact } from '@keymanapp/developer-utils';

import { PackageCompilerMessages } from './package-compiler-messages.js';
import { PackageMetadataCollector } from './package-metadata-collector.js';
import { KmpInfWriter } from './kmp-inf-writer.js';
import { transcodeToCP1252 } from './cp1252.js';
import { MIN_LM_FILEVERSION_KMP_JSON, PackageVersionValidator } from './package-version-validator.js';
import { PackageKeyboardTargetValidator } from './package-keyboard-target-validator.js';
import { PackageMetadataUpdater } from './package-metadata-updater.js';
import { markdownToHTML } from './markdown.js';
import { PackageValidation } from './package-validation.js';

const KMP_JSON_FILENAME = 'kmp.json';
const KMP_INF_FILENAME = 'kmp.inf';

// welcome.htm: this is a legacy filename, as of 17.0 the welcome
// (documentation) filename can be any file, but we will fallback to detecting
// this filename for existing keyboard packages.
const WELCOME_HTM_FILENAME = 'welcome.htm';

/**
 * @public
 * Options for the .kps compiler
 */
export interface KmpCompilerOptions extends CompilerOptions {
  // Note: WindowsPackageInstallerCompilerOptions extends KmpCompilerOptions, so
  // be careful when modifying this interface
};

/**
 * @public
 * Internal in-memory build artifacts from a successful compilation
 */
export interface KmpCompilerArtifacts extends KeymanCompilerArtifacts {
  /**
   * Binary keyboard package filedata and filename - installable into Keyman
   * desktop and mobile projects
   */
  kmp: KeymanCompilerArtifact;
};

/**
 * @public
 * Build artifacts from the .kps compiler
 */
export interface KmpCompilerResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link KmpCompiler.write}
   */
  artifacts: KmpCompilerArtifacts;
};

/**
 * @public
 * Compiles a .kps file to a .kmp archive. The compiler does not read or write
 * from filesystem or network directly, but relies on callbacks for all external
 * IO.
 */
export class KmpCompiler implements KeymanCompiler {
  private callbacks: CompilerCallbacks;
  private options: KmpCompilerOptions;

  /**
   * Initialize the compiler.
   * Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Compiler options
   * @returns false if initialization fails
   */
  public async init(callbacks: CompilerCallbacks, options: KmpCompilerOptions): Promise<boolean> {
    this.callbacks = callbacks;
    this.options = options ? {...options} : {};
    return true;
  }

  /**
   * Compiles a .kps file to .kmp file. Returns an object containing binary
   * artifacts on success. The files are passed in by name, and the compiler
   * will use callbacks as passed to the {@link KmpCompiler.init} function
   * to read any input files by disk.
   * @param infile  - Path to source file. Path will be parsed to find relative
   *                  references in the .kmn file, such as icon or On Screen
   *                  Keyboard file
   * @param outfile - Path to output file. The file will not be written to, but
   *                  will be included in the result for use by
   *                  {@link KmpCompiler.write}.
   * @returns         Binary artifacts on success, null on failure.
   */
  public async run(inputFilename: string, outputFilename?: string): Promise<KmpCompilerResult> {
    const kmpJsonData = this.transformKpsToKmpObject(inputFilename);
    if(!kmpJsonData) {
      return null;
    }

    //
    // Validate the package file
    //

    const validation = new PackageValidation(this.callbacks, this.options);
    if(!validation.validate(inputFilename, kmpJsonData)) {
      return null;
    }

    //
    // Build the .kmp package file
    //

    const data = await this.buildKmpFile(inputFilename, kmpJsonData);
    if(!data) {
      return null;
    }

    const result: KmpCompilerResult = {
      artifacts: {
        kmp: {
          data,
          filename: outputFilename ?? inputFilename.replace(/\.kps$/, '.kmp')
        }
      }
    }

    return result;
  }

  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written may include:
   *
   * - .kmp file - binary keyboard package used by Keyman on desktop and touch
   *   platforms
   *
   * @param artifacts - object containing artifact binary data to write out
   * @returns true on success
   */
  public async write(artifacts: KmpCompilerArtifacts): Promise<boolean> {
    this.callbacks.fs.writeFileSync(artifacts.kmp.filename, artifacts.kmp.data);
    return true;
  }

  /**
   * @internal
   */
  public transformKpsToKmpObject(kpsFilename: string): KmpJsonFile.KmpJsonFile {
    const reader = new KpsFileReader(this.callbacks);
    const data = this.callbacks.loadFile(kpsFilename);
    if(!data) {
      this.callbacks.reportMessage(PackageCompilerMessages.Error_FileDoesNotExist({filename: kpsFilename}));
      return null;
    }
    const kps = reader.read(data);
    if(!kps) {
      // errors will already have been reported by KpsFileReader
      return null;
    }
    const kmp = this.transformKpsFileToKmpObject(kpsFilename, kps.Package);
    if(!kmp) {
      return null;
    }

    // Verify that the generated kmp.json validates with the kmp.json schema
    if(!SchemaValidators.default.kmp(kmp)) {
      // This is an internal error, so throwing an exception is appropriate
      throw new Error(JSON.stringify((<any>SchemaValidators.default.kmp).errors));
    }

    return kmp;
  }

  readonly normalizePath = (path: string) => path || path === '' ? path.trim().replaceAll('\\','/') : undefined;

  /**
   * @internal
   */
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
      kmp.options.executeProgram = this.normalizePath(kps.Options.ExecuteProgram || undefined);
      kmp.options.graphicFile = this.normalizePath(kps.Options.GraphicFile || undefined);
      kmp.options.msiFilename = this.normalizePath(kps.Options.MSIFileName || undefined);
      kmp.options.msiOptions = kps.Options.MSIOptions || undefined;
      kmp.options.readmeFile = this.normalizePath(kps.Options.ReadMeFile || undefined);
      kmp.options.licenseFile = this.normalizePath(kps.Options.LicenseFile || undefined);
      kmp.options.welcomeFile = this.normalizePath(kps.Options.WelcomeFile || undefined);
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

    if(kps.RelatedPackages?.RelatedPackage?.length) {
      // Note: 'relationship' field is required for kmp.json but optional for .kps, only
      // two values are supported -- deprecates or related.
      kmp.relatedPackages = kps.RelatedPackages.RelatedPackage.map(p =>
        ({id: p.$.ID, relationship: p.$.Relationship == 'deprecates' ? 'deprecates' : 'related'})
      );
    }

    //
    // Add file metadata
    //

    if(kps.Files?.File?.length) {
      kmp.files = kps.Files.File.map((file: KpsFile.KpsFileContentFile) => {
        return {
          name: this.normalizePath(file.Name),
          description: (file.Description ?? '').trim(),
          copyLocation: parseInt(file.CopyLocation, 10) || undefined
          // note: we don't emit fileType as that is not permitted in kmp.json
        };
      });
      if(!kmp.files.reduce((result: boolean, file) => {
        if(!file.name) {
          // as the filename field is missing or blank, we'll try with the description instead
          this.callbacks.reportMessage(PackageCompilerMessages.Error_FileRecordIsMissingName({description: file.description ?? '(no description)'}));
          return false;
        }
        return result;
      }, true)) {
        return null;
      }
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

    if(kps.Keyboards?.Keyboard?.length) {
      kmp.keyboards = kps.Keyboards.Keyboard.map((keyboard: KpsFile.KpsFileKeyboard) => ({
        displayFont: keyboard.DisplayFont ? this.callbacks.path.basename(this.normalizePath(keyboard.DisplayFont)) : undefined,
        oskFont: keyboard.OSKFont ? this.callbacks.path.basename(this.normalizePath(keyboard.OSKFont)) : undefined,
        name:keyboard.Name?.trim(),
        id:keyboard.ID?.trim(),
        version:keyboard.Version?.trim(),
        rtl:keyboard.RTL == 'True' ? true : undefined,
        languages: keyboard.Languages?.Language?.length ?
          this.kpsLanguagesToKmpLanguages(keyboard.Languages.Language) :
          [],
        examples: keyboard.Examples?.Example?.length ?
          keyboard.Examples.Example.map(
            e => ({id: e.$.ID, keys: e.$.Keys, text: e.$.Text, note: e.$.Note})
          ) as KmpJsonFile.KmpJsonFileExample[] :
          undefined,
        webDisplayFonts: keyboard.WebDisplayFonts?.Font?.length ?
          keyboard.WebDisplayFonts.Font.map(
            e => (this.callbacks.path.basename(this.normalizePath(e.$.Filename)))
          ) :
          undefined,
        webOskFonts: keyboard.WebOSKFonts?.Font?.length ?
          keyboard.WebOSKFonts.Font.map(
            e => (this.callbacks.path.basename(this.normalizePath(e.$.Filename)))
          ) :
          undefined,
      }));
    }

    //
    // Add lexical-model metadata
    //

    if(kps.LexicalModels?.LexicalModel?.length) {
      kmp.lexicalModels = kps.LexicalModels.LexicalModel.map((model: KpsFile.KpsFileLexicalModel) => ({
        name:model.Name.trim(),
        id:model.ID.trim(),
        languages: model.Languages?.Language?.length ?
          this.kpsLanguagesToKmpLanguages(model.Languages.Language) : []
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
      if(kps.StartMenu?.Items?.Item?.length) {
        kmp.startMenu.items = kps.StartMenu.Items.Item.map((item: KpsFile.KpsFileStartMenuItem) => ({
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
   * @internal
   * Returns a Promise to the serialized data which can then be written to a .kmp file.
   *
   * @param kpsFilename - Filename of the kps, not read, used only for calculating relative paths
   * @param kmpJsonData - The kmp.json Object
   */
  public buildKmpFile(kpsFilename: string, kmpJsonData: KmpJsonFile.KmpJsonFile): Promise<Uint8Array> {
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
        this.callbacks.reportMessage(PackageCompilerMessages.Warn_AbsolutePath({filename: filename}));
      }

      filename = this.callbacks.resolveFilename(kpsFilename, filename);
      const basename = this.callbacks.path.basename(filename);

      if(!this.callbacks.fs.existsSync(filename)) {
        this.callbacks.reportMessage(PackageCompilerMessages.Error_FileDoesNotExist({filename: filename}));
        failed = true;
        return;
      }

      let memberFileData;
      try {
        memberFileData = this.callbacks.loadFile(filename);
      } catch(e) {
        this.callbacks.reportMessage(PackageCompilerMessages.Error_FileCouldNotBeRead({filename: filename, e: e}));
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
    return zip.generateAsync({type:'uint8array', compression:'DEFLATE'});
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
      this.callbacks.reportMessage(PackageCompilerMessages.Warn_FileIsNotABinaryKvkFile({filename: filename}));
    }
  }
}
