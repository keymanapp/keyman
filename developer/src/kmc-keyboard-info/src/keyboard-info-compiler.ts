/**
 * Merges a source .keyboard_info file with metadata extracted from .kps file and
 * compiled files to produce a comprehensive .keyboard_info file.
 */

import { minKeymanVersion } from "./min-keyman-version.js";
import { KeyboardInfoFile, KeyboardInfoFileIncludes, KeyboardInfoFileLanguageFont, KeyboardInfoFilePlatform } from "./keyboard-info-file.js";
import { KeymanFileTypes, KmpJsonFile, KmxFileReader, KMX, KeymanTargets } from "@keymanapp/common-types";
import { KeyboardInfoCompilerMessages } from "./keyboard-info-compiler-messages.js";
import langtags from "./imports/langtags.js";
import { CompilerCallbacks, KeymanCompiler, CompilerOptions, KeymanCompilerResult, KeymanCompilerArtifacts, KeymanCompilerArtifact, KeymanUrls, isValidEmail, validateMITLicense } from "@keymanapp/developer-utils";
import { KmpCompiler } from "@keymanapp/kmc-package";

import { SchemaValidators } from "@keymanapp/common-types";
import { getFontFamily } from "./font-family.js";

const regionNames = new Intl.DisplayNames(['en'], { type: "region" });
const scriptNames = new Intl.DisplayNames(['en'], { type: "script" });
const langtagsByTag = {};

/**
 * Build a dictionary of language tags from langtags.json
 */

function preinit(): void {
  if(langtagsByTag['en']) {
    // Already initialized, we can reasonably assume that 'en' will always be in
    // langtags.json.
    return;
  }

  for(const tag of langtags) {
    langtagsByTag[tag.tag] = tag;
    langtagsByTag[tag.full] = tag;
    if(tag.tags) {
      for(const t of tag.tags) {
        langtagsByTag[t] = tag;
      }
    }
  }
}

/**
 * @public
 * Description of sources and metadata required to build a .keyboard_info file
 */
export interface KeyboardInfoSources {
  /** The path in the keymanapp/keyboards repo where this keyboard may be found */
  sourcePath: string;

  /** The compiled keyboard filename and relative path (.js only) */
  jsFilename?: string;

  /** The compiled package filename and relative path (.kmp) */
  kmpFilename: string;

  /** The source package filename and relative path (.kps) */
  kpsFilename: string;

  /** Last modification date for files in the project folder 'YYYY-MM-DDThh:mm:ssZ' */
  lastCommitDate?: string;

  /** Return an error if project does not meet requirements of keyboards repository */
  forPublishing: boolean;
};

/**
 * @public
 * Options for the .keyboard_info compiler
 */
export interface KeyboardInfoCompilerOptions extends CompilerOptions {
  /**
   * Description of sources and metadata required to build a .keyboard_info file
   */
  sources: KeyboardInfoSources;
};

/**
 * @public
 * Internal in-memory build artifacts from a successful compilation
 */
export interface KeyboardInfoCompilerArtifacts extends KeymanCompilerArtifacts {
  /**
   * Binary keyboard info filedata and filename - used by keyman.com
   */
  keyboard_info: KeymanCompilerArtifact;
};

/**
 * @public
 * Build artifacts from the .keyboard_info compiler
 */
export interface KeyboardInfoCompilerResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link KeyboardInfoCompiler.write}
   */
  artifacts: KeyboardInfoCompilerArtifacts;
};

/**
 * @public
 * Compiles source data from a keyboard project to a .keyboard_info. The
 * compiler does not read or write from filesystem or network directly, but
 * relies on callbacks for all external IO.
 */
export class KeyboardInfoCompiler implements KeymanCompiler {
  private callbacks: CompilerCallbacks;
  private options: KeyboardInfoCompilerOptions;

  constructor() {
    preinit();
  }

  /**
   * Initialize the compiler.
   * Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Compiler options
   * @returns false if initialization fails
   */
  public async init(callbacks: CompilerCallbacks, options: KeyboardInfoCompilerOptions): Promise<boolean> {
    this.callbacks = callbacks;
    this.options = {...options};
    return true;
  }

  /**
   * @public
   * Builds a .keyboard_info file with metadata from the keyboard and package
   * source file. Returns an object containing binary artifacts on success. The
   * files are passed in by name, and the compiler will use callbacks as passed
   * to the {@link KeyboardInfoCompiler.init} function to read any input files
   * by disk.
   *
   * This function is intended for use within the keyboards repository. While
   * many of the parameters could be deduced from each other, they are specified
   * here to reduce the number of places the filenames are constructed. For full
   * documentation, see: https://help.keyman.com/developer/cloud/keyboard_info/
   *
   * @param infile  - Path to source file. Path will be parsed to find relative
   *                  references in the .kpj file, such as .model.ts or
   *                  .model.kps file
   * @param outfile - Path to output file. The file will not be written to, but
   *                  will be included in the result for use by
   *                  {@link KeyboardInfoCompiler.write}.
   * @returns         Binary artifacts on success, null on failure.
   */
  public async run(inputFilename: string, outputFilename?: string): Promise<KeyboardInfoCompilerResult> {
    const sources = this.options.sources;

    // TODO(lowpri): work from .kpj and nothing else as input. Blocked because
    // .kpj work is largely in kmc at present, so that would need to move to
    // a separate module.

    const kmpCompiler = new KmpCompiler();
    if(!await kmpCompiler.init(this.callbacks, {})) {
      // Errors will have been emitted by KmpCompiler
      return null;
    }
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(sources.kpsFilename);
    if(!kmpJsonData) {
      // Errors will have been emitted by KmpCompiler
      return null;
    }

    if(!sources.kmpFilename) {
      // We can't build any metadata without a .kmp file
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_CannotBuildWithoutKmpFile());
      return null;
    }

    const keyboard_info: KeyboardInfoFile = {};

    let jsFile: string = null;

    if(sources.jsFilename) {
      jsFile = this.loadJsFile(sources.jsFilename);
      if(!jsFile) {
         return null;
      }
    }

    const kmxFiles: {
      filename: string,
      data: KMX.KEYBOARD
    }[] = this.loadKmxFiles(sources.kpsFilename, kmpJsonData);

    //
    // Build .keyboard_info file
    // https://api.keyman.com/schemas/keyboard_info.schema.json
    // https://help.keyman.com/developer/cloud/keyboard_info/2.0
    //

    keyboard_info.id = this.callbacks.path.basename(sources.kmpFilename, '.kmp');
    keyboard_info.name = kmpJsonData.info.name.description;

    // License

    if(sources.forPublishing) {
      // We will only verify the license if asked to do so, so that all keyboard
      // projects can be built even if license is not present. Keyboards
      // repository will always verify license
      if(!kmpJsonData.options?.licenseFile) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_NoLicenseFound());
        return null;
      }

      if(!this.isLicenseMIT(this.callbacks.resolveFilename(sources.kpsFilename, kmpJsonData.options.licenseFile))) {
        return null;
      }
    }

    // Even if license is not verified, we set the .keyboard_info license to
    // 'mit' to meet the schema requirements. The .keyboard_info file is only
    // used by the keyboards repository, so this is a fair assumption to make.
    keyboard_info.license = 'mit';

    // isRTL

    if(jsFile?.match(/this\.KRTL=1/)) {
      keyboard_info.isRTL = true;
    }

    // author

    const author = kmpJsonData.info.author;
    if(author?.description || author?.url) {
      keyboard_info.authorName = author.description;

      if (author.url) {
        // we strip the mailto: from the .kps file for the .keyboard_info
        const match = author.url.match(/^(mailto\:)?(.+)$/);
        /* c8 ignore next 4 */
        if (match === null) {
          this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
          return null;
        }

        if(!isValidEmail(match[2])) {
          this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
          return null;
        }

        keyboard_info.authorEmail = match[2];
      }
    }

    // description

    if(kmpJsonData.info.description?.description) {
      keyboard_info.description = kmpJsonData.info.description.description.trim();
    } else {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_DescriptionIsMissing({filename:sources.kpsFilename}));
      return null;
    }

    // extract the language identifiers from the language metadata arrays for
    // each of the keyboards in the kmp.json file, and merge into a single array
    // of identifiers in the .keyboard_info file.

    if(!await this.fillLanguages(sources.kpsFilename, keyboard_info, kmpJsonData)) {
      return null;
    }

    // If a last commit date is not given, then just use the current time
    keyboard_info.lastModifiedDate = sources.lastCommitDate ?? (new Date).toISOString();

    keyboard_info.packageFilename = this.callbacks.path.basename(sources.kmpFilename);

    // Always overwrite with actual file size
    if(!this.callbacks.fs.existsSync(sources.kmpFilename)) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename:sources.kmpFilename}));
      return null;
    }
    keyboard_info.packageFileSize = this.callbacks.fileSize(sources.kmpFilename);

    if(sources.jsFilename) {
      keyboard_info.jsFilename = this.callbacks.path.basename(sources.jsFilename);
      // Always overwrite with actual file size
      /* c8 ignore next 5 */
      keyboard_info.jsFileSize = this.callbacks.fileSize(sources.jsFilename);
      if(keyboard_info.jsFileSize === undefined) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename:sources.jsFilename}));
        return null;
      }
    }

    const includes = new Set<KeyboardInfoFileIncludes>();
    keyboard_info.packageIncludes = [];
    for(const file of kmpJsonData.files) {
      if(file.name.match(/\.(otf|ttf|ttc)$/)) {
        includes.add('fonts');
      } else if(file.name.match(/welcome\.htm$/)) {
        includes.add('welcome');
      } else if(file.name.match(/\.kvk$/)) {
        includes.add('visualKeyboard');
      } else if(file.name.match(/\.(rtf|html|htm|pdf)$/)) {
        includes.add('documentation');
      }
    }
    keyboard_info.packageIncludes = [...includes];

    keyboard_info.version = kmpJsonData.info?.version?.description ?? '1.0';

    let minVersion = minKeymanVersion;
    const m = jsFile?.match(/this.KMINVER\s*=\s*(['"])(.*?)\1/);
    if(m) {
      if(parseFloat(m[2]) > parseFloat(minVersion)) {
        minVersion = m[2];
      }
    }

    for(const file of kmxFiles) {
      const v = this.kmxFileVersionToString(file.data.fileVersion);
      if(parseFloat(v) > parseFloat(minVersion)) {
        minVersion = v;
      }
    }

    // Only legacy keyboards supprt non-Unicode encodings, and we no longer
    // rewrite the .keyboard_info for those.
    keyboard_info.encodings = ['unicode'];

    // platformSupport
    const platforms = new Set<KeyboardInfoFilePlatform>();
    for(const file of kmxFiles) {
      const targets = KeymanTargets.keymanTargetsFromString(file.data.targets, {expandTargets: true});
      for(const target of targets) {
        this.mapKeymanTargetToPlatform(target).forEach(platform => platforms.add(platform));
      }
    }

    if(jsFile) {
      if(platforms.size == 0) {
        // In this case, there was no .kmx metadata available. We need to
        // make an assumption that this keyboard is both desktop+mobile web,
        // and if the .js is in the package, that it is mobile native as well,
        // because the targets metadata is not available in the .js.
        platforms.add('mobileWeb').add('desktopWeb');
        if(kmpJsonData.files.find(file => file.name.match(/\.js$/))) {
          platforms.add('android').add('ios');
        }
      }
      // Special case for determining desktopWeb and mobileWeb support: we use
      // &targets to determine which platforms the .js is actually compatible
      // with. The presence of the .js file itself determines whether there is
      // supposed to be any web support. The presence of the .js file in the
      // package (which is a separate check) does not determine whether or not
      // the keyboard itself actually supports mobile, although it must be
      // included in the package in order to actually be delivered to mobile
      // apps.
      if(platforms.has('android') || platforms.has('ios')) {
        platforms.add('mobileWeb');
      }
      if(platforms.has('linux') || platforms.has('macos') || platforms.has('windows')) {
        platforms.add('desktopWeb');
      }
    }

    keyboard_info.platformSupport = {};
    for(const platform of platforms) {
      keyboard_info.platformSupport[platform] = 'full';
    }

    keyboard_info.minKeymanVersion = minVersion;
    keyboard_info.sourcePath = sources.sourcePath;
    keyboard_info.helpLink = KeymanUrls.HELP_KEYBOARD(keyboard_info.id);

    // Related packages
    if(kmpJsonData.relatedPackages?.length) {
      keyboard_info.related = {};
      for(const p of kmpJsonData.relatedPackages) {
        keyboard_info.related[p.id] = {
          deprecates: p.relationship == 'deprecates'
        };
      }
    }

    const jsonOutput = JSON.stringify(keyboard_info, null, 2);

    /* c8 ignore next 8 */
    if(!SchemaValidators.default.keyboard_info(keyboard_info)) {
      // This is an internal fatal error; we should not be capable of producing
      // invalid output, so it is best to throw and die
      throw new Error(JSON.stringify({
        keyboard_info: keyboard_info,
        error: SchemaValidators.default.keyboard_info.errors
      }, null, 2));
    }

    const data = new TextEncoder().encode(jsonOutput);
    const result: KeyboardInfoCompilerResult = {
      artifacts: {
        keyboard_info: {
          data,
          filename: outputFilename ?? inputFilename.replace(/\.kpj$/, '.keyboard_info')
        }
      }
    };

    return result;
  }

  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written may include:
   *
   * - .keyboard_info file - metadata file used by keyman.com
   *
   * @param artifacts - object containing artifact binary data to write out
   * @returns true on success
   */
  public async write(artifacts: KeyboardInfoCompilerArtifacts): Promise<boolean> {
    this.callbacks.fs.writeFileSync(artifacts.keyboard_info.filename, artifacts.keyboard_info.data);
    return true;
  }

  private mapKeymanTargetToPlatform(target: KeymanTargets.KeymanTarget): KeyboardInfoFilePlatform[] {
    const map: {[index in KeymanTargets.KeymanTarget]: KeyboardInfoFilePlatform[]} = {
      any: [], // unused
      androidphone: ['android'],
      androidtablet: ['android'],
      desktop: [], // unused
      ipad: ['ios'],
      iphone: ['ios'],
      linux: ['linux'],
      macosx: ['macos'],
      mobile: [], // unused
      tablet: [], // unused
      web: ['desktopWeb'],  // note: assuming that web == desktopWeb but not necessarily mobileWeb, for historical reasons
      windows: ['windows']
    }
    return map[target] ?? [];
  }

  private kmxFileVersionToString(version: number) {
    return ((version & 0xFF00) >> 8).toString() + '.' + (version & 0xFF).toString();
  }

  private isLicenseMIT(filename: string) {
    const data = this.callbacks.loadFile(filename);
    if(!data) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_LicenseFileIsMissing({filename}));
      return false;
    }

    let license = null;
    try {
      license = new TextDecoder().decode(data);
    } catch(e) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_LicenseFileIsDamaged({filename}));
      return false;
    }
    if(!license) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_LicenseFileIsDamaged({filename}));
      return false;
    }
    const message = validateMITLicense(license);
    if(message != null) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_LicenseIsNotValid({filename, message}));
      return false;
    }
    return true;
  }

  private loadKmxFiles(kpsFilename: string, kmpJsonData: KmpJsonFile.KmpJsonFile) {
    const reader = new KmxFileReader();
    return kmpJsonData.files
      .filter(file => KeymanFileTypes.filenameIs(file.name, KeymanFileTypes.Binary.Keyboard))
      .map(file => ({
        filename: this.callbacks.path.basename(file.name),
        data: reader.read(this.callbacks.loadFile(this.callbacks.resolveFilename(kpsFilename, file.name)))
      })
    );
  }

  private loadJsFile(filename: string) {
    const data = this.callbacks.loadFile(filename);
    if(!data) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename}));
      return null;
    }
    const text = new TextDecoder('utf-8', {fatal: true}).decode(data);
    return text;
  }

  private async fillLanguages(kpsFilename: string, keyboard_info: KeyboardInfoFile, kmpJsonData:  KmpJsonFile.KmpJsonFile): Promise<boolean> {
    // Collapse language data from multiple keyboards
    const languages =
      kmpJsonData.keyboards.reduce((a, e) => [].concat(a, (e.languages ?? []).map((f) => f.id)), []);
    const examples: KmpJsonFile.KmpJsonFileExample[] =
      kmpJsonData.keyboards.reduce((a, e) => [].concat(a, e.examples ?? []), []);

    // Transform array into object
    keyboard_info.languages = {};
    for(const language of languages) {
      keyboard_info.languages[language] = {};
    }

    let commonScript = null;

    for(const bcp47 of Object.keys(keyboard_info.languages)) {
      const language = keyboard_info.languages[bcp47];

      //
      // Add examples
      //
      language.examples = [];
      for(const example of examples) {
        if(example.id == bcp47) {
          language.examples.push({
            // we don't copy over example.id
            keys: example.keys,
            note: example.note,
            text: example.text
          });
        }
      }

      //
      // Add fonts -- which are duplicated for each language; we'll mark this as a future
      // optimization, but it's another keyboard_info breaking change so don't want to
      // do it right now.
      //

      // The code below:
      // 1. Only includes fonts associated with keyboards which support the current bcp47 (filter)
      // 2. Joins the displayFont and webDisplayFonts data, and removes duplicates (...new Set())

      const supportedKeyboards = kmpJsonData.keyboards.filter(k => k.languages.find(lang => lang.id == bcp47));
      const fontSource = [...new Set([].concat(
        ...supportedKeyboards.map(e => e.displayFont ? [e.displayFont] : []),
        ...supportedKeyboards.map(e => e.webDisplayFonts ?? [])
      ))];
      const oskFontSource = [...new Set([].concat(
        ...supportedKeyboards.map(e => e.oskFont ? [e.oskFont] : []),
        ...supportedKeyboards.map(e => e.webOskFonts ?? [])
      ))];

      if(fontSource.length) {
        language.font = await this.fontSourceToKeyboardInfoFont(kpsFilename, kmpJsonData, fontSource);
        if(language.font == null) {
          return false;
        }
      }

      if(oskFontSource.length) {
        language.oskFont = await this.fontSourceToKeyboardInfoFont(kpsFilename, kmpJsonData, oskFontSource);
        if(language.oskFont == null) {
          return false;
        }
      }

      //
      // Add locale description
      //
      const locale = new Intl.Locale(bcp47);
      // DisplayNames.prototype.of will throw a RangeError if it doesn't understand
      // the format of the bcp47 tag. This happens with Node 18.14.1, for example, with:
      //   new Intl.DisplayNames(['en'], {type: 'language'}).of('und-fonipa');
      const mapName = (code: string, dict: Intl.DisplayNames) => {
        try {
           return dict.of(code);
        } catch(e) {
          if(e instanceof RangeError) {
            return code;
          } else {
            throw e;
          }
        }
      }
      const tag = langtagsByTag[bcp47] ?? langtagsByTag[locale.language];
      language.languageName = tag ? tag.name : bcp47;
      language.regionName = mapName(locale.region, regionNames);
      language.scriptName = mapName(locale.script, scriptNames);

      language.displayName = language.languageName + (
        (language.scriptName && language.regionName) ?
        ` (${language.scriptName}, ${language.regionName})` :
        language.scriptName ?
        ` (${language.scriptName})` :
        language.regionName ?
        ` (${language.regionName})` :
        ''
      );

      const resolvedScript = locale.script ?? langtagsByTag[bcp47]?.script ?? langtagsByTag[locale.language]?.script ?? undefined;
      if(commonScript === null) {
        commonScript = resolvedScript;
      } else {
        if(resolvedScript !== commonScript) {
          this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Hint_ScriptDoesNotMatch({commonScript, bcp47, script: resolvedScript}))
        }
      }
    }
    return true;
  }

  /**
   * @internal
   */
  async fontSourceToKeyboardInfoFont(kpsFilename: string, kmpJsonData: KmpJsonFile.KmpJsonFile, source: string[]): Promise<KeyboardInfoFileLanguageFont> {
    // locate a .ttf, .otf, or .woff font file
    const ttf = source.find(file => file.endsWith('.ttf') || file.endsWith('.otf') || file.endsWith('.woff'));
    if(!ttf) {
      return {
        // If we can't find a matching font, we'll just use the filename of the first font
        family: this.callbacks.path.basename(source[0]),
        source: source
      }
    }

    // The font sources already have path information stripped, but we can
    // find the matching file from the list of files in the package.
    const sourcePath = kmpJsonData.files.find(file => ('/' + file.name.replaceAll('\\', '/')).endsWith('/'+ttf))?.name;
    if(!sourcePath) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename: ttf}));
      return null;
    }

    const fontData = this.callbacks.loadFile(this.callbacks.resolveFilename(kpsFilename, sourcePath));
    if(!fontData) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename: sourcePath}));
      return null;
    }

    let fontFamily = null;
    try {
      fontFamily = await getFontFamily(fontData)
    } catch(e) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FontFileMetaDataIsInvalid({filename: sourcePath,message: e}));
      return null;
    }

    const result = {
      family: fontFamily,
      source
    };

    /* c8 ignore next 4 */
    if(!result.family) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FontFileCannotBeRead({filename: sourcePath}));
      return null;
    }
    return result;
  }

}

/**
 * these are exported only for unit tests, do not use
 */
export const unitTestEndpoints = {
  langtagsByTag,
};