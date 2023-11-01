/**
 * Merges a source .keyboard_info file with metadata extracted from .kps file and
 * compiled files to produce a comprehensive .keyboard_info file.
 */

import { minKeymanVersion } from "./min-keyman-version.js";
import { KeyboardInfoFile, KeyboardInfoFileIncludes, KeyboardInfoFilePlatform } from "./keyboard-info-file.js";
import { KeymanFileTypes, CompilerCallbacks, KmpJsonFile, KmxFileReader, KMX, KeymanTargets } from "@keymanapp/common-types";
import { KeyboardInfoCompilerMessages } from "./messages.js";
import langtags from "./imports/langtags.js";
import { validateMITLicense } from "@keymanapp/developer-utils";
import { KmpCompiler } from "@keymanapp/kmc-package";

import { SchemaValidators } from "@keymanapp/common-types";

const regionNames = new Intl.DisplayNames(['en'], { type: "region" });
const scriptNames = new Intl.DisplayNames(['en'], { type: "script" });
const langtagsByTag = {};

const HelpRoot = 'https://help.keyman.com/keyboard/';

/**
 * Build a dictionary of language tags from langtags.json
 */

function init(): void {
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

export class KeyboardInfoCompiler {
  constructor(private callbacks: CompilerCallbacks) {
    init();
  }

  /**
   * Builds a .keyboard_info file with metadata from the keyboard and package source file.
   * This function is intended for use within the keyboards repository. While many of the
   * parameters could be deduced from each other, they are specified here to reduce the
   * number of places the filenames are constructed.
   * For full documentation, see:
   * https://help.keyman.com/developer/cloud/keyboard_info/
   *
   * @param sources                     Details on files from which to extract metadata
   */
  public writeKeyboardInfoFile(
    sources: KeyboardInfoSources
  ): Uint8Array {

    // TODO(lowpri): work from .kpj and nothing else as input. Blocked because
    // .kpj work is largely in kmc at present, so that would need to move to
    // a separate module.

    const kmpCompiler = new KmpCompiler(this.callbacks);
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
        /* c8 ignore next 3 */
        if (match === null) {
          this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
          return null;
        }

        keyboard_info.authorEmail = match[2];
      }
    }

    // description

    if(kmpJsonData.info.description?.description) {
      keyboard_info.description = kmpJsonData.info.description.description.trim();
    }

    // extract the language identifiers from the language metadata arrays for
    // each of the keyboards in the kmp.json file, and merge into a single array
    // of identifiers in the .keyboard_info file.

    this.fillLanguages(keyboard_info, kmpJsonData);

    // If a last commit date is not given, then just use the current time
    keyboard_info.lastModifiedDate = sources.lastCommitDate ?? (new Date).toISOString();

    keyboard_info.packageFilename = this.callbacks.path.basename(sources.kmpFilename);

    // Always overwrite with actual file size
    keyboard_info.packageFileSize = this.callbacks.fileSize(sources.kmpFilename);
    if(keyboard_info.packageFileSize === undefined) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename:sources.kmpFilename}));
      return null;
    }

    if(sources.jsFilename) {
      keyboard_info.jsFilename = this.callbacks.path.basename(sources.jsFilename);
      // Always overwrite with actual file size
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

    keyboard_info.version = kmpJsonData.info.version.description;

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
    keyboard_info.helpLink = HelpRoot + keyboard_info.id;

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

    if(!SchemaValidators.default.keyboard_info(keyboard_info)) {
      // This is an internal fatal error; we should not be capable of producing
      // invalid output, so it is best to throw and die
      throw new Error(JSON.stringify({
        keyboard_info: keyboard_info,
        error: SchemaValidators.default.keyboard_info.errors
      }, null, 2));
    }

    return new TextEncoder().encode(jsonOutput);
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
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_LicenseFileDoesNotExist({filename}));
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

  private fillLanguages(keyboard_info: KeyboardInfoFile, kmpJsonData:  KmpJsonFile.KmpJsonFile) {
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

    const fontSource = [].concat(...kmpJsonData.keyboards.map(e => e.displayFont ? [e.displayFont] : []), ...kmpJsonData.keyboards.map(e => e.webDisplayFonts ?? []));
    const oskFontSource = [].concat(...kmpJsonData.keyboards.map(e => e.oskFont ? [e.oskFont] : []), ...kmpJsonData.keyboards.map(e => e.webOskFonts ?? []));

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

      if(fontSource.length) {
        language.font = {
          family: keyboard_info.id + ' Keyman Display Font',
          source: fontSource
        };
      }

      if(oskFontSource.length) {
        language.oskFont = {
          family: keyboard_info.id + ' Keyman OSK Font',
          source: oskFontSource
        };
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
    }
  }
}

