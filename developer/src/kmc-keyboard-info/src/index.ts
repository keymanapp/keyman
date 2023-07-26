/**
 * Merges a source .keyboard_info file with metadata extracted from .kps file and
 * compiled files to produce a comprehensive .keyboard_info file.
 */

import { minKeymanVersion } from "./min-keyman-version.js";
import { KeyboardInfoFile, KeyboardInfoFileIncludes, KeyboardInfoFileLanguage, KeyboardInfoFilePlatform } from "./keyboard-info-file.js";
import { KeymanFileTypes, CompilerCallbacks, KmpJsonFile, KmxFileReader, KMX, KeymanTargets } from "@keymanapp/common-types";
import { KeyboardInfoCompilerMessages } from "./messages.js";
import langtags from "./imports/langtags.js";

const regionNames = new Intl.DisplayNames(['en'], { type: "region" });
const scriptNames = new Intl.DisplayNames(['en'], { type: "script" });
const langtagsByTag = {};

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

/* c8 ignore start */
export class KeyboardInfoOptions {
  /** The identifier for the keyboard */
  keyboard_id: string;

  /** The data from the .kps file, transformed to kmp.json */
  kmpJsonData: KmpJsonFile.KmpJsonFile;

  /** The path in the keymanapp/keyboards repo where this keyboard may be found (optional) */
  sourcePath?: string;

  /** The full URL to the keyboard help, starting with https://help.keyman.com/keyboard/ (optional) */
  helpLink?: string;

  /** The compiled keyboard filename and relative path (.js only) */
  keyboardFileNameJs?: string;

  /** The compiled package filename and relative path (.kmp) */
  kmpFileName: string;

  /** The source package filename and relative path (.kps) */
  kpsFileName: string;
};
/* c8 ignore stop */

export class KeyboardInfoCompiler {
  constructor(private callbacks: CompilerCallbacks) {
    init();
  }

  /**
   * Merges source .keyboard_info file with metadata from the keyboard and package source file.
   * This function is intended for use within the keyboards repository. While many of the
   * parameters could be deduced from each other, they are specified here to reduce the
   * number of places the filenames are constructed.
   * For full documentation, see:
   * https://help.keyman.com/developer/cloud/keyboard_info/
   *
   * @param sourceKeyboardInfoFileName  Path for the source .keyboard_info file
   * @param options                     Details on files from which to extract additional metadata
   */
  public writeMergedKeyboardInfoFile(
    sourceKeyboardInfoFileName: string,
    options: KeyboardInfoOptions
  ): Uint8Array {
    let keyboard_info: KeyboardInfoFile = null;

    if(this.callbacks.fs.existsSync(sourceKeyboardInfoFileName)) {
      const dataInput = this.callbacks.loadFile(sourceKeyboardInfoFileName);
      if(!dataInput) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename: sourceKeyboardInfoFileName}));
        return null;
      }

      try {
        const jsonInput = new TextDecoder('utf-8', {fatal: true}).decode(dataInput);
        keyboard_info = JSON.parse(jsonInput);
      } catch(e) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileIsNotValid({filename: sourceKeyboardInfoFileName, e}));
        return null;
      }
    } else {
      // TODO: this code pathway is still under development and won't yet be
      // called by kmc. Building metadata without a source .keyboard_info file
      // is on the roadmap but more data is needed in the .kps to complete this.
      if(!options.kmpFileName) {
        // We can't build any metadata without a .kmp file
        return null;
      }
      keyboard_info = {
        // Only two fields that won't be automatically filled if file is not
        // present
        encodings: ['unicode'],
        license: 'mit'
      };
    }

    let jsFile: string = null;

    if(options.keyboardFileNameJs) {
      jsFile = this.loadJsFile(options.keyboardFileNameJs);
      if(!jsFile) {
         return null;
      }
    }

    const kmxFiles: {
      filename: string,
      data: KMX.KEYBOARD
    }[] = this.loadKmxFiles(options.kpsFileName, options.kmpJsonData);

    //
    // Build merged .keyboard_info file
    // https://api.keyman.com/schemas/keyboard_info.source.json and
    // https://api.keyman.com/schemas/keyboard_info.distribution.json
    // https://help.keyman.com/developer/cloud/keyboard_info/1.0
    //

    keyboard_info.isRTL = keyboard_info.isRTL ?? !!jsFile?.match(/this\.KRTL=1/);
    if(!keyboard_info.isRTL) {
      delete keyboard_info.isRTL;
    }

    this.setField(keyboard_info, 'id', options.keyboard_id);

    this.setField(keyboard_info, 'name', options.kmpJsonData.info.name.description);

    const author = options.kmpJsonData.info.author;
    if(author && (author.description || author.url)) {
      this.setField(keyboard_info, 'authorName', author?.description);

      if (author?.url) {
        // we strip the mailto: from the .kps file for the .keyboard_info
        const match = author.url.match(/^(mailto\:)?(.+)$/);
        /* c8 ignore next 3 */
        if (match === null) {
          this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
          return null;
        }

        const email = match[2];
        this.setField(keyboard_info, 'authorEmail', email, false);
      }
    }

    // extract the language identifiers from the language metadata arrays for
    // each of the keyboards in the kmp.json file, and merge into a single array
    // of identifiers in the .keyboard_info file.

    this.fillLanguages(keyboard_info, options.kmpJsonData);

    this.setField(keyboard_info, 'lastModifiedDate', (new Date).toISOString(), false);

    if(options.kmpFileName) {
      this.setField(keyboard_info, 'packageFilename', this.callbacks.path.basename(options.kmpFileName));

      // Always overwrite with actual file size
      keyboard_info.packageFileSize = this.callbacks.fileSize(options.kmpFileName);
      if(keyboard_info.packageFileSize === undefined) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename:options.kmpFileName}));
        return null;
      }
    } else {
      // TODO: warn if set in .keyboard_info?
      delete keyboard_info.packageFilename;
      delete keyboard_info.packageFileSize;
    }

    if(options.keyboardFileNameJs) {
      this.setField(keyboard_info, 'jsFilename', this.callbacks.path.basename(options.keyboardFileNameJs));
      // Always overwrite with actual file size
      keyboard_info.jsFileSize = this.callbacks.fileSize(options.keyboardFileNameJs);
      if(keyboard_info.jsFileSize === undefined) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename:options.keyboardFileNameJs}));
        return null;
      }
    } else {
      // TODO: warn if set in .keyboard_info?
      delete keyboard_info.jsFilename;
      delete keyboard_info.jsFileSize;
    }

    const includes = new Set<KeyboardInfoFileIncludes>();
    keyboard_info.packageIncludes = [];
    for(const file of options.kmpJsonData.files) {
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

    // Always overwrite source data
    if(options.kmpFileName) {
      this.setField(keyboard_info, 'version', options.kmpJsonData.info.version.description);
    } else {
      const m = jsFile?.match(/this\.KBVER\s*=\s*(['"])([^'"]+)(\1)/);
      if(m) {
        this.setField(keyboard_info, 'version', m[2]);
      } else {
        keyboard_info.version = '1.0';
      }
    }

    // The minimum Keyman version detected in the package file may be manually
    // set higher by the developer
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
        if(options.kmpJsonData.files.find(file => file.name.match(/\.js$/))) {
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

    // minKeymanVersion
    if(parseFloat(keyboard_info.minKeymanVersion ?? minKeymanVersion) < parseFloat(minVersion)) {
      this.setField(keyboard_info, 'minKeymanVersion', minVersion, false);
    }

    if(options.sourcePath) {
      this.setField(keyboard_info, 'sourcePath', options.sourcePath);
    }

    if(options.helpLink) {
      this.setField(keyboard_info, 'helpLink', options.helpLink);
    }

    const jsonOutput = JSON.stringify(keyboard_info, null, 2);
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

  private setField(keyboard_info: KeyboardInfoFile, field: keyof KeyboardInfoFile, expected: unknown, warn: boolean = true) {
    /* c8 ignore next 4 */
    if (keyboard_info[field] && keyboard_info[field] !== expected && expected !== undefined) {
      if (warn ?? true) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Warn_MetadataFieldInconsistent({
          field, value: keyboard_info[field], expected
        }));
      }

    }
    // TypeScript gets upset with this assignment, because it cannot deduce
    // the exact type of keyboard_info[field] -- there are many possibilities!
    // So we assert that it's unknown so that TypeScript can chill.
    (<unknown> keyboard_info[field]) = keyboard_info[field] || expected;
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
    keyboard_info.languages = keyboard_info.languages ||
      kmpJsonData.keyboards.reduce((a, e) => [].concat(a, e.languages.map((f) => f.id)), []);

    // Transform array into object
    if(Array.isArray(keyboard_info.languages)) {
      const languages: {[index:string]: KeyboardInfoFileLanguage} = {};
      for(const language of keyboard_info.languages) {
        languages[language] = {};
      }
      keyboard_info.languages = languages;
    }

    for(const bcp47 of Object.keys(keyboard_info.languages)) {
      const language = keyboard_info.languages[bcp47];
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