/**
 * Merges a source .keyboard_info file with metadata extracted from .kps file and
 * compiled files to produce a comprehensive .keyboard_info file.
 */

import { minKeymanVersion } from "./min-keyman-version.js";
import { KeyboardInfoFile, KeyboardInfoFileIncludes, KeyboardInfoFileLanguage, KeyboardInfoFilePlatform } from "./keyboard-info-file.js";
import { KeymanFileTypes, CompilerCallbacks, KmpJsonFile, KmxFileReader, KMX, KeymanTargets } from "@keymanapp/common-types";
import { KeyboardInfoCompilerMessages } from "./messages.js";

const regionNames = new Intl.DisplayNames(['en'], { type: "region" });
const scriptNames = new Intl.DisplayNames(['en'], { type: "script" });
const languageNames = new Intl.DisplayNames(['en'], { type: "language" });

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

  /** The compiled keyboard filename and relative path (.js and .kmx) */
  keyboardFileNameJs?: string;
  //keyboardFileNameKmx?: string;

  /** The compiled package filename and relative path (.kmp) */
  kmpFileName: string;
  kpsFileName: string;
};
/* c8 ignore stop */

export class KeyboardInfoCompiler {
  private keyboard_info: KeyboardInfoFile = null;

  private jsFile: {
    filename: string;
    data: string;
    standalone: boolean;
  } = null;

  private kmxFiles: {
    filename: string,
    data: KMX.KEYBOARD
  }[] = [];

  constructor(private callbacks: CompilerCallbacks) {
  }

  private clear() {
    this.keyboard_info = null;
    this.jsFile = null;
    this.kmxFiles = [];
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

    const dataInput = this.callbacks.loadFile(sourceKeyboardInfoFileName);
    if(!dataInput) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename: sourceKeyboardInfoFileName}));
      return null;
    }

    this.clear();
    try {
      const jsonInput = new TextDecoder('utf-8', {fatal: true}).decode(dataInput);
      this.keyboard_info = JSON.parse(jsonInput);
    } catch(e) {
      this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileIsNotValid({filename: sourceKeyboardInfoFileName, e}));
      return null;
    }

    this.loadJsFile(options.keyboardFileNameJs);
    this.loadKmxFiles(options.kpsFileName, options.kmpJsonData);

    //
    // Build merged .keyboard_info file
    // https://api.keyman.com/schemas/keyboard_info.source.json and
    // https://api.keyman.com/schemas/keyboard_info.distribution.json
    // https://help.keyman.com/developer/cloud/keyboard_info/1.0
    //

    // CheckPackageKeyboardFilenames;
    // CheckOrMigrateLanguages;

    this.keyboard_info.isRTL = this.keyboard_info.isRTL ?? !!this.jsFile?.data.match(/this\.KRTL=1/);
    if(!this.keyboard_info.isRTL) {
      delete this.keyboard_info.isRTL;
    }

    //
    // Merge keyboard info file -- some fields have "special" behaviours -- see below
    //

    this.setField('id', options.keyboard_id);

    this.setField('name', options.kmpJsonData.info.name.description);

    const author = options.kmpJsonData.info.author;
    this.setField('authorName', author.description);

    if (author.url) {
      // we strip the mailto: from the .kps file for the .keyboard_info
      const match = author.url.match(/^(mailto\:)?(.+)$/);
      /* c8 ignore next 3 */
      if (match === null) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
        return null;
      }

      const email = match[2];
      this.setField('authorEmail', email, false);
    }

    // extract the language identifiers from the language metadata
    // arrays for each of the keyboards in the kmp.json file,
    // and merge into a single array of identifiers in the
    // .keyboard_info file.

    // TODO: expand and add extra metadata?
    this.fillLanguages(options.kmpJsonData);

    this.setField('lastModifiedDate', (new Date).toISOString());

    if(options.kmpFileName) {
      this.setField('packageFilename', this.callbacks.path.basename(options.kmpFileName));

      // Always overwrite with actual file size
      this.keyboard_info.packageFileSize = this.callbacks.fileSize(options.kmpFileName);
      if(this.keyboard_info.packageFileSize === undefined) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename:options.kmpFileName}));
        return null;
      }
    } else {
      // TODO: warn if set in .keyboard_info
      delete this.keyboard_info.packageFilename;
      delete this.keyboard_info.packageFileSize;
    }

    if(options.keyboardFileNameJs) {
      this.setField('jsFilename', this.callbacks.path.basename(options.keyboardFileNameJs));
      // Always overwrite with actual file size
      this.keyboard_info.jsFileSize = this.callbacks.fileSize(options.keyboardFileNameJs);
      if(this.keyboard_info.jsFileSize === undefined) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Error_FileDoesNotExist({filename:options.keyboardFileNameJs}));
        return null;
      }
    } else {
      // TODO: warn if set in .keyboard_info
      delete this.keyboard_info.jsFilename;
      delete this.keyboard_info.jsFileSize;
    }

    const includes = new Set<KeyboardInfoFileIncludes>();
    this.keyboard_info.packageIncludes = [];
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
    this.keyboard_info.packageIncludes = [...includes];


    // Always overwrite source data
    if(options.kmpFileName) {
      this.setField('version', options.kmpJsonData.info.version.description);
    } else if(this.jsFile?.data) {
      const m = this.jsFile.data.match(/this\.KBVER\s*=\s*(['"])([^'"]+)(\1)/);
      if(m) {
        this.setField('version', m[2]);
      } else {
        this.keyboard_info.version = '1.0';
      }
    }

    // The minimum Keyman version detected in the package file may be manually set higher by the developer
    // TODO: extract from .kmx files
    let minVersion = minKeymanVersion;
    if(this.jsFile?.data) {
      const m = this.jsFile.data.match(/this.KMINVER\s*=\s*(['"])(.*?)\1/);
      if(m) {
        if(parseFloat(m[2]) > parseFloat(minVersion)) {
          minVersion = m[2];
        }
      }
    }

    for(const file of this.kmxFiles) {
      const v = this.kmxFileVersionToString(file.data.fileVersion);
      if(parseFloat(v) > parseFloat(minVersion)) {
        minVersion = v;
      }
    }

    // Only legacy keyboards supprt non-Unicode encodings, and we no longer
    // rewrite the .keyboard_info for those.
    this.keyboard_info.encodings = ['unicode'];

    // platformSupport
    const platforms = new Set<KeyboardInfoFilePlatform>();
    for(const file of this.kmxFiles) {
      const targets = KeymanTargets.keymanTargetsFromString(file.data.targets, {expandAny: true});
      for(const target of targets) {
        this.mapKeymanTargetToPlatform(target).forEach(platform => platforms.add(platform));
      }
    }

    this.keyboard_info.platformSupport = {};
    for(const platform of platforms) {
      this.keyboard_info.platformSupport[platform] = 'full';
    }

    // minKeymanVersion
    if(parseFloat(this.keyboard_info.minKeymanVersion ?? minKeymanVersion) < parseFloat(minVersion)) {
      this.setField('minKeymanVersion', minVersion, false);
    }

    if(options.sourcePath) {
      this.setField('sourcePath', options.sourcePath);
    }

    if(options.helpLink) {
      this.setField('helpLink', options.helpLink);
    }

    const jsonOutput = JSON.stringify(this.keyboard_info, null, 2);
    return new TextEncoder().encode(jsonOutput);
  }

  private mapKeymanTargetToPlatform(target: KeymanTargets.KeymanTarget): KeyboardInfoFilePlatform[] {
    const map: {[index in KeymanTargets.KeymanTarget]: KeyboardInfoFilePlatform[]} = {
      any: [],
      androidphone: ['android'],
      androidtablet: ['android'],
      desktop: ['linux', 'macos', 'windows'],
      ipad: ['ios'],
      iphone: ['ios'],
      linux: ['linux'],
      macosx: ['macos'],
      mobile: ['android','ios','mobileWeb'],
      tablet: ['android','ios','mobileWeb'],
      web: ['desktopWeb'],
      windows: ['windows']
    }
    return map[target] ?? [];
  }

  private kmxFileVersionToString(version: number) {
    return ((version & 0xFF00) >> 8).toString() + '.' + (version & 0xFF).toString();
  }

  private setField(field: keyof KeyboardInfoFile, expected: unknown, warn: boolean = true) {
    /* c8 ignore next 4 */
    if (this.keyboard_info[field] && this.keyboard_info[field] !== expected) {
      if (warn ?? true) {
        this.callbacks.reportMessage(KeyboardInfoCompilerMessages.Warn_MetadataFieldInconsistent({
          field, value:this.keyboard_info[field], expected
        }));
      }

    }
    // TypeScript gets upset with this assignment, because it cannot deduce
    // the exact type of keyboard_info[field] -- there are many possibilities!
    // So we assert that it's unknown so that TypeScript can chill.
    (<unknown> this.keyboard_info[field]) = this.keyboard_info[field] || expected;
  }

  private loadKmxFiles(kpsFilename: string, kmpJsonData: KmpJsonFile.KmpJsonFile) {
    const reader = new KmxFileReader();
    this.kmxFiles = kmpJsonData.files
      .filter(file => KeymanFileTypes.filenameIs(file.name, KeymanFileTypes.Binary.Keyboard))
      .map(file => ({
        filename: this.callbacks.path.basename(file.name),
        data: reader.read(this.callbacks.loadFile(this.callbacks.resolveFilename(kpsFilename, file.name)))
      })
    );
  }

  private loadJsFile(filename?: string) {
    this.jsFile = filename
      ? {
        filename,
        // TODO: consider loader errors
        data: new TextDecoder().decode(this.callbacks.loadFile(filename)),
        standalone: true
      }
      : null;
  }

  private fillLanguages(kmpJsonData:  KmpJsonFile.KmpJsonFile) {
    this.keyboard_info.languages = this.keyboard_info.languages ||
      kmpJsonData.keyboards.reduce((a, e) => [].concat(a, e.languages.map((f) => f.id)), []);

    // Transform array into object
    if(Array.isArray(this.keyboard_info.languages)) {
      const languages: {[index:string]: KeyboardInfoFileLanguage} = {};
      for(const language of this.keyboard_info.languages) {
        languages[language] = {};
      }
      this.keyboard_info.languages = languages;
    }

    for(const bcp47 of Object.keys(this.keyboard_info.languages)) {
      const locale = new Intl.Locale(bcp47);
      this.keyboard_info.languages[bcp47].displayName = languageNames.of(bcp47);
      this.keyboard_info.languages[bcp47].languageName = languageNames.of(locale.language);
      this.keyboard_info.languages[bcp47].regionName = locale.region ? regionNames.of(locale.region) : undefined;
      this.keyboard_info.languages[bcp47].scriptName = locale.script ? scriptNames.of(locale.script) : undefined;
    }
  }

}