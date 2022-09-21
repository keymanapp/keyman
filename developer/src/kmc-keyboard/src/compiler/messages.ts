
export enum CompilerErrorSeverity {
  Info =          0x000000,
  Hint =          0x100000,
  Warn =          0x200000,
  Error =         0x300000,
  Fatal =         0x400000,
  Severity_Mask = 0xF00000, // includes reserved bits
  Error_Mask =    0x0FFFFF,
};

const m = (code: number, message: string) => { return { code, message } };
const SevInfo = CompilerErrorSeverity.Info;
const SevHint = CompilerErrorSeverity.Hint;
// const SevWarn = CompilerErrorSeverity.Warn;
const SevError = CompilerErrorSeverity.Error;
const SevFatal = CompilerErrorSeverity.Fatal;

export class CompilerMessages {
  static Error_InvalidNormalization = (o:{form: string}) => m(this.ERROR_InvalidNormalization, `Invalid normalization form '${o.form}`);
  static ERROR_InvalidNormalization = SevError | 0x0001;

  static Error_InvalidLocale = (o:{tag: string}) => m(this.ERROR_InvalidLocale, `Invalid BCP 47 locale form '${o.tag}`);
  static ERROR_InvalidLocale = SevError | 0x0002;

  static Error_HardwareLayerHasTooManyRows = () => m(this.ERROR_HardwareLayerHasTooManyRows, `'hardware' layer has too many rows`);
  static ERROR_HardwareLayerHasTooManyRows = SevError | 0x0003;

  static Error_RowOnHardwareLayerHasTooManyKeys = (o:{row: number}) =>  m(this.ERROR_RowOnHardwareLayerHasTooManyKeys, `Row #${o.row} on 'hardware' layer has too many keys`);
  static ERROR_RowOnHardwareLayerHasTooManyKeys = SevError | 0x0004;

  static Error_KeyNotFoundInKeyBag = (o:{keyId: string, col: number, row: number, layer: string, form: string}) =>
     m(this.ERROR_KeyNotFoundInKeyBag, `Key '${o.keyId}' in position #${o.col} on row #${o.row} of layer ${o.layer}, form '${o.form}' not found in key bag`);
  static ERROR_KeyNotFoundInKeyBag = SevError | 0x0005;

  static Hint_OneOrMoreRepeatedLocales = () =>
    m(this.HINT_OneOrMoreRepeatedLocales, `After minimization, one or more locales is repeated and has been removed`);
  static HINT_OneOrMoreRepeatedLocales = SevHint | 0x0006;

  static Error_InvalidFile = (o:{errorText: string}) =>
    m(this.ERROR_InvalidFile, `The source file has an invalid structure: ${o.errorText}`);
  static ERROR_InvalidFile = SevError | 0x0007;

  static Hint_LocaleIsNotMinimalAndClean = (o:{sourceLocale: string, locale: string}) =>
    m(this.HINT_LocaleIsNotMinimalAndClean, `Locale '${o.sourceLocale}' is not minimal or correctly formatted and should be '${o.locale}'`);
  static HINT_LocaleIsNotMinimalAndClean = SevHint | 0x0008;

  static Error_VkeyIsNotValid = (o:{vkey: string}) =>
    m(this.ERROR_VkeyIsNotValid, `Virtual key '${o.vkey}' is not found in the CLDR VKey Enum table.`);
  static ERROR_VkeyIsNotValid = SevError | 0x0009;

  static Hint_VkeyMapIsRedundant = (o:{vkey: string}) =>
    m(this.HINT_VkeyMapIsRedundant, `Virtual key '${o.vkey}' is mapped to itself, which is redundant.`);
  static HINT_VkeyMapIsRedundant = SevHint | 0x000A;

  static Error_VkeyMapIsRepeated = (o:{vkey: string}) =>
    m(this.ERROR_VkeyMapIsRepeated, `Virtual key '${o.vkey}' has more than one VkeyMap entry.`);
  static ERROR_VkeyMapIsRepeated = SevError | 0x000B;

  static Info_MultipleVkeyMapsHaveSameTarget = (o:{vkey: string}) =>
    m(this.INFO_MultipleVkeyMapsHaveSameTarget, `Target virtual key '${o.vkey}' has multiple source mappings, which may be an error.`);
  static INFO_MultipleVkeyMapsHaveSameTarget = SevInfo | 0x000C;

  static Error_InvalidVersion = (o:{version: string}) =>
    m(this.ERROR_InvalidVersion, `Version number '${o.version}' must be a semantic version format string.`);
  static ERROR_InvalidVersion = SevError | 0x000D;

  static Error_MustBeAtLeastOneLayerElement = () =>
    m(this.ERROR_MustBeAtLeastOneLayerElement, `The source file must contain at least one layer element.`);
  static ERROR_MustBeAtLeastOneLayerElement = SevError | 0x000E;

  static Fatal_SectionCompilerFailed = (o:{sect: string}) =>
    m(this.FATAL_SectionCompilerFailed, `The compiler for '${o.sect}' failed unexpectedly.`);
  static FATAL_SectionCompilerFailed = SevFatal | 0x000F;

  static severityName(code: number): string {
    let severity = code & CompilerErrorSeverity.Severity_Mask;
    switch(severity) {
      case CompilerErrorSeverity.Info: return 'INFO';
      case CompilerErrorSeverity.Hint: return 'HINT';
      case CompilerErrorSeverity.Warn: return 'WARN';
      case CompilerErrorSeverity.Error: return 'ERROR';
      case CompilerErrorSeverity.Fatal: return 'FATAL';
      /* istanbul ignore next */
      default: return 'UNKNOWN';
    }
  }
}

