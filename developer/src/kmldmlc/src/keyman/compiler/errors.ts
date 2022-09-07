
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
// const SevInfo = CompilerErrorSeverity.Info;
const SevHint = CompilerErrorSeverity.Hint;
// const SevWarn = CompilerErrorSeverity.Warn;
const SevError = CompilerErrorSeverity.Error;
// const SevFatal = CompilerErrorSeverity.Fatal;

export class CompilerErrors {
  static InvalidNormalization = (o:{form: string}) => m(this.ERROR_InvalidNormalization, `Invalid normalization form '${o.form}`);
  static ERROR_InvalidNormalization = SevError | 0x0001;

  static InvalidLocale = (o:{tag: string}) => m(this.ERROR_InvalidLocale, `Invalid BCP 47 locale form '${o.tag}`);
  static ERROR_InvalidLocale = SevError | 0x0002;

  static HardwareLayerHasTooManyRows = () => m(this.ERROR_HardwareLayerHasTooManyRows, `'hardware' layer has too many rows`);
  static ERROR_HardwareLayerHasTooManyRows = SevError | 0x0003;

  static RowOnHardwareLayerHasTooManyKeys = (o:{row: number}) =>  m(this.ERROR_RowOnHardwareLayerHasTooManyKeys, `Row #${o.row} on 'hardware' layer has too many keys`);
  static ERROR_RowOnHardwareLayerHasTooManyKeys = SevError | 0x0004;

  static KeyNotFoundInKeyBag = (o:{keyId: string, col: number, row: number, layer: string, form: string}) =>
     m(this.ERROR_KeyNotFoundInKeyBag, `Key ${o.keyId} in position #${o.col} on row #${o.row} of layer ${o.layer}, form '${o.form}' not found in key bag`);
  static ERROR_KeyNotFoundInKeyBag = SevError | 0x0005;

  static OneOrMoreRepeatedLocales = () =>
    m(this.HINT_OneOrMoreRepeatedLocales, `After minimization, one or more locales is repeated and has been removed`);
  static HINT_OneOrMoreRepeatedLocales = SevHint | 0x0006;

  static InvalidFile = (errorText: string) =>
    m(this.ERROR_InvalidFile, `The source file has an invalid structure: ${errorText}`);
  static ERROR_InvalidFile = SevError | 0x0007;

  static severityName(code: number): string {
    let severity = code & CompilerErrorSeverity.Severity_Mask;
    switch(severity) {
      case CompilerErrorSeverity.Info: return 'INFO';
      case CompilerErrorSeverity.Hint: return 'HINT';
      case CompilerErrorSeverity.Warn: return 'WARN';
      case CompilerErrorSeverity.Error: return 'ERROR';
      case CompilerErrorSeverity.Fatal: return 'FATAL';
      default: return 'UNKNOWN';
    }
  }
}

