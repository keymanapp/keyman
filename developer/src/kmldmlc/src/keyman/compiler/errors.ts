
export enum CompilerErrorSeverity {
  Info =          0x000000,
  Hint =          0x100000,
  Warn =          0x200000,
  Error =         0x300000,
  Fatal =         0x400000,
  Severity_Mask = 0xF00000, // includes reserved bits
  Error_Mask =    0x0FFFFF,
};

export enum CompilerErrors {
  ERROR_InvalidNormalization = CompilerErrorSeverity.Error | 0x0001, // `Invalid normalization form '${}'`
  ERROR_InvalidLocale        = CompilerErrorSeverity.Error | 0x0002, // `Invalid BCP 47 locale form '${}'`
};

export function getErrorSeverityName(code: number): string {
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