/**
 * Error codes. Use these when logging messages.
 * 
 * Extends https://github.com/keymanapp/keyman/blob/99db3c0d2448f448242e6397f9d72e9a7ccee4b9/windows/src/global/inc/Comperr.h
 */
export enum KeymanCompilerError {
  CERR_LEXICAL_MODEL_MIN = 0x0800,
  CERR_LEXICAL_MODEL_MAX = 0x08FF,

  CERR_FATAL = 0x8000,
  CERR_ERROR = 0x4000,
  CERR_WARNING = 0x2000,
  CERR_MEMORY = 0x1000,  // N.B., probably unused in TypeScript

  CERR_FATAL_LM = CERR_FATAL | CERR_LEXICAL_MODEL_MIN,
  /* Place all fatal LM compiler errors here! */

  CERR_ERROR_LM = CERR_ERROR | CERR_LEXICAL_MODEL_MIN,
  /* Place all recoverable LM compiler errors here! */

  CERR_WARN_LM = CERR_WARNING | CERR_LEXICAL_MODEL_MIN,
  /* Place all LM compiler warnings here! */
  MixedNormalizationForms,
  DuplicateWordInSameFile,
}

/**
 * Human-readable titles for the various log levels.
 * 
 * Taken from https://github.com/keymanapp/keyman/blob/d83cfffe511ce65b781f919e89e3693146844849/windows/src/developer/TIKE/project/Keyman.Developer.System.Project.ProjectLog.pas#L39-L46
 */
const LOG_LEVEL_TITLE = {
  [0]: '',
  [KeymanCompilerError.CERR_WARNING]: 'Warning',
  [KeymanCompilerError.CERR_ERROR]: 'Error',
  [KeymanCompilerError.CERR_FATAL]: 'Fatal Error',
};

/**
 * Logs compiler messages (warnings, errors, logs).
 * 
 * @param code Error code
 * @param message A helpful message!
 * @param source [optional] the filename/line number in the source that induced this error
 * 
 * @see https://github.com/keymanapp/keyman/blob/99db3c0d2448f448242e6397f9d72e9a7ccee4b9/windows/src/developer/TIKE/project/Keyman.Developer.System.Project.ProjectLog.pas#L60-L77
 */
export function log(code: KeymanCompilerError, message: string, source?: FilenameAndLineNo) {
  let logMessage = source
    ? new LogMessageFromSource(code, message, source)
    : new LogMessage(code, message);

  console.error(logMessage.format());
}

/**
 * Duct tapes together a filename and a line number of a log.
 */
interface FilenameAndLineNo {
  readonly filename: string;
  readonly lineno: number;
}

/**
 * A log message that knows how to format itself.
 */
class LogMessage {
  readonly code: KeymanCompilerError;
  readonly message: string;

  constructor(code: KeymanCompilerError, message: string) {
    this.code = code;
    this.message = message;
  }

  get logLevel(): KeymanCompilerError {
    return this.code & 0xF000;
  }

  determineLogLevelTitle(): string {
    return LOG_LEVEL_TITLE[this.logLevel] || '';
  }

  format(): string {
    let prefix = this.determineLogLevelTitle();
    if (prefix)
      prefix = `${prefix}: `;

    return `${prefix}${h(this.code)} ${this.message}`   
  }
}

/**
 * A log message with a filename and line number.
 */
class LogMessageFromSource extends LogMessage {
  readonly filename: string;
  readonly lineno: number;

  constructor(code: KeymanCompilerError, message: string, source: FilenameAndLineNo) {
    super(code, message);
    this.filename = source.filename;
    this.lineno = source.lineno;
  }

  format(): string {
    let originalMessage = super.format();
    return `${this.filename} (${this.lineno}): ${originalMessage}`;
  }
}

/**
 * Format a number as a zero-padded 4 digit hexadecimal.
 */
function h(n: number) {
  let formatted = n.toString(16).toUpperCase();
  if (formatted.length < 4) {
    formatted = '0'.repeat(4 - formatted.length);
  }

  return formatted; 
}