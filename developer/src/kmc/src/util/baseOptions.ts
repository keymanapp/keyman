import { ALL_COMPILER_LOG_FORMATS, ALL_COMPILER_LOG_LEVELS, CompilerLogFormat, CompilerLogLevel } from "@keymanapp/developer-utils";
import { Command, Option } from "commander";

/**
 * Abstract interface for compiler options
 */

export interface CommandLineBaseOptions {
  // These options map to CompilerBaseOptions
  logLevel?: CompilerLogLevel;
  logFormat?: CompilerLogFormat;
  color?: boolean;

  // This option is not in CompilerBaseOptions
  outFile?:string;
}

/**
 * These options map to CompilerBaseOptions
 */
export class BaseOptions {
  public static addLogLevel(program: Command) {
    return program.addOption(new Option('-l, --log-level <logLevel>', 'Log level').choices(ALL_COMPILER_LOG_LEVELS).default('info'));
  }

  public static addLogFormat(program: Command) {
    return program.addOption(new Option('--log-format <logFormat>', 'Log format').choices(ALL_COMPILER_LOG_FORMATS).default('formatted'));
  }

  public static addOutFile(program: Command) {
    return program.option('-o, --out-file <filename>', 'Override the default path and filename for the output file')
  }

  public static addAll(program: Command) {
    return [
      this.addLogLevel,
      this.addLogFormat,
      this.addOutFile,
    ].reduce((p,f) => f(p), program);
  }
}
