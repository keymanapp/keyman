import { ALL_COMPILER_LOG_FORMATS, ALL_COMPILER_LOG_LEVELS } from "@keymanapp/common-types";
import { Command, Option } from "commander";
import KEYMAN_VERSION from "@keymanapp/keyman-version";

// These options map to CompilerBaseOptions

export class BaseOptions {
  public static addVersion(program: Command) {
    return program.version(KEYMAN_VERSION.VERSION_WITH_TAG);
  }

  public static addSentry(program: Command) {
    // This corresponds to an option tested in KeymanSentry.ts, which is
    // searched for in process.argv, in order to avoid depending on Commander to
    // start Sentry, and to ensure that we capture errors as early as possible
    // in launch
    return program
      .addOption(new Option('--no-error-reporting', 'Disable error reporting to keyman.com (overriding user settings)'))
      .addOption(new Option('--error-reporting', 'Enable error reporting to keyman.com (overriding user settings)'));
  }

  public static addLogLevel(program: Command) {
    return program.addOption(new Option('-l, --log-level <logLevel>', 'Log level').choices(ALL_COMPILER_LOG_LEVELS).default('info'));
  }

  public static addLogFormat(program: Command) {
    return program.addOption(new Option('-l, --log-format <logFormat>', 'Log format').choices(ALL_COMPILER_LOG_FORMATS).default('formatted'));
  }

  public static addOutFile(program: Command) {
    return program.option('-o, --out-file <filename>', 'Override the default path and filename for the output file')
  }

  public static addAll(program: Command) {
    return [
      this.addVersion,
      this.addLogLevel,
      this.addLogFormat,
      this.addOutFile,
    ].reduce((p,f) => f(p), program);
  }
}
