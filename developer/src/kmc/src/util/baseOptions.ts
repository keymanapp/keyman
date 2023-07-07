import { ALL_COMPILER_LOG_LEVELS } from "@keymanapp/common-types";
import { Command, Option } from "commander";
import KEYMAN_VERSION from "@keymanapp/keyman-version";

// These options map to CompilerBaseOptions

export class BaseOptions {
  public static addVersion(program: Command) {
    return program.version(KEYMAN_VERSION.VERSION_WITH_TAG);
  }

  public static addLogLevel(program: Command) {
    return program.addOption(new Option('-l, --log-level <logLevel>', 'Log level').choices(ALL_COMPILER_LOG_LEVELS).default('info'));
  }

  public static addOutFile(program: Command) {
    return program.option('-o, --out-file <filename>', 'Override the default path and filename for the output file')
  }

  public static addAll(program: Command) {
    return [
      this.addVersion,
      this.addLogLevel,
      this.addOutFile,
    ].reduce((p,f) => f(p), program);
  }
}
