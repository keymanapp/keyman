import { ALL_COMPILER_LOG_LEVELS } from "@keymanapp/common-types";
import { Command, Option } from "commander";
import KEYMAN_VERSION from "@keymanapp/keyman-version";

export function addBaseOptions(program: Command) {
  return program
    .version(KEYMAN_VERSION.VERSION_WITH_TAG)
    // These options map to CompilerBaseOptions
    .option('-o, --out-file <filename>', 'Override the default path and filename for the output file')
    .addOption(new Option('-l, --log-level <logLevel>', 'Log level').choices(ALL_COMPILER_LOG_LEVELS).default('info'));
}
