#!/usr/bin/env node
/**
 * kmc - Keyman Next Generation Compiler
 */


import { Command, Option } from 'commander';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { declareBuild } from './commands/build.js';
import { declareBuildTestData } from './commands/buildTestData.js';
import { declareAnalyze } from './commands/analyze.js';
import { ALL_COMPILER_LOG_LEVELS } from '@keymanapp/common-types';

const program = new Command();

/* Arguments */
program
  .description('Keyman Developer Command Line Interface')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG)

  // These options map to CompilerBaseOptions
  .option('-o, --out-file <filename>', 'Override the default path and filename for the output file')
  .addOption(new Option('-l, --log-level <logLevel>', 'Log level').choices(ALL_COMPILER_LOG_LEVELS).default('info'));

declareBuild(program);
declareBuildTestData(program);
declareAnalyze(program);

/*
program
  .command('clean');

program
  .command('copy');

program
  .command('rename');

program
  .command('generate');

program
  .command('import');

program
  .command('test');

program
  .command('publish');
*/

program.parseAsync(process.argv)
  .catch(reason => console.error(reason));

