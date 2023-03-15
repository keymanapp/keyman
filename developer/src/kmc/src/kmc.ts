#!/usr/bin/env node
/**
 * kmc - Keyman Next Generation Compiler
 */


import { Command } from 'commander';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { declareBuild } from './commands/build.js';
import { declareBuildTestData } from './commands/buildTestData.js';

const program = new Command();

/* Arguments */
program
  .description('Keyman Developer Command Line Interface')
  .version(KEYMAN_VERSION.VERSION_WITH_TAG);

declareBuild(program);
declareBuildTestData(program);

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

