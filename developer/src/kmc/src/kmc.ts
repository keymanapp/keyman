#!/usr/bin/env node
/**
 * kmc - Keyman Next Generation Compiler
 */


import { Command } from 'commander';
import { declareBuild } from './commands/build.js';
import { declareBuildTestData } from './commands/buildTestData.js';
import { declareAnalyze } from './commands/analyze.js';
import { BaseOptions } from './util/baseOptions.js';

const program = new Command();

/* Arguments */

BaseOptions.addVersion(program)
  .description('Keyman Developer Command Line Interface');
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

