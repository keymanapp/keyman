#!/usr/bin/env node
/**
 * kmc - Keyman Next Generation Compiler
 */

import { Command } from 'commander';
import { declareBuild } from './commands/build.js';
import { declareBuildTestData } from './commands/buildTestData.js';
import { declareAnalyze } from './commands/analyze.js';
import { BaseOptions } from './util/baseOptions.js';
import { KeymanSentry } from './util/KeymanSentry.js';

await KeymanSentry.runTestIfCLRequested();
try {
  await run();
} catch(e) {
  KeymanSentry.captureException(e);
}

async function run() {
  /* Arguments */

  const program = new Command();
  program.description('Keyman Developer Command Line Interface');
  BaseOptions.addVersion(program);
  BaseOptions.addSentry(program);

  if(await KeymanSentry.isEnabled()) {
    KeymanSentry.init();
  }

  declareBuild(program);
  declareBuildTestData(program);  // TODO: consider renaming this (build vs build-test-data is confusing)
  declareAnalyze(program);

  /* Future commands:
  declareClean(program);
  declareCopy(program);
  declareRename(program);
  declareGenerate(program);
  declareImport(program);
  declareTest(program);
  declarePublish(program);
  */

  await program.parseAsync(process.argv)
    .catch(reason => console.error(reason));
}