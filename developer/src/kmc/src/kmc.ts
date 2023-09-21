#!/usr/bin/env node
/**
 * kmc - Keyman Next Generation Compiler
 */

import { Command } from 'commander';
import { declareBuild } from './commands/build.js';
import { declareAnalyze } from './commands/analyze.js';
import { BaseOptions } from './util/baseOptions.js';
import { KeymanSentry } from './util/KeymanSentry.js';

await KeymanSentry.runTestIfCLRequested();
try {
  await run();
} catch(e) {
  KeymanSentry.captureException(e);
}

// Ensure any messages reported to Sentry have had time to be uploaded before we
// exit. In most cases, this will be a no-op so should not affect performance.
await KeymanSentry.close();

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