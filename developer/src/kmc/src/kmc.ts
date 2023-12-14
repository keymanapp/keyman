#!/usr/bin/env node
/**
 * kmc - Keyman Next Generation Compiler
 */

import { Command, Option } from 'commander';
import { declareBuild } from './commands/build.js';
import { declareAnalyze } from './commands/analyze.js';
import { KeymanSentry, loadOptions } from '@keymanapp/developer-utils';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { TestKeymanSentry } from './util/TestKeymanSentry.js';

await TestKeymanSentry.runTestIfCLRequested();
KeymanSentry.init();
try {
  await run();
} catch(e) {
  KeymanSentry.captureException(e);
}

// Ensure any messages reported to Sentry have had time to be uploaded before we
// exit. In most cases, this will be a no-op so should not affect performance.
await KeymanSentry.close();

async function run() {
  await loadOptions();

  /* Arguments */

  const program = new Command();
  program
    .description('Keyman Developer Command Line Interface')
    .configureHelp({
      showGlobalOptions: true
    })
    .version(KEYMAN_VERSION.VERSION_WITH_TAG)

    // This corresponds to an option tested in KeymanSentry.ts, which is
    // searched for in process.argv, in order to avoid depending on Commander to
    // start Sentry, and to ensure that we capture errors as early as possible
    // in launch
    .addOption(new Option('--no-error-reporting', 'Disable error reporting to keyman.com (overriding user settings)'))
    .addOption(new Option('--error-reporting', 'Enable error reporting to keyman.com (overriding user settings)'));

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