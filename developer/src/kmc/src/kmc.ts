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
import { exitProcess } from './util/sysexits.js';
import { declareMessage } from './commands/messageCommand.js';
import { kmcSentryOptions } from './util/kmcSentryOptions.js';
import { declareGenerate } from './commands/generate.js';
import { declareCopy } from './commands/copy.js';
import { declareConvert } from './commands/convert.js';

await TestKeymanSentry.runTestIfCLRequested(kmcSentryOptions);
if(KeymanSentry.isEnabled()) {
  KeymanSentry.init(kmcSentryOptions);
}

run().then(async () => {
  // Ensure any messages reported to Sentry have had time to be uploaded before we
  // exit. In most cases, this will be a no-op so should not affect performance.
  await exitProcess(0);
}, (reason: any) => {
  KeymanSentry.captureException(reason);
  // in local environment, captureException will
  // return so we want to re-throw; note that this is
  // a little noisy because it comes from an async function
  console.error('Aborting due to fatal exception. Local development environment detected, so printing trace.');
  const report: any = process.report?.getReport(reason);
  console.log(report?.javascriptStack?.message);
  console.log(report?.javascriptStack?.stack?.map((s: string) => '   '+s).join('\n'));
  // This cannot be an async function because otherwise the stack trace gets captured
  exitProcess(1);
});

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

  declareBuild(program);
  declareAnalyze(program);
  declareConvert(program);
  declareMessage(program);
  declareGenerate(program);
  declareCopy(program);

  /* Future commands:
  declareClean(program);
  declareImport(program);
  declareTest(program);
  declarePublish(program);
  */

  await program.parseAsync(process.argv);
}