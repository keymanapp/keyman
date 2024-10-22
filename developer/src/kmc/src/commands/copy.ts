/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Command-line wrapper for kmc-copy
 */

import { Command } from 'commander';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { BaseOptions } from '../util/baseOptions.js';
import { exitProcess } from '../util/sysexits.js';
import { CopierOptions, KeymanProjectCopier } from '@keymanapp/kmc-copy';

/* c8 ignore start */

export function declareCopy(program: Command) {
  const command = program.command('copy <project>');
  BaseOptions.addLogLevel(command);
  command
    .description('Copy a Keyman keyboard or lexical model project')
    .option('-o, --out-path <path>', 'New name and path for project')
    // TODO-COPY .option('-r, --rename', 'Rename instead of copying project')
    .option('-n, --dry-run', 'Show what would happen, without making changes')
    .action(copyProject)
    .addHelpText('before', `
      <project> can be:
      * a .kpj file, e.g. ./keyboards/khmer_angkor/khmer_angkor.kpj
      * a local folder (with a .kpj file in it), e.g. ./keyboards/khmer_angkor
    `);
// TODO-COPY * a cloud keyboard, e.g. cloud:khmer_angkor
// TODO-COPY * a GitHub repository, e.g. github:keyman-keyboards/khmer_angkor
  }

function commanderOptionsToCopierOptions(options: any): CopierOptions {
  const result: CopierOptions = {
    outPath: options.outPath,
    // TODO-COPY rename: options.rename ?? false,
    dryRun: options.dryRun ?? false,
  };
  return result;
}

const MaxDryRunCopyLogMessages = 10000;

async function copyProject(ids: string | string[], _options: any, commander: any): Promise<never|void> {
  const commanderOptions = commander.optsWithGlobals();
  const callbacks = new NodeCompilerCallbacks({logLevel: commanderOptions.logLevel ?? 'info'});
  if(!await doCopy(callbacks, ids, commanderOptions)) {
    return await exitProcess(1);
  }
}

/* c8 ignore stop */

async function doCopy(callbacks: NodeCompilerCallbacks, sources: string | string[], commanderOptions: any): Promise<boolean> {
  const source = sources;

  if(!source || typeof source != 'string') {
    // Note that commander can pass an array for the ids parameter, so we
    // constrain here
    callbacks.reportMessage(InfrastructureMessages.Error_CopyRequiresSource());
    return false;
  }

  const options = commanderOptionsToCopierOptions(commanderOptions);

  if(options.dryRun) {
    // For dry run, we may need a lot of log messages, to show
    // where all the files are copied to
    callbacks.maxLogMessages = MaxDryRunCopyLogMessages;
  }

  if(!options.outPath) {
    callbacks.reportMessage(InfrastructureMessages.Error_CopyRequiresOutPath());
    return false;
  }

  const copier = new KeymanProjectCopier();
  try {
    if(!await copier.init(callbacks, options)) {
      // errors will have been reported by the copier
      return false;
    }
    const result = await copier.run(source);
    if(!result) {
      // errors will have been reported by the copier
      return false;
    }
    if(!await copier.write(result.artifacts)) {
      // errors will have been reported by the copier
      return false;
    }
  } catch(e) {
    /* c8 ignore next 3 */
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }

  return true;
}

/** @internal */
export const unitTestEndpoints = {
  doCopy,
}