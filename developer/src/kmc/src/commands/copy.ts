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
import { commanderOptionsToBaseOptions } from '../util/extendedCompilerOptions.js';

/* c8 ignore start */

export function declareCopy(program: Command) {
  const command = program.command('copy <project>');
  BaseOptions.addAll(command);
  command
    .description('Copy a Keyman keyboard or lexical model project')
    .option('-o, --out-path <path>', 'New name and path for project')
    .option('-n, --dry-run', 'Show what would happen, without making changes')
    .option('-r, --relocate-external', 'Copy external files into "external" folder')
    .action(copyProject)
    .addHelpText('before', `
      <project> can be:
      * a .kpj file, e.g. ./keyboards/khmer_angkor/khmer_angkor.kpj
      * a local folder (with a .kpj file in it), e.g. ./keyboards/khmer_angkor
      * a cloud keyboard or lexical model, cloud:id, e.g. cloud:khmer_angkor
      * a GitHub repository, branch, and path, [https://]github.com/owner/repo/tree/branch/path
        e.g. https://github.com/keyman-keyboards/khmer_angkor/tree/main/khmer_angkor.kpj or
             github.com/keymanapp/keyboards/tree/master/release/k/khmer_angkor
    `);
  }

function commanderOptionsToCopierOptions(options: any): CopierOptions {
  const result: CopierOptions = {
    ...commanderOptionsToBaseOptions(options),
    // CopierOptions
    outPath: options.outPath,
    dryRun: options.dryRun ?? false,
    relocateExternalFiles: options.relocateExternal ?? false,
  };
  return result;
}

const MaxDryRunCopyLogMessages = 10000;

async function copyProject(ids: string | string[], _options: any, commander: any): Promise<never|void> {
  const commanderOptions = commander.optsWithGlobals();
  const options = commanderOptionsToCopierOptions(commanderOptions);
  const callbacks = new NodeCompilerCallbacks(options);
  if(!await doCopy(callbacks, ids, options)) {
    return await exitProcess(1);
  }
}

/* c8 ignore stop */

async function doCopy(callbacks: NodeCompilerCallbacks, sources: string | string[], options: CopierOptions): Promise<boolean> {
  const source = sources;

  if(!source || typeof source != 'string') {
    // Note that commander can pass an array for the ids parameter, so we
    // constrain here
    callbacks.reportMessage(InfrastructureMessages.Error_CopyRequiresSource());
    return false;
  }

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