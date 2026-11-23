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
import { posix } from 'node:path';

/* c8 ignore start */

export function declareCopy(program: Command) {
  const command = program.command('copy <origin> [id]');
  BaseOptions.addAll(command);
  command
    .description('copy a Keyman keyboard or lexical model project')
    .option('-o, --out-path <path>', 'output path (may exist)')
    .option('-n, --dry-run', 'show what would happen, without making changes')
    .option('-r, --relocate-external', 'copy external files into "external" folder')
    .action(copyProject)
    .addHelpText('after', `
<project> can be:
  * a .kpj file, e.g. ./keyboards/khmer_angkor/khmer_angkor.kpj
  * a local folder (with a .kpj file in it), e.g. ./keyboards/khmer_angkor
  * a cloud keyboard or lexical model, cloud:id, e.g. cloud:khmer_angkor
  * a GitHub repository, branch, and path, [https://]github.com/owner/repo/tree/branch/path
    e.g. https://github.com/keyman-keyboards/khmer_angkor/tree/main/khmer_angkor.kpj or
         github.com/keymanapp/keyboards/tree/master/release/k/khmer_angkor

A new subfolder will be made either in the current folder, or in the output path, with the
same name as the <id> parameter. This subfolder must not already exist.

Deprecation note (19.0): if <id> is not passed as a parameter, the last component of the
output path will be the new id of the project. This mode is deprecated and will be removed
in a future version of kmc.`);
  }

function commanderOptionsToCopierOptions(id: string, options: any): CopierOptions {
  const result: CopierOptions = {
    ...commanderOptionsToBaseOptions(options),
    // CopierOptions
    outPath: id ? posix.join(options.outPath?.replace(/\\/g, '/') ?? '', id) : options.outPath,
    dryRun: options.dryRun ?? false,
    relocateExternalFiles: options.relocateExternal ?? false,
  };
  return result;
}

const MaxDryRunCopyLogMessages = 10000;

async function copyProject(projects: string | string[], ids: string | string[], _options: any, commander: any): Promise<never|void> {
  const source = Array.isArray(projects) ? projects[0] : projects;
  const target = Array.isArray(ids) ? ids[0] : ids;

  const commanderOptions = commander.optsWithGlobals();
  const options = commanderOptionsToCopierOptions(target, commanderOptions);
  const callbacks = new NodeCompilerCallbacks(options);

  if(!target) {
    if(!commanderOptions.outPath) {
      callbacks.reportMessage(InfrastructureMessages.Error_CopyRequiresOutPath());
      return await exitProcess(1);
    }
    callbacks.reportMessage(InfrastructureMessages.Warn_CopyWithoutIdIsDeprecated());
  }

  if(!await doCopy(callbacks, source, options)) {
    return await exitProcess(1);
  }
}

/* c8 ignore stop */

async function doCopy(callbacks: NodeCompilerCallbacks, sources: string, options: CopierOptions): Promise<boolean> {
  const source = sources;

  if(!source) {
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

  const dest = options.outPath;
  callbacks.reportMessage(InfrastructureMessages.Info_CopyingProject({source, dest}));
  const result = await runCopier(callbacks, source, options);
  if(result) {
    callbacks.reportMessage(InfrastructureMessages.Info_ProjectCopiedSuccessfully({source, dest}));
  } else {
    callbacks.reportMessage(InfrastructureMessages.Info_ProjectNotCopiedSuccessfully({source, dest}));
  }
  return result;
}

async function runCopier(callbacks: NodeCompilerCallbacks, source: string, options: CopierOptions) {
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