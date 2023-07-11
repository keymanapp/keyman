import * as fs from 'fs';
import { Command } from 'commander';
import { buildActivities } from './buildClasses/buildActivities.js';
import { BuildProject } from './buildClasses/BuildProject.js';
import { CompilerLogColor, NodeCompilerCallbacks } from '../messages/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/messages.js';
import { CompilerErrorSeverity, CompilerErrorMask, CompilerFileCallbacks, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { BaseOptions } from '../util/baseOptions.js';


function commandOptionsToCompilerOptions(options: any): CompilerOptions {
  // We don't want to rename command line options to match the precise
  // properties that we have in CompilerOptions, but nor do we want to rename
  // CompilerOptions properties...
  return {
    // CompilerBaseOptions
    outFile: options.outFile,
    logLevel: options.logLevel,
    // CompilerOptions
    shouldAddCompilerVersion: options.compilerVersion,
    saveDebug: options.debug,
    compilerWarningsAsErrors: options.compilerWarningsAsErrors,
    warnDeprecatedCode: options.warnDeprecatedCode,
  }
}

export function declareBuild(program: Command) {
  BaseOptions.addAll(program
    .command('build [infile...]')
    .description('Build a source file into a final file')
  )
    .option('-d, --debug', 'Include debug information in output')
    .option('--no-compiler-version', 'Exclude compiler version metadata from output')
    .option('-w, --compiler-warnings-as-errors', 'Causes warnings to fail the build')
    .option('--no-warn-deprecated-code', 'Turn off warnings for deprecated code styles')
    .action(async (filenames: string[], options: any) => {
      options = commandOptionsToCompilerOptions(options);
      const callbacks = new NodeCompilerCallbacks({color:CompilerLogColor.default, ...options});

      if(!filenames.length) {
        // If there are no filenames provided, then we are building the current
        // folder ('.') as a project-style build
        filenames.push('.');
      }

      for(let filename of filenames) {
        if(!await build(filename, callbacks, options)) {
          // Once a file fails to build, we bail on subsequent builds
          // TODO: is this the most appropriate semantics?
          process.exit(1);
        }
      }
    });
}

async function build(filename: string, parentCallbacks: NodeCompilerCallbacks, options: CompilerOptions): Promise<boolean> {
  let callbacks = new CompilerFileCallbacks(filename, parentCallbacks);

  try {
    callbacks.reportMessage(InfrastructureMessages.Info_BuildingFile({filename}));

    if(!fs.existsSync(filename)) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileDoesNotExist({filename}));
      return false;
    }

    let builder = null;

    // If infile is a directory, then we treat that as a project and build it
    if(fs.statSync(filename).isDirectory() || KeymanFileTypes.filenameIs(filename, KeymanFileTypes.Source.Project)) {
      builder = new BuildProject();
    } else {
      // Otherwise, if it's one of our known file extensions, we build it
      let extensions: string[] = [];
      builder = buildActivities.find(build => {
        extensions.push(build.sourceExtension);
        return filename.toLowerCase().endsWith(build.sourceExtension);
      });
      if(!builder) {
        callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotRecognized({filename, extensions: extensions.join(', ')}));
        return false;
      }
    }

    const failureCodes = [CompilerErrorSeverity.Fatal, CompilerErrorSeverity.Error];
    // TODO: #9100: .concat(options.compilerWarningsAsErrors ? [CompilerErrorSeverity.Warn] : []);
    let result = await builder.build(filename, callbacks, options);
    const firstFailureMessage = parentCallbacks.messages.find(m => failureCodes.includes(m.code & CompilerErrorMask.Severity));
    if(result && firstFailureMessage == undefined) {
      callbacks.reportMessage(builder instanceof BuildProject
        ? InfrastructureMessages.Info_ProjectBuiltSuccessfully({filename})
        : InfrastructureMessages.Info_FileBuiltSuccessfully({filename})
      );
    } else {
      callbacks.reportMessage(builder instanceof BuildProject
        ? InfrastructureMessages.Info_ProjectNotBuiltSuccessfully({filename})
        : InfrastructureMessages.Info_FileNotBuiltSuccessfully({filename})
      );
    }

    return result;
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }
}
