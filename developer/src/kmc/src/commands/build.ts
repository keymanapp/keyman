import * as fs from 'fs';
import { Command } from 'commander';
import { BuildActivityOptions } from './build/BuildActivity.js';
import { buildActivities } from './build/buildActivities.js';
import { BuildProject, PROJECT_EXTENSION } from './build/BuildProject.js';
import { NodeCompilerCallbacks } from '../messages/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/messages.js';

export function declareBuild(program: Command) {
  program
    .command('build [infile...]')
    .description('Build a source file into a final file')
    .option('-d, --debug', 'Include debug information in output')
    .option('-o, --out-file <filename>', 'Override the default path and filename for the output file')
    .option('--no-compiler-version', 'Exclude compiler version metadata from output')
    .option('-w, --compiler-warnings-as-errors', 'Causes warnings to fail the build')
    .option('--no-warn-deprecated-code', 'Turn off warnings for deprecated code styles')
    .action(async (filenames: string[], options: any) => {
      if(!filenames.length) {
        // If there are no filenames provided, then we are building the current
        // folder ('.') as a project-style build
        filenames.push('.');
      }

      for(let filename of filenames) {
        if(!await build(filename, options)) {
          // Once a file fails to build, we bail on subsequent builds
          // TODO: is this the most appropriate semantics?
          process.exit(1);
        }
      }
    });
}

async function build(filename: string, options: BuildActivityOptions): Promise<boolean> {
  let callbacks = new NodeCompilerCallbacks();

  try {
    callbacks.reportMessage(InfrastructureMessages.Info_BuildingFile({filename}));

    if(!fs.existsSync(filename)) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileDoesNotExist({filename}));
      return false;
    }

    let builder = null;

    // If infile is a directory, then we treat that as a project and build it
    if(fs.statSync(filename).isDirectory() || filename.toLowerCase().endsWith(PROJECT_EXTENSION)) {
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

    let result = await builder.build(filename, callbacks, options);
    if(result) {
      callbacks.reportMessage(InfrastructureMessages.Info_FileBuiltSuccessfully({filename}));
    } else {
      callbacks.reportMessage(InfrastructureMessages.Info_FileNotBuiltSuccessfully({filename}));
    }

    return result;
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }
}
