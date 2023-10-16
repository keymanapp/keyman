import * as fs from 'fs';
import * as path from 'path';
import { Command } from 'commander';
import { buildActivities } from './buildClasses/buildActivities.js';
import { BuildProject } from './buildClasses/BuildProject.js';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { CompilerFileCallbacks, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { BaseOptions } from '../util/baseOptions.js';
import { expandFileLists } from '../util/fileLists.js';
import { isProject } from '../util/projectLoader.js';
import { buildTestData } from './buildTestData/index.js';
import { buildWindowsPackageInstaller } from './buildWindowsPackageInstaller/index.js';
import { ExtendedCompilerOptions } from 'src/util/extendedCompilerOptions.js';

function commandOptionsToCompilerOptions(options: any): ExtendedCompilerOptions {
  // We don't want to rename command line options to match the precise
  // properties that we have in CompilerOptions, but nor do we want to rename
  // CompilerOptions properties...
  return {
    // CompilerBaseOptions
    outFile: options.outFile,
    logLevel: options.logLevel,
    logFormat: options.logFormat,
    color: options.color,
    // CompilerOptions
    shouldAddCompilerVersion: options.compilerVersion,
    saveDebug: options.debug,
    compilerWarningsAsErrors: options.compilerWarningsAsErrors,
    warnDeprecatedCode: options.warnDeprecatedCode,
    // ExtendedOptions
    forPublishing: options.forPublishing,
  }
}

export function declareBuild(program: Command) {
  const buildCommand = program
    .command('build')
    .option('--color', 'Force colorization for log messages')
    .option('--no-color', 'No colorization for log messages; if both omitted, detects from console')

    // These options are only used with build file but are included here so that
    // they are visible in `kmc build --help`
    .option('-d, --debug', 'Include debug information in output')
    .option('-w, --compiler-warnings-as-errors', 'Causes warnings to fail the build; overrides project-level warnings-as-errors option')
    .option('-W, --no-compiler-warnings-as-errors', 'Warnings do not fail the build; overrides project-level warnings-as-errors option')
    .option('--no-compiler-version', 'Exclude compiler version metadata from output')
    .option('--no-warn-deprecated-code', 'Turn off warnings for deprecated code styles');

  BaseOptions.addAll(buildCommand);

  buildCommand.command('file [infile...]', {isDefault: true})
    .description(`Compile one or more source files or projects ('file' subcommand is default).`)
    .option('--for-publishing', 'Verify that project meets @keymanapp repository requirements')
    .addHelpText('after', `
Supported file types:
* folder: Keyman project in folder
* .kpj: Keyman project
* .kmn: Keyman keyboard
* .xml: LDML keyboard
* .model.ts: Keyman lexical model
* .kps: Keyman keyboard or lexical model package

File lists can be referenced with @filelist.txt.

If no input file is supplied, kmc will build the current folder.`)

    .action(async (filenames: string[], _options: any, commander: any) => {
      const options = commandOptionsToCompilerOptions(commander.optsWithGlobals());
      const callbacks = new NodeCompilerCallbacks(options);

      if(!filenames.length) {
        // If there are no filenames provided, then we are building the current
        // folder ('.') as a project-style build
        filenames.push('.');
      }

      if(!expandFileLists(filenames, callbacks)) {
        process.exit(1);
      }

      for(let filename of filenames) {
        if(!await build(filename, callbacks, options)) {
          // Once a file fails to build, we bail on subsequent builds
          process.exit(1);
        }
      }
    });

  buildCommand
    .command('ldml-test-data <infile>')
    .description('Convert LDML keyboard test .xml to .json')
    .action(buildTestData);

  buildCommand
    .command('windows-package-installer <infile>')
    .description('Build an executable installer for Windows for a Keyman package')
    .option('--msi <msiFilename>', 'Location of keymandesktop.msi')
    .option('--exe <exeFilename>', 'Location of setup.exe')
    .option('--license <licenseFilename>', 'Location of license.txt')
    .option('--title-image [titleImageFilename]', 'Location of title image')
    .option('--app-name [applicationName]', 'Installer property: name of the application to be installed', 'Keyman')
    .option('--start-disabled', 'Installer property: do not enable keyboards after installation completes')
    .option('--start-with-configuration', 'Installer property: start Keyman Configuration after installation completes')
    .action(buildWindowsPackageInstaller);
}

async function build(filename: string, parentCallbacks: NodeCompilerCallbacks, options: CompilerOptions): Promise<boolean> {
  try {
    // TEST: allow command-line simulation of infrastructure fatal errors, and
    // also for unit tests
    if(process.env.SENTRY_CLIENT_TEST_BUILD_EXCEPTION == '1') {
      throw new Error('Test exception from SENTRY_CLIENT_TEST_BUILD_EXCEPTION');
    }

    if(!fs.existsSync(filename)) {
      parentCallbacks.reportMessage(InfrastructureMessages.Error_FileDoesNotExist({filename}));
      return false;
    }

    // Normalize case for the filename and expand the path; this avoids false
    // positive case mismatches on input filenames and glommed paths
    filename = fs.realpathSync.native(filename);

    let builder = null;

    // If infile is a directory, then we treat that as a project and build it
    if(isProject(filename)) {
      builder = new BuildProject();
    } else {
      // Otherwise, if it's one of our known file extensions, we build it
      let extensions: string[] = [];
      builder = buildActivities.find(build => {
        extensions.push(build.sourceExtension);
        return filename.toLowerCase().endsWith(build.sourceExtension);
      });
      if(!builder) {
        parentCallbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotRecognized({filename, extensions: extensions.join(', ')}));
        return false;
      }
    }

    // For builds which refer to a project folder, we'll imply a build of the
    // .kpj, even if it doesn't actually exist, just for clarity.
    let buildFilename = path.resolve(filename);
    if(fs.statSync(filename).isDirectory()) {
      buildFilename = path.join(buildFilename, path.basename(buildFilename) + KeymanFileTypes.Source.Project);
    }
    const relativeFilename = path.relative(process.cwd(), buildFilename).replace(/\\/g, '/');

    const callbacks = new CompilerFileCallbacks(buildFilename, options, parentCallbacks);
    callbacks.reportMessage(InfrastructureMessages.Info_BuildingFile({filename:buildFilename, relativeFilename}));

    let result = await builder.build(filename, callbacks, options);
    result = result && !callbacks.hasFailureMessage();
    if(result) {
      callbacks.reportMessage(builder instanceof BuildProject
        ? InfrastructureMessages.Info_ProjectBuiltSuccessfully({filename:buildFilename, relativeFilename})
        : InfrastructureMessages.Info_FileBuiltSuccessfully({filename:buildFilename, relativeFilename})
      );
    } else {
      if(!callbacks.hasFailureMessage(false)) { // false == check only for error+fatal messages
        callbacks.reportMessage(InfrastructureMessages.Info_WarningsHaveFailedBuild());
      }
      callbacks.reportMessage(builder instanceof BuildProject
        ? InfrastructureMessages.Info_ProjectNotBuiltSuccessfully({filename:buildFilename, relativeFilename})
        : InfrastructureMessages.Info_FileNotBuiltSuccessfully({filename:buildFilename, relativeFilename})
      );
    }

    return result;
  } catch(e) {
    parentCallbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }
}

/**
 * these are exported only for unit tests, do not use
 */
export const unitTestEndpoints = {
  build
};
