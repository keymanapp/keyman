/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Declares the 'convert' command line action
 */
import * as path from 'path';
import { Command } from 'commander';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { BaseOptions } from '../util/baseOptions.js';
import { exitProcess } from '../util/sysexits.js';
import { Converter, ConverterOptions } from '@keymanapp/kmc-convert';
import { CompilerCallbacks } from '@keymanapp/developer-utils';

export function declareConvert(program: Command) {
  let command = program.command('convert <infile...>');
  BaseOptions.addLogLevel(command);
  BaseOptions.addLogFormat(command);
  command
    .description('Convert keyboard source files between formats')
    .option('-p, --out-path <path>', 'Path for output files')
    .requiredOption('-t, --out-type <type>', 'File extension for output files, e.g. .kmn, required')
    .action(async (filenames: string[], _options: any, commander: any): Promise<never|void> => {
      const options = commander.optsWithGlobals();
      const callbacks = new NodeCompilerCallbacks({logLevel: options.logLevel});

      for(const filename of filenames) {
        const outputFilename = path.join(
          options.outPath ?? path.dirname(filename),
          path.basename(filename, path.extname(filename)) + options.outType
        );

        if(!await convert(callbacks, options, filename, outputFilename)) {
          return await exitProcess(1);
        }
      }
    });
}

async function convert(
  callbacks: CompilerCallbacks,
  options: ConverterOptions,
  inputFilename: string,
  outputFilename: string,
): Promise<boolean> {
  try {
    const converter = new Converter();
    if(!await converter.init(callbacks, options)) {
      return false;
    }

    const result = await converter.run(inputFilename, outputFilename);
    if(!result) {
      return false;
    }

    if(!await converter.write(result.artifacts)) {
      return false;
    }
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }

  return true;
}

/**
 * these are exported only for unit tests, do not use
 */
export const convertUnitTestEndpoints = {
  convert
};