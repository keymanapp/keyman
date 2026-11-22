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
import { Converter, ConverterOptions, ConverterResult } from '@keymanapp/kmc-convert';
import { CompilerCallbacks } from '@keymanapp/developer-utils';

export function declareConvert(program: Command) {
  const command = program.command('convert <infile...>');
  BaseOptions.addLogLevel(command);
  BaseOptions.addLogFormat(command);
  command
    // TODO kmc-convert: add more conversions later, then alter text here
    //.description('Convert keyboard source files between formats:')
    .description('convert keyboard source files between formats')
    .addHelpText('after', `
Supported file format conversions:
* macOS (Ukelele) .keylayout -> Keyman .kmn
* Keyman binary .kvk -> Keyman source .kvks
`)
    .requiredOption('-t, --target <target>', 'File extension for output files, e.g. .kmn, .kvks (required)')
    .option('-o, --out-path <path>', 'path where output files will be written (must exist)')
    .action(async (filenames: string[], _options: any, commander: any): Promise<never|void> => {
      const options = commander.optsWithGlobals();
      const callbacks = new NodeCompilerCallbacks({logLevel: options.logLevel});

      for(const filename of filenames) {
        const outputFilename = path.join(
          options.outPath ?? path.dirname(filename),
          path.basename(filename, path.extname(filename)) + options.target
        );

        if(!await doConvert(callbacks, options, filename, outputFilename)) {
          return await exitProcess(1);
        }
      }
    });
}

async function doConvert(
  callbacks: CompilerCallbacks,
  options: ConverterOptions,
  inputFilename: string,
  outputFilename: string,
): Promise<boolean> {
  try {
    const relativeFilename = path.relative(process.cwd(), inputFilename).replace(/\\/g, '/');
    callbacks.reportMessage(InfrastructureMessages.Info_ConvertingFile({filename: inputFilename, relativeFilename, outputBaseFilename: path.basename(outputFilename)}));
    const result = await convert(callbacks, options, inputFilename, outputFilename);
    if(!result) {
      callbacks.reportMessage(InfrastructureMessages.Info_FileNotConvertedSuccessfully({filename:inputFilename, relativeFilename}));
      return false;
    }

    const outputRelativeFilename = path.relative(process.cwd(), result.artifacts.defaultOutput.filename).replace(/\\/g, '/');
    callbacks.reportMessage(InfrastructureMessages.Info_FileConvertedSuccessfully({filename:inputFilename, relativeFilename, outputRelativeFilename}));
    return true;
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }
}

async function convert(
  callbacks: CompilerCallbacks,
  options: ConverterOptions,
  inputFilename: string,
  outputFilename: string,
): Promise<ConverterResult> {
  const converter = new Converter();
  if(!await converter.init(callbacks, options)) {
    return null;
  }

  const result = await converter.run(inputFilename, outputFilename);
  if(!result) {
    return null;
  }

  if(!await converter.write(result.artifacts)) {
    return null;
  }

  return result;
}

/**
 * these are exported only for unit tests, do not use
 */
export const convertUnitTestEndpoints = {
  convert
};