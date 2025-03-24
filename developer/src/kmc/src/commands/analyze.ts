import * as fs from 'fs';
import * as path from 'path';
import { Command, Option } from 'commander';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { CompilerBaseOptions, CompilerCallbacks } from '@keymanapp/developer-utils';
import { AnalyzeOskCharacterUse, AnalyzeOskRewritePua } from '@keymanapp/kmc-analyze';
import { BaseOptions } from '../util/baseOptions.js';
import { runOnFiles } from '../util/projectRunner.js';
import { exitProcess } from '../util/sysexits.js';
import { commanderOptionsToBaseOptions } from '../util/extendedCompilerOptions.js';

interface AnalysisActivityOptions extends CompilerBaseOptions {
  base?: string;
  stripDottedCircle?: boolean;
  includeCounts?: boolean;
  mappingFile?: string;
  inputMappingFile?: string;
};

export function declareAnalyze(program: Command) {
  let command = program.command('analyze');
  declareOskCharUse(command);
  declareOskRewrite(command);
}

function declareOskCharUse(command: Command) {
  let subCommand = command.command('osk-char-use [infile...]');
  BaseOptions.addAll(subCommand);
  subCommand
    .description('Analyze On Screen Keyboards for character usage')
    .option('-b, --base', 'First PUA codepoint to use, in hexadecimal (default F100)', 'F100')
    .option('--include-counts', 'Include number of times each character is referenced', false)
    .option('--strip-dotted-circle', 'Strip U+25CC (dotted circle base) from results', false)
    .option('-i, --input-mapping-file <filename>', 'Merge result file with existing mapping file')
    .addOption(new Option('-m, --mapping-file <filename>', 'Result file to write to (.json, .md, or .txt)').makeOptionMandatory())
    .action(async (filenames: string[], _options: any, commander: any): Promise<never|void> => {
      const options = commander.optsWithGlobals();
      if(!filenames.length) {
        // If there are no filenames provided, then we are building the current
        // folder ('.') as a project-style build
        filenames.push('.');
      }

      if(!await analyze(analyzeOskCharUse, filenames, options)) {
        // Once a file fails to build, we bail on subsequent builds
        return await exitProcess(1);
      }
    });

}

function declareOskRewrite(command: Command) {
  let subCommand = command.command('osk-rewrite-from-char-use [infile...]');
  subCommand
    .description('Rewrite On Screen Keyboard files from source mapping; given file.ext, output is written to file-pua.ext')
    .addOption(new Option('-m, --mapping-file <filename>', 'JSON mapping file to read from').makeOptionMandatory())
    .action(async (filenames: string[], options: any) => {
      if(!filenames.length) {
        // If there are no filenames provided, then we are building the current
        // folder ('.') as a project-style build
        filenames.push('.');
      }

      if(!await analyze(analyzeOskRewritePua, filenames, options)) {
        // Once a file fails to build, we bail on subsequent builds
        return await exitProcess(1);
      }

      return null;
    });
}

function commanderOptionsToAnalyzeOptions(options: any): AnalysisActivityOptions {
  // TODO: split options per sub-command
  const result: AnalysisActivityOptions = {
    ...commanderOptionsToBaseOptions(options),
    // AnalysisActivityOptions
    base: options.base,
    includeCounts: options.includeCounts ?? false,
    inputMappingFile: options.inputMappingFile,
    mappingFile: options.mappingFile,
    stripDottedCircle: options.stripDottedCircle ?? false,
  };
  return result;
}

async function analyze(action: (callbacks: CompilerCallbacks, filenames: string[], options: AnalysisActivityOptions)=>Promise<boolean>,
    filenames: string[], commanderOptions: any): Promise<boolean> {
  const options = commanderOptionsToAnalyzeOptions(commanderOptions);
  let callbacks = new NodeCompilerCallbacks(options);
  try {
    return await action(callbacks, filenames, options);
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }
}

async function analyzeOskCharUse(callbacks: CompilerCallbacks, filenames: string[], options: AnalysisActivityOptions) {
  const format = path.extname(options.mappingFile);
  if(format != '.txt' && format != '.json' && format != '.md') {
    callbacks.reportMessage(InfrastructureMessages.Error_UnknownFileFormat({format}));
    return false;
  }

  const analyzer = new AnalyzeOskCharacterUse(callbacks, {
    puaBase: parseInt(options.base, 16),
    stripDottedCircle: options.stripDottedCircle,
    includeCounts: options.includeCounts,
    mergeMapFile: options.inputMappingFile
  });

  if(!await runOnFiles(callbacks, filenames, analyzer.analyze.bind(analyzer))) {
    return false;
  }

  const output = analyzer.getStrings(format).join('\n');
  fs.writeFileSync(options.mappingFile, output, 'utf8');
  return true;
}

async function analyzeOskRewritePua(callbacks: CompilerCallbacks, filenames: string[], options: AnalysisActivityOptions) {
  const analyzer = new AnalyzeOskRewritePua(callbacks);
  const mapping: any = JSON.parse(fs.readFileSync(options.mappingFile, 'utf-8'));

  return await runOnFiles(callbacks, filenames, async (filename: string): Promise<boolean> => {
    if(!await analyzer.analyze(filename, mapping)) {
      return false;
    }
    const mappedFilenames = Object.keys(analyzer.data);
    for(let filename of mappedFilenames) {
      const ext = path.extname(filename);
      const outputFilename = filename.substring(0, filename.length - ext.length) + '-pua' + ext;
      fs.writeFileSync(outputFilename, analyzer.data[filename]);
    }
    return true;
  });
}

/**
 * these are exported only for unit tests, do not use
 */
export const analyzeUnitTestEndpoints = {
  analyzeOskCharUse
};