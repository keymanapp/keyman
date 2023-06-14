import * as fs from 'fs';
import { Command, Option } from 'commander';
import { NodeCompilerCallbacks } from '../messages/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/messages.js';
import { CompilerBaseOptions, CompilerCallbacks } from '@keymanapp/common-types';
import { AnalyzeOskCharacterUse, AnalyzeOskRewritePua } from '@keymanapp/kmc-analyze';
import { addBaseOptions } from '../util/baseOptions.js';
import { runOnFiles } from '../util/projectReader.js';

interface AnalysisActivityOptions extends CompilerBaseOptions {
  action: 'osk-char-use' | 'osk-rewrite-from-char-use';
  format: 'text' | 'markdown' | 'json';
  base?: string;
  stripDottedCircle?: boolean;
  includeCounts?: boolean;
  mappingFile?: string;
};

export function declareAnalyze(program: Command) {
  addBaseOptions(program
    .command('analyze [infile...]')
    .description('Analyze a source file or files')
  )
    .addOption(new Option('-f, --format <filename>', 'Output file format').choices(['text','markdown','json']).default('text'))
    .addOption(new Option('-a, --action <action>', 'Specify the analysis to run').choices([
      'osk-char-use',
      'osk-rewrite-from-char-use',
    ]).makeOptionMandatory())
    .option('-b, --base', 'First PUA codepoint to use, in hexadecimal', 'F100')
    .option('--include-counts', 'Include number of times each character is referenced', false)
    .option('--strip-dotted-circle', 'Strip U+25CC (dotted circle base) from results', false)
    .option('-m, --mapping-file <filename>', 'Input mapping file (for osk-rewrite-from-char-use)')
    .action(async (filenames: string[], options: any) => {
      if(!filenames.length) {
        // If there are no filenames provided, then we are building the current
        // folder ('.') as a project-style build
        filenames.push('.');
      }

      if(!analyze(filenames, options)) {
        // Once a file fails to build, we bail on subsequent builds
        process.exit(1);
      }
    });
}

async function analyze(filenames: string[], options: AnalysisActivityOptions): Promise<boolean> {
  // Note: we always use logLevel=silent if we write the output to console
  let callbacks = new NodeCompilerCallbacks({logLevel: options.outFile || options.action != 'osk-char-use' ? options.logLevel : 'silent'});

  try {
    switch(options.action) {
      case 'osk-char-use':
        return await analyzeOskCharUse(callbacks, filenames, options);
      case 'osk-rewrite-from-char-use':
        return await analyzeOskRewritePua(callbacks, filenames, options);
      default:
        throw new Error(`Internal error: Invalid analyze action '${options.action}'`);
    }
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }
}

async function analyzeOskCharUse(callbacks: CompilerCallbacks, filenames: string[], options: AnalysisActivityOptions) {
  const analyzer = new AnalyzeOskCharacterUse(callbacks, {
    puaBase: parseInt(options.base, 16),
    stripDottedCircle: options.stripDottedCircle,
    includeCounts: options.includeCounts,
  });

  if(!await runOnFiles(callbacks, filenames, analyzer.analyze.bind(analyzer))) {
    return false;
  }

  let output = analyzer.getStrings(options.format ?? 'text').join('\n');
  if(options.outFile) {
    fs.writeFileSync(options.outFile, output, 'utf8');
  } else {
    process.stdout.write(output + '\n');
  }

  return true;
}

async function analyzeOskRewritePua(callbacks: CompilerCallbacks, filenames: string[], options: AnalysisActivityOptions) {
  const analyzer = new AnalyzeOskRewritePua(callbacks, {
    stripDottedCircle: options.stripDottedCircle,
    mappingFile: options.mappingFile,
  });
  const mapping: any = JSON.parse(callbacks.fs.readFileSync(options.mappingFile, 'UTF-8'));

  return await runOnFiles(callbacks, filenames, async (filename: string): Promise<boolean> => {
    if(!await analyzer.analyze(filename, mapping)) {
      return false;
    }
    for(let filename of Object.keys(analyzer.data)) {
      fs.writeFileSync(filename, analyzer.data[filename]);
    }
    return true;
  });
}
