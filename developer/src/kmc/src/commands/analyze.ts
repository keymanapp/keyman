import * as fs from 'fs';
import { Command, Option } from 'commander';
import { NodeCompilerCallbacks } from '../messages/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/messages.js';
import { CompilerBaseOptions, CompilerCallbacks, KeymanFileTypes } from '@keymanapp/common-types';
import { AnalyzeOskCharacterUse } from '@keymanapp/kmc-analyze';
import { addBaseOptions } from '../util/baseOptions.js';
import { runProject, runProjectFolder } from '../util/projectReader.js';

interface AnalysisActivityOptions extends CompilerBaseOptions {
  action: 'osk-char-use';
  format: 'text' | 'markdown' | 'json';
  base?: string;
  stripDottedCircle?: boolean;
  includeCounts?: boolean;
};

export function declareAnalyze(program: Command) {
  addBaseOptions(program
    .command('analyze [infile...]')
    .description('Analyze a source file or files')
  )
    .addOption(new Option('-f, --format <filename>', 'Output file format').choices(['text','markdown','json']).default('text'))
    .addOption(new Option('-a, --action <action>', 'Specify the analysis to run').choices(['osk-char-use']).makeOptionMandatory())
    .option('-b, --base', 'First PUA codepoint to use, in hexadecimal', 'F100')
    .option('--include-counts', 'Include number of times each character is referenced', false)
    .option('--strip-dotted-circle', 'Strip U+25CC (dotted circle base) from results', false)
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
  let callbacks = new NodeCompilerCallbacks({logLevel: options.outFile ? options.logLevel : 'silent'});

  try {
    switch(options.action) {
      case 'osk-char-use':
        return await analyzeOskCharUse(callbacks, filenames, options);
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

  async function analyzeFile(filename: string) {
    return await analyzer.analyze(filename);
  }

  for(let filename of filenames) {
    if(!fs.existsSync(filename)) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileDoesNotExist({filename}));
      continue;
    }

    // If infile is a directory, then we treat that as a project and build it
    if(fs.statSync(filename).isDirectory()) {
      if(!await runProjectFolder(callbacks, filename, analyzeFile)) {
        return false;
      }
    } else if(KeymanFileTypes.sourceTypeFromFilename(filename) == KeymanFileTypes.Source.Project) {
      if(!await runProject(callbacks, filename, analyzeFile)) {
        return false;
      }
    } else {
      if(!await analyzeFile(filename)) {
        return false;
      }
    }
  }

  let output = analyzer.getStrings(options.format ?? 'text').join('\n');
  if(options.outFile) {
    fs.writeFileSync(options.outFile, output, 'utf8');
  } else {
    // TODO: when logging to console we need to be 'quiet' for everything else
    process.stdout.write(output + '\n');
  }

  return true;
}

