import * as fs from 'fs';
import { Command, Option } from 'commander';
import { NodeCompilerCallbacks } from '../messages/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/messages.js';
import { CompilerCallbacks } from '@keymanapp/common-types';
import { AnalyzeOskCharacterUse } from '@keymanapp/kmc-analyze';

interface AnalysisActivityOptions {
  action: 'osk-char-use';
  outFile?: string;
  format: 'text' | 'markdown' | 'json';
  quiet?: boolean;
};

export function declareAnalyze(program: Command) {
  program
    .command('analyze [infile...]')
    .description('Analyze a source file or files')
    .option('-o, --out-file <filename>', 'Override the default path and filename for the output file')
    .addOption(new Option('-f, --format <filename>', 'Output file format').choices(['text','markdown','json']).default('text'))
    .addOption(new Option('-a, --action <action>', 'Specify the analysis to run').choices(['osk-char-use']).makeOptionMandatory())
    .action(async (filenames: string[], options: any) => {
      if(!filenames.length) {
        // If there are no filenames provided, then we are building the current
        // folder ('.') as a project-style build
        filenames.push('.');
      }

      if(!analyze(filenames, options)) {
        // Once a file fails to build, we bail on subsequent builds
        // TODO: is this the most appropriate semantics?
        process.exit(1);
      }
    });
}

async function analyze(filenames: string[], options: AnalysisActivityOptions): Promise<boolean> {
  // TODO: refactor this silent vs quiet and object init for NodeCompilerCallbacks i.e. {silent:...}
  let callbacks = new NodeCompilerCallbacks(options.quiet || options.outFile ? false : true);

  try {
    // callbacks.reportMessage(InfrastructureMessages.Info_AnalyzingFile({filename}));

    switch(options.action) {
      case 'osk-char-use':
        return await analyzeOskCharUse(callbacks, filenames, options);
      default:
        throw new Error('Invalid action'); // TODO: ReportMessage
    }
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }
}

async function analyzeOskCharUse(callbacks: CompilerCallbacks, filenames: string[], options: AnalysisActivityOptions) {
  const analyzer = new AnalyzeOskCharacterUse(callbacks);

  for(let filename of filenames) {
    if(!fs.existsSync(filename)) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileDoesNotExist({filename}));
      // TODO: is this the best action here?
      return false;
    }

    // If infile is a directory, then we treat that as a project and build it
    if(fs.statSync(filename).isDirectory()) {
      await analyzer.analyzeProjectFolder(filename);
    }
    else {
      await analyzer.analyze([filename]);
    }
  }

  let output = analyzer.getStrings(options.format ?? 'text').join('\n');
  if(options.outFile) {
    fs.writeFileSync(options.outFile, output, 'utf8');
  } else {
    // TODO: do we need an alternate way of emitting to console here?
    // TODO: when logging to console we need to be 'quiet' for everything else
    console.log(output);
  }

  return true;
}
