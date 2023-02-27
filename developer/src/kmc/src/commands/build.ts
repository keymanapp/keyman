import { Command } from 'commander';
import { buildPackage } from '../activities/buildPackage.js';
import { buildKmnKeyboard } from '../activities/buildKmnKeyboard.js';
import { buildLdmlKeyboard } from '../activities/buildLdmlKeyboard.js';
import { buildModel } from '../activities/buildModel.js';
import { buildProject } from '../activities/buildProject.js';

export interface BuildCommandOptions {
  debug?: boolean;
  outFile?: string;
  compilerVersion?: boolean;
};

export function declareBuild(program: Command) {
  program
    .command('build [infile...]')
    .description('Build a source file into a final file')
    .option('-d, --debug', 'Include debug information in output')
    .option('-o, --out-file <filename>', 'where to save the resulting .kmx file')
    .option('--no-compiler-version', 'Exclude compiler version metadata from output')
    .action((infiles: string[], options: any) => {
      let p = [];
      if(!infiles.length) {
        console.debug('Assuming infile == .');
        p.push(build('.', options));
      }
      for(let infile of infiles) {
        p.push(build(infile, options));
      }
      return Promise.all(p).then();
    });
}

async function build(infile: string, options: BuildCommandOptions): Promise<boolean> {
  console.log(`Building ${infile}`);

  if(infile.endsWith('.xml')) {
    return buildLdmlKeyboard(infile, options);
  }

  if(infile.endsWith('.kmn')) {
    return buildKmnKeyboard(infile, options);
  }

  if(infile.endsWith('.kps')) {
    return buildPackage(infile, options);
  }

  if(infile.endsWith('.model.ts')) {
    return buildModel(infile, options);
  }

  if(infile.endsWith('.kpj')) {
    return buildProject(infile, options);
  }
/*
  if(fs.statSync(infile).isDirectory()) {
    return buildProjectFolder(infile, options);
  }
*/

  console.error(`Unrecognised input file ${infile}, expecting .xml, .kmn, .kps, .model.ts, .kpj, or project folder`);
  process.exit(2);
}
