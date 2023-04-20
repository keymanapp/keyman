import * as fs from 'fs';
import { Command } from 'commander';
import { BuildActivityOptions } from './build/BuildActivity.js';
import { buildActivities } from './build/buildActivities.js';
import { BuildProject } from './build/BuildProject.js';

export function declareBuild(program: Command) {
  program
    .command('build [infile...]')
    .description('Build a source file into a final file')
    .option('-d, --debug', 'Include debug information in output')
    .option('-o, --out-file <filename>', 'Override the default path and filename for the output file')
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

async function build(infile: string, options: BuildActivityOptions): Promise<boolean> {
  console.log(`Building ${infile}`);

  if(!fs.existsSync(infile)) {
    // TODO: consolidate errors
    console.error(`File ${infile} does not exist`);
    process.exit(2);
  }

  // If infile is a directory, then we treat that as a project and build it
  if(fs.statSync(infile).isDirectory()) {
    return (new BuildProject()).build(infile, options);
  }

  // Otherwise, if it's one of our known file extensions, we build it
  let extensions: string[] = [];
  for(let build of buildActivities) {
    if(infile.toLowerCase().endsWith(build.sourceExtension)) {
      return build.build(infile, options);
    }
    extensions.push(build.sourceExtension);
  }

  // TODO: consolidate errors
  console.error(`Unrecognised input file ${infile}, expecting ${extensions.join(', ')}, or project folder`);
  process.exit(2);
}
