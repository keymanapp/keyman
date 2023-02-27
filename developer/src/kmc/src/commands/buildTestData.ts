

import { Command } from 'commander';
import { buildTestData } from '../activities/buildTestData.js';

export function declareBuildTestData(program: Command) {
  program
    .command('build-test-data <infile>')
    .description('Convert keyboard test .xml to .json')
    .option('-o, --out-file <filename>', 'where to save the resulting .json file')
    .action(buildTestData);
}


