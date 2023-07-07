import { Command } from 'commander';
import { buildTestData } from './buildTestData/index.js';
import { BaseOptions } from '../util/baseOptions.js';

export function declareBuildTestData(program: Command) {
  BaseOptions.addAll(program
    .command('build-test-data <infile>')
    .description('Convert keyboard test .xml to .json')
  )
    .action(buildTestData);
}


