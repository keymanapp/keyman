import { posix as path } from 'node:path';

import chalk from 'chalk';
import { Command } from 'commander';

import { severityColors, type LinkRef } from './types.js';
import { checkLinks } from './check-link.js';
import { findFiles } from './find-files.js';
import { parseFiles } from './parse-files.js';

const program = new Command();
const command = program
  .description('Markdown link and sanity checker')
  .option('-r, --root <path>', 'Root path to check')
  .option('-v, --verbose', 'Report on external links and warnings')
  .action(run);

program.parse(process.argv);

function run() {
  const root = command.opts().root;
  const verbose = program.opts().verbose;
  const files = findFiles(root);
  const links = parseFiles(root, files);
  const result = checkLinks(root, links);

  for(const file of links) {
    if(file.messages.length) {
      reportMessages(root, result && verbose /* only give verbose output if no errors */, file);
    }
  }

  process.exit(result ? 0 : 1);
}{

}
function reportMessages(root: string, verbose: boolean, file: LinkRef) {
  for(const message of file.messages) {
    if(message.type == 'error' || verbose) {
      process.stdout.write(
          chalk.cyan(path.join(root, file.file)) + ' - ' +
          severityColors[message.type](message.type) + ': ' +
          message.message +
          chalk.grey(' [' + message.token.text + '](' + message.token.href + ')') +
          '\n'
      );
    }
  }
}
