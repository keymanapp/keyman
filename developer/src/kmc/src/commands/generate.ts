// import * as path from 'path';
import { Command } from 'commander';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { BaseOptions } from '../util/baseOptions.js';
import { exitProcess } from '../util/sysexits.js';
import { GeneratorOptions, KeymanKeyboardGenerator, LdmlKeyboardGenerator, LexicalModelGenerator } from '@keymanapp/kmc-generate';
import { KeymanCompiler } from '@keymanapp/common-types';


export function declareGenerate(program: Command) {
  const command = program.command('generate');
  declareGenerateKmnKeyboard(command);
  declareGenerateLdmlKeyboard(command);
  declareGenerateLexicalModel(command);
}

function declareGenerateKmnKeyboard(command: Command) {
  const subCommand = command.command('keyman-keyboard <id>');
  BaseOptions.addLogLevel(subCommand);
  subCommand
    .description('Generate a .kmn keyboard project')
    .option('-t, --target <target>', 'Target platforms',
      (value, previous) => previous.concat([value]), ['any'])
    .option('--out-path, -o <path>', 'Output path (may exist)')
    .option('--name, -n <name>', 'Keyboard descriptive name')
    .option('--copyright, -c <copyright-name>', 'Copyright holder')
    .option('--version, -v <version-string>', 'Keyboard version', '1.0')
    .option('--language-tag, -L <bcp-47 tag>', 'BCP-47 language tag',
      (value, previous) => previous.concat([value]), [])
    .option('--author, -a <author-name>', 'Name of keyboard author')
    .option('--icon', 'Include a generated icon', true)
    .option('--description', 'Short description of the project, Markdown')
    .action(generateKmnKeyboard);
}

function declareGenerateLdmlKeyboard(command: Command) {
  const ldmlSubCommand = command.command('ldml-keyboard <id>');
  BaseOptions.addLogLevel(ldmlSubCommand);
  ldmlSubCommand
    .description('Generate an LDML .xml keyboard project')
    .option('-o, --out-path <path>', 'Output path (may exist)')
    .option('-n, --name <name>', 'Keyboard descriptive name')
    .option('-c, --copyright <copyright-name>', 'Copyright holder') /* © yyyy <copyright-name> */
    .option('-v, --version <version-string>', 'Keyboard version', '1.0')
    .option('-L, --language-tag <bcp-47 tag>', 'BCP-47 language tag',
      (value, previous) => previous.concat([value]), [])
    .option('-a, --author <author-name>', 'Name of keyboard author')
    .option('--description', 'Short description of the project, Markdown')
    .action(generateLdmlKeyboard);
}

function declareGenerateLexicalModel(command: Command) {
  const modelSubCommand = command.command('lexical-model <id>');
  BaseOptions.addLogLevel(modelSubCommand);
  modelSubCommand
    .description('Generate a wordlist lexical model project')
    .option('-o, --out-path <path>', 'Output path (may exist)')
    .option('-n, --name <name>', 'Keyboard descriptive name')
    .option('-c, --copyright <copyright-name>', 'Copyright holder') /* © yyyy <copyright-name> */
    .option('-v, --version <version-string>', 'Keyboard version', '1.0')
    .option('-L, --language-tag <bcp-47 tag>', 'BCP-47 language tag',
      (value, previous) => previous.concat([value]), [])
    .option('--author, -a <author-name>', 'Name of keyboard author')
    .option('--description', 'Short description of the project, Markdown')
    .action(generateLexicalModel);

    //TODO:required opts
}

function commanderOptionsToGeneratorOptions(id: string, options: any): GeneratorOptions {
  const result: GeneratorOptions = {
    icon: options.icon ?? true,
    id,
    languageTags: options.languageTag ?? [],
    name: options.name ?? id,
    outPath: options.outPath ?? '.',
    targets: options.target ?? ['any'],
    version: options.version ?? '1.0',
    author: options.author ?? id,
    copyright: options.copyright ?? options.author ?? id,
    logLevel: options.logLevel ?? 'info',
    description: options.description ?? '',
  };
  return result;
}

const generateKmnKeyboard = async (ids: string[], _options: any, commander: any) =>
  generate(new KeymanKeyboardGenerator(), ids, commander);
const generateLdmlKeyboard = async (ids: string[], _options: any, commander: any) =>
  generate(new LdmlKeyboardGenerator(), ids, commander);
const generateLexicalModel = async (ids: string[], _options: any, commander: any) =>
  generate(new LexicalModelGenerator(), ids, commander);

async function generate(
  generator: KeymanCompiler,
  ids: string | string[],
  commander: any
): Promise<never|void> {
  if(typeof ids != 'string') {
    //TODO vvv
    console.error('only 1 id may be specified');
    return await exitProcess(1);
  }

  const id = ids;
  const commanderOptions = commander.optsWithGlobals();
  const options = commanderOptionsToGeneratorOptions(id, commanderOptions);
  if(!id) {
    //TODO vvv
    console.error('id must be specified');
    return await exitProcess(1);
  }

  let callbacks = new NodeCompilerCallbacks({logLevel: options.logLevel});
  try {
    if(!await generator.init(callbacks, options)) {
      return await exitProcess(1);
    }
    const result = await generator.run(id); // note: id is currently ignored here
    if(!result) {
      return await exitProcess(1);
    }
    if(!await generator.write(result.artifacts)) {
      return await exitProcess(1);
    }
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return await exitProcess(1);
  }
}
