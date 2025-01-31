// import * as path from 'path';
import { Command } from 'commander';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { InfrastructureMessages } from '../messages/infrastructureMessages.js';
import { BaseOptions } from '../util/baseOptions.js';
import { exitProcess } from '../util/sysexits.js';
import { GeneratorOptions, KeymanKeyboardGenerator, LdmlKeyboardGenerator, LexicalModelGenerator } from '@keymanapp/kmc-generate';
import { CompilerCallbacks, KeymanCompiler } from '@keymanapp/developer-utils';

/* c8 ignore start */

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
      (value, previous) => previous.concat([value]), [])
    .option('-o, --out-path <path>', 'Output path (may exist)')
    .option('-n, --name <name>', 'Keyboard descriptive name')
    .option('-c, --copyright <copyright-name>', 'Copyright holder')
    .option('-v, --version <version-string>', 'Keyboard version', '1.0')
    .option('-L, --language-tag <bcp-47 tag>', 'BCP-47 language tag',
      (value, previous) => previous.concat([value]), [])
    .option('-a, --author <author-name>', 'Name of keyboard author')
    // TODO-GENERATE: icon support
    // .option('-i, --icon', 'Include a generated icon', true)
    .option('-d, --description <description>', 'Short description of the project, Markdown')
    .action(generateKmnKeyboard);
}

function declareGenerateLdmlKeyboard(command: Command) {
  const ldmlSubCommand = command.command('ldml-keyboard <id>');
  BaseOptions.addLogLevel(ldmlSubCommand);
  ldmlSubCommand
    .description('Generate an LDML .xml keyboard project')
    .option('-t, --target <target>', 'Target platforms',
      (value, previous) => previous.concat([value]), [])
    .option('-o, --out-path <path>', 'Output path (may exist)')
    .option('-n, --name <name>', 'Keyboard descriptive name')
    .option('-c, --copyright <copyright-name>', 'Copyright holder') /* © yyyy <copyright-name> */
    .option('-v, --version <version-string>', 'Keyboard version', '1.0.0')
    .option('-L, --language-tag <bcp-47 tag>', 'BCP-47 language tag',
      (value, previous) => previous.concat([value]), [])
    .option('-a, --author <author-name>', 'Name of keyboard author')
    .option('-d, --description <description>', 'Short description of the project, Markdown')
    .action(generateLdmlKeyboard);
}

function declareGenerateLexicalModel(command: Command) {
  const modelSubCommand = command.command('lexical-model <id>');
  BaseOptions.addLogLevel(modelSubCommand);
  modelSubCommand
    .description('Generate a wordlist lexical model project')
    .option('-o, --out-path <path>', 'Output path (may exist)')
    .option('-n, --name <name>', 'Lexical model descriptive name')
    .option('-c, --copyright <copyright-name>', 'Copyright holder') /* © yyyy <copyright-name> */
    .option('-v, --version <version-string>', 'Lexical model version', '1.0')
    .option('-L, --language-tag <bcp-47 tag>', 'BCP-47 language tag',
      (value, previous) => previous.concat([value]), [])
    .option('-a, --author <author-name>', 'Name of lexical model author')
    .option('-d, --description <description>', 'Short description of the project, Markdown')
    .action(generateLexicalModel);

    //TODO:required opts
}

function commanderOptionsToGeneratorOptions(id: string, options: any): GeneratorOptions {
  const result: GeneratorOptions = {
    // TODO-GENERATE: icon support
    // icon: options.icon ?? true,
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

const generateKmnKeyboard = async (ids: string | string[], _options: any, commander: any) =>
  generate(new KeymanKeyboardGenerator(), ids, commander);
const generateLdmlKeyboard = async (ids: string | string[], _options: any, commander: any) =>
  generate(new LdmlKeyboardGenerator(), ids, commander);
const generateLexicalModel = async (ids: string | string[], _options: any, commander: any) =>
  generate(new LexicalModelGenerator(), ids, commander);

async function generate(generator: KeymanCompiler, ids: string | string[], commander: any): Promise<never|void> {
  const commanderOptions = commander.optsWithGlobals();
  const callbacks = new NodeCompilerCallbacks({logLevel: commanderOptions.logLevel ?? 'info'});
  if(!await doGenerate(callbacks, generator, ids, commanderOptions)) {
    return await exitProcess(1);
  }
}

/* c8 ignore stop */

async function doGenerate(callbacks: CompilerCallbacks, generator: KeymanCompiler, ids: string | string[], commanderOptions: any): Promise<boolean> {
  const id = ids;

  if(!id || typeof id != 'string') {
    // Note that commander can pass an array for the ids parameter, so we
    // constrain here
    callbacks.reportMessage(InfrastructureMessages.Error_GenerateRequiresId());
    return false;
  }

  const options = commanderOptionsToGeneratorOptions(id, commanderOptions);

  try {
    if(!await generator.init(callbacks, options)) {
      // errors will have been reported by the generator
      return false;
    }
    const result = await generator.run(id); // note: id is currently ignored here
    if(!result) {
      // errors will have been reported by the generator
      return false;
    }
    if(!await generator.write(result.artifacts)) {
      // errors will have been reported by the generator
      return false;
    }
  } catch(e) {
    /* c8 ignore next 3 */
    callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
    return false;
  }

  return true;
}

/** @internal */
export const unitTestEndpoints = {
  doGenerate,
}