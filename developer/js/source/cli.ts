#!/usr/bin/env node
/**
 * kmlmc - Keyman Lexical Model Compiler
 */
/// <reference path="lexical-model-compiler/lexical-model.ts" />

import * as ts from 'typescript';
import * as fs from 'fs';

import LexicalModelCompiler from "./";

/**
 * Exit codes defined in <sysexits.h>:
 * https://www.freebsd.org/cgi/man.cgi?query=sysexits&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html
 */
const enum SysExits {
  EX_USAGE = 64,
  EX_DATAERR = 65,
};

/**
 * Loads a lexical model's source module from the given filename.
 * @param filename path to the model source file.
 */
function loadFromFilename(filename: string): LexicalModelSource {
  let sourceCode = fs.readFileSync(filename, 'utf8');

  // Compile the module to JavaScript code.
  // NOTE: transpile module does a very simple TS to JS compilation.
  // It DOES NOT check for types!
  let compilationOutput = ts.transpile(sourceCode, {
    // Our runtime should support ES6 with Node/CommonJS modules.
    target: ts.ScriptTarget.ES2015,
    module: ts.ModuleKind.CommonJS,
  });

  // Turn the module into a function in which we can inject a global.
  let moduleCode = '(function(exports){' + compilationOutput + '})';

  // Run the module; its exports will be assigned to `moduleExports`.
  let moduleExports = {};
  let module = eval(moduleCode);
  module(moduleExports);

  if (!moduleExports['__esModule'] || !moduleExports['default']) {
    console.error(`Model source '${filename}' does have a default export. Did you remember to write \`export default source;\`?`);
    process.exit(SysExits.EX_DATAERR);
  }

  return moduleExports['default'] as LexicalModelSource;
}

if (process.argv.length < 3) {
  console.error('Must provide a lexical model source file.');
  process.exit(SysExits.EX_USAGE);
}

let o = loadFromFilename(process.argv[2]);
let code = (new LexicalModelCompiler).generateLexicalModelCode('<unknown>', o, '.');
console.log(code);
