import { CompilerCallbacks } from '@keymanapp/common-types';
import * as fs from 'fs';
import * as path from 'path';
import ts from 'typescript';
import { setCompilerCallbacks } from './compiler-callbacks.js';

import LexicalModelCompiler from './lexical-model-compiler.js';
import { LexicalModelSource } from './lexical-model.js';
import { ModelCompilerError, ModelCompilerMessageContext, ModelCompilerMessages } from './model-compiler-errors.js';

export { default as LexicalModelCompiler } from './lexical-model-compiler.js';

/**
 * Compiles a model.ts file, using paths relative to its location.
 *
 * @param filename path to model.ts source.
 * @return model source code, or null on error
 */
export function compileModel(filename: string, callbacks: CompilerCallbacks): string {
  setCompilerCallbacks(callbacks);

  try {
    let modelSource = loadFromFilename(filename, callbacks);
    let containingDirectory = path.dirname(filename);

    return (new LexicalModelCompiler)
      .generateLexicalModelCode('<unknown>', modelSource, containingDirectory);
  } catch(e) {
    callbacks.reportMessage(
      e instanceof ModelCompilerError
      ? e.event
      : ModelCompilerMessages.Fatal_UnexpectedException({e:e})
    );
  }
  return null;
}

/**
 * An ECMAScript module as emitted by the TypeScript compiler.
 */
interface ES2015Module {
  /** This is always true. */
  __esModule: boolean;
  'default'?: unknown;
}

/**
 * Loads a lexical model's source module from the given filename.
 *
 * @param filename path to the model source file.
 */
export function loadFromFilename(filename: string, callbacks: CompilerCallbacks): LexicalModelSource {
  setCompilerCallbacks(callbacks);

  let sourceCode = fs.readFileSync(filename, 'utf8');
  // Compile the module to JavaScript code.
  // NOTE: transpile module does a very simple TS to JS compilation.
  // It DOES NOT check for types!
  let compilationOutput = ts.transpile(sourceCode, {
    // Our runtime only supports ES3 with Node/CommonJS modules on Android 5.0.
    // When we drop Android 5.0 support, we can update this to a `ScriptTarget`
    // matrix against target version of Keyman, here and in
    // lexical-model-compiler.ts.
    target: ts.ScriptTarget.ES3,
    module: ts.ModuleKind.CommonJS,
  });
  // Turn the module into a function in which we can inject a global.
  let moduleCode = '(function(exports){' + compilationOutput + '})';

  // Run the module; its exports will be assigned to `moduleExports`.
  let moduleExports: Partial<ES2015Module> = {};
  let module = eval(moduleCode);
  module(moduleExports);

  if (!moduleExports['__esModule'] || !moduleExports['default']) {
    ModelCompilerMessageContext.filename = filename;
    throw new ModelCompilerError(ModelCompilerMessages.Error_NoDefaultExport());
  }

  return moduleExports['default'] as LexicalModelSource;
}
