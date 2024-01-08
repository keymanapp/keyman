/*
  lexical-model-compiler.ts: base file for lexical model compiler.
*/

import ts from "typescript";
import { createTrieDataStructure } from "./build-trie.js";
import { ModelDefinitions } from "./model-definitions.js";
import {decorateWithJoin} from "./join-word-breaker-decorator.js";
import {decorateWithScriptOverrides} from "./script-overrides-decorator.js";
import { LexicalModelSource, WordBreakerSpec, SimpleWordBreakerSpec } from "./lexical-model.js";
import { ModelCompilerError, ModelCompilerMessageContext, ModelCompilerMessages } from "./model-compiler-errors.js";
import { callbacks, setCompilerCallbacks } from "./compiler-callbacks.js";
import { CompilerCallbacks, CompilerOptions, KeymanCompiler, KeymanCompilerArtifact, KeymanCompilerArtifacts, KeymanCompilerResult } from "@keymanapp/common-types";

/**
 * An ECMAScript module as emitted by the TypeScript compiler.
 */
interface ES2015Module {
  /** This is always true. */
  __esModule: boolean;
  'default'?: unknown;
};

export interface LexicalModelCompilerArtifacts extends KeymanCompilerArtifacts {
  js: KeymanCompilerArtifact;
};

export interface LexicalModelCompilerResult extends KeymanCompilerResult {
  artifacts: LexicalModelCompilerArtifacts;
};

export class LexicalModelCompiler implements KeymanCompiler {

  async init(callbacks: CompilerCallbacks, _options: CompilerOptions): Promise<boolean> {
    setCompilerCallbacks(callbacks);
    return true;
  }

  async run(inputFilename: string, outputFilename?: string): Promise<LexicalModelCompilerResult> {
    try {
      let modelSource = this.loadFromFilename(inputFilename);
      let containingDirectory = callbacks.path.dirname(inputFilename);
      let code = this.generateLexicalModelCode('<unknown>', modelSource, containingDirectory);
      const result: LexicalModelCompilerResult = {
        artifacts: {
          js: {
            data: new TextEncoder().encode(code),
            filename: outputFilename ?? inputFilename.replace(/\.model\.ts$/, '.model.js')
          }
        }
      }
      return result;
    } catch(e) {
      callbacks.reportMessage(
        e instanceof ModelCompilerError
        ? e.event
        : ModelCompilerMessages.Fatal_UnexpectedException({e:e})
      );
      return null;
    }
  }

  async write(artifacts: LexicalModelCompilerArtifacts): Promise<boolean> {
    callbacks.fs.writeFileSync(artifacts.js.filename, artifacts.js.data);
    return true;
  }

  /**
   * Loads a lexical model's source module from the given filename.
   *
   * @param filename path to the model source file.
   */
  public loadFromFilename(filename: string): LexicalModelSource {

    let sourceCode = new TextDecoder().decode(callbacks.loadFile(filename));
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

  /**
   * Returns the generated code for the model that will ultimately be loaded by
   * the LMLayer worker. This code contains all model parameters, and specifies
   * word breakers and auxilary functions that may be required.
   *
   * @param model_id      The model ID. TODO: not sure if this is actually required!
   * @param modelSource   A specification of the model to compile
   * @param sourcePath    Where to find auxilary sources files
   */
  generateLexicalModelCode(model_id: string, modelSource: LexicalModelSource, sourcePath: string) {
    // TODO: add metadata in comment
    const filePrefix: string = `(function() {\n'use strict';\n`;
    const fileSuffix: string = `})();`;
    let func = filePrefix;

    //
    // Emit the model as code and data
    //

    switch(modelSource.format) {
      case "custom-1.0":
        let sources: string[] = modelSource.sources.map(function(source) {
          return new TextDecoder().decode(callbacks.loadFile(callbacks.path.join(sourcePath, source)));
        });
        func += this.transpileSources(sources).join('\n');
        func += `LMLayerWorker.loadModel(new ${modelSource.rootClass}());\n`;
        break;
      case "fst-foma-1.0":
        throw new ModelCompilerError(ModelCompilerMessages.Error_UnimplementedModelFormat({format:modelSource.format}));
      case "trie-1.0":
        // Convert all relative path names to paths relative to the enclosing
        // directory. This way, we'll read the files relative to the model.ts
        // file, rather than the current working directory.
        let filenames = modelSource.sources.map(filename => callbacks.path.join(sourcePath, filename));

        let definitions = new ModelDefinitions(modelSource);

        func += definitions.compileDefinitions();

        // Needs the actual searchTermToKey closure...
        // Which needs the actual applyCasing closure as well.
        func += `LMLayerWorker.loadModel(new models.TrieModel(${
          createTrieDataStructure(filenames, definitions.searchTermToKey)
        }, {\n`;

        let wordBreakerSourceCode = compileWordBreaker(normalizeWordBreakerSpec(modelSource.wordBreaker));
        func += `  wordBreaker: ${wordBreakerSourceCode},\n`;

        // START - the lexical mapping option block
        func += `  searchTermToKey: ${definitions.compileSearchTermToKey()},\n`;

        if(modelSource.languageUsesCasing != null) {
          func += `  languageUsesCasing: ${modelSource.languageUsesCasing},\n`;
        } // else leave undefined.

        if(modelSource.languageUsesCasing) {
          func += `  applyCasing: ${definitions.compileApplyCasing()},\n`;
        }
        // END - the lexical mapping option block.

        if (modelSource.punctuation) {
          func += `  punctuation: ${JSON.stringify(modelSource.punctuation)},\n`;
        }
        func += `}));\n`;
        break;
      default:
        throw new ModelCompilerError(ModelCompilerMessages.Error_UnknownModelFormat({format: modelSource.format}));
    }

    func += fileSuffix;

    return func;
  }

  transpileSources(sources: Array<string>): Array<string> {
    return sources.map((source) => ts.transpileModule(source, {
        compilerOptions: {
          target: ts.ScriptTarget.ES3,
          module: ts.ModuleKind.None,
        }
      }).outputText
    );
  };

};

/**
 * Returns a JavaScript expression (as a string) that can serve as a word
 * breaking function.
 */
function compileWordBreaker(spec: WordBreakerSpec): string {
  let wordBreakerCode = compileInnerWordBreaker(spec.use);

  if (spec.joinWordsAt) {
    wordBreakerCode = compileJoinDecorator(spec, wordBreakerCode);
  }

  if (spec.overrideScriptDefaults) {
    wordBreakerCode = compileScriptOverrides(spec, wordBreakerCode);
  }

  return wordBreakerCode;
}

function compileJoinDecorator(spec: WordBreakerSpec, existingWordBreakerCode: string) {
  // Bundle the source of the join decorator, as an IIFE,
  // like this: (function join(breaker, joiners) {/*...*/}(breaker, joiners))
  // The decorator will run IMMEDIATELY when the model is loaded,
  // by the LMLayer returning the decorated word breaker to the
  // LMLayer model.
  let joinerExpr: string = JSON.stringify(spec.joinWordsAt)
  return `(${decorateWithJoin.toString()}(${existingWordBreakerCode}, ${joinerExpr}))`;
}

function compileScriptOverrides(spec: WordBreakerSpec, existingWordBreakerCode: string) {
  return `(${decorateWithScriptOverrides.toString()}(${existingWordBreakerCode}, '${spec.overrideScriptDefaults}'))`;
}

/**
 * Compiles the base word breaker, that may be decorated later.
 * Returns the source code of a JavaScript expression.
 */
function compileInnerWordBreaker(spec: SimpleWordBreakerSpec): string {
  if (typeof spec === "string") {
    // It must be a builtin word breaker, so just instantiate it.
    return `wordBreakers['${spec}']`;
  } else {
    // It must be a function:
    return spec.toString()
      // Note: the .toString() might just be the property name, but we want a
      // plain function:
      .replace(/^wordBreak(ing|er)\b/, 'function');
  }
}

/**
 * Given a word breaker specification in any of the messy ways,
 * normalizes it to a common form that the compiler can deal with.
 */
function normalizeWordBreakerSpec(wordBreakerSpec: LexicalModelSource["wordBreaker"]): WordBreakerSpec {
  if (wordBreakerSpec == undefined) {
    // Use the default word breaker when it's unspecified
    return { use: 'default' };
  } else if (isSimpleWordBreaker(wordBreakerSpec)) {
    // The word breaker was passed as a literal function; use its source code.
    return { use: wordBreakerSpec };
  } else if (wordBreakerSpec.use) {
    return wordBreakerSpec;
  } else {
    throw new ModelCompilerError(ModelCompilerMessages.Error_UnknownWordBreaker({spec: wordBreakerSpec.toString()}));
  }
}

function isSimpleWordBreaker(spec: WordBreakerSpec | SimpleWordBreakerSpec): spec is SimpleWordBreakerSpec  {
  return typeof spec === "function" || spec === "default" || spec === "ascii";
}
