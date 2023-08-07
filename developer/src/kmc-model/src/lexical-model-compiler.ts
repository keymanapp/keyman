/*
  lexical-model-compiler.ts: base file for lexical model compiler.
*/

import * as ts from "typescript";
import { createTrieDataStructure } from "./build-trie.js";
import { ModelDefinitions } from "./model-definitions.js";
import {decorateWithJoin} from "./join-word-breaker-decorator.js";
import {decorateWithScriptOverrides} from "./script-overrides-decorator.js";
import { LexicalModelSource, WordBreakerSpec, SimpleWordBreakerSpec } from "./lexical-model.js";
import { ModelCompilerError, ModelCompilerMessages } from "./model-compiler-errors.js";
import { callbacks, setCompilerCallbacks } from "./compiler-callbacks.js";
import { CompilerCallbacks } from "@keymanapp/common-types";

export default class LexicalModelCompiler {

  constructor(callbacks: CompilerCallbacks) {
    setCompilerCallbacks(callbacks);
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
