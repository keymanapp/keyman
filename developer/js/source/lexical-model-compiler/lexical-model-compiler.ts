/*
  lexical-model-compiler.ts: base file for lexical model compiler.
*/

/// <reference path="./lexical-model.ts" />
/// <reference path="./model-info-file.ts" />

import * as ts from "typescript";
import * as fs from "fs";
import * as path from "path";
import { createTrieDataStructure, parseWordList, compileWordListCharacterSet } from "./build-trie";

export default class LexicalModelCompiler {

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

    // Figure out what word breaker the model is using, if any.
    let wordBreakerSpec = getWordBreakerSpec();
    let wordBreakerSourceCode: string = null;
    if (wordBreakerSpec) {
      if (typeof wordBreakerSpec === "string") {
        // It must be a builtin word breaker, so just instantiate it.
        wordBreakerSourceCode = `wordBreakers['${wordBreakerSpec}']`;
      } else if (typeof wordBreakerSpec === "function") {
        // The word breaker was passed as a literal function; use its source code.
        wordBreakerSourceCode = wordBreakerSpec.toString()
        // Note: the .toString() might just be the property name, but we want a
        // plain function:
          .replace(/^wordBreak(ing|er)\b/, 'function');
      }
    }

    function getWordBreakerSpec() {
      if (modelSource.wordBreaker) {
        return modelSource.wordBreaker;
      } else {
        return null;
      }
    }

    //
    // Emit the model as code and data
    //

    switch(modelSource.format) {
      case "custom-1.0":
        let sources: string[] = modelSource.sources.map(function(source) {
          return fs.readFileSync(path.join(sourcePath, source), 'utf8');
        });
        func += this.transpileSources(sources).join('\n');
        func += `LMLayerWorker.loadModel(new ${modelSource.rootClass}());\n`;
        break;
      case "fst-foma-1.0":
        throw new ModelSourceError(`Unimplemented model format: ${modelSource.format}`);
      case "trie-1.0":
        // Convert all relative path names to paths relative to the enclosing
        // directory. This way, we'll read the files relative to the model.ts
        // file, rather than the current working directory.
        let filenames = modelSource.sources.map(filename => path.join(sourcePath, filename));

        func += `LMLayerWorker.loadModel(new models.TrieModel(${
          createTrieDataStructure(filenames, modelSource.searchTermToKey)
        }, {\n`;
        if (wordBreakerSourceCode) {
          func += `  wordBreaker: ${wordBreakerSourceCode},\n`;
        }
        if (modelSource.searchTermToKey) {
          func += `  searchTermToKey: ${modelSource.searchTermToKey.toString()},\n`;
        }
        if (modelSource.punctuation) {
          func += `  punctuation: ${JSON.stringify(modelSource.punctuation)},\n`;
        }
        let wordlist = parseWordList(modelSource.sources.join('\n'));
        let charSet = compileWordListCharacterSet(wordlist, modelSource.searchTermToKey);
        func += `  characterSet: ${charSet},\n`;
        func += `}));\n`;
        break;
      default:
        throw new ModelSourceError(`Unknown model format: ${modelSource.format}`);
    }

    //
    // TODO: Load custom wordbreak source files
    //

    func += fileSuffix;

    return func;
  }

  transpileSources(sources: Array<string>): Array<string> {
    return sources.map((source) => ts.transpileModule(source, {
        compilerOptions: { module: ts.ModuleKind.None }
      }).outputText
    );
  };

  logError(s: string) {
    console.error(require('chalk').red(s));
  };
};

export class ModelSourceError extends Error {
}
