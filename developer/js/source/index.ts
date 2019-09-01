/*
  index.ts: base file for lexical model compiler.
  TODO: move this to lexical-model-compiler/lexical-model-compiler.ts and remove legacy compile() function
*/

/// <reference path="./lexical-model-compiler/lexical-model.ts" />
/// <reference path="./lexical-model-compiler/model-info-file.ts" />

import * as ts from "typescript";
import KmpCompiler from "./package-compiler/kmp-compiler";
import * as fs from "fs";
import * as path from "path";
import { createTrieDataStructure } from "./lexical-model-compiler/build-trie";
import ModelInfoCompiler from "./model-info-compiler/model-info-compiler";

// The model ID MUST adhere to this pattern:
//                         author           .bcp47            .uniq
const MODEL_ID_PATTERN = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_]*$/;

export default class LexicalModelCompiler {

  /**
   * Compiles a lexical model for the lexical-models repo.
   * TODO: This will be removed once the kmlmc / kmlmp CLI tools
   * are complete.
   * @deprecated
   * @param modelSource: 
   */

  compile(modelSource: LexicalModelSource) {
    //
    // Load the model info file
    //
    let files = fs.readdirSync('../');
    let model_info_file = files.find((f) => !!f.match(/\.model_info$/));

    if(!model_info_file) {
      this.logError('Unable to find .model_info file in parent folder');
      return false;
    }

    let model_id = model_info_file.match(/^(.+)\.model_info$/)[1];
    if(!model_id.match(MODEL_ID_PATTERN)) {
      this.logError(
        `The model identifier '${model_id}' is invalid.\n`+
        `Must be a valid alphanumeric identifier in format (author).(bcp_47).(uniq).\n`+
        `bcp_47 should be underscore (_) separated.`);
      return false;
    }

    //
    // Filename expectations
    //

    const kpsFileName = `../source/${model_id}.model.kps`;
    const kmpFileName = `${model_id}.model.kmp`;
    const modelFileName = `${model_id}.model.js`;
    const modelInfoFileName = `${model_id}.model_info`;
    const sourcePath = '../source';

    //
    // This script is run from folder group/author/bcp47.uniq/build/ folder. We want to
    // verify that author.bcp47.uniq is the same as the model identifier.
    //

    let paths = process.cwd().split(path.sep).reverse();
    if(paths.length < 4 || paths[0] != 'build' || model_id != paths[2] + '.' + paths[1]) {
      this.logError(`Unexpected model path ${paths[2]}.${paths[1]}, does not match model id ${model_id}`);
      return false;
    }

    //
    // Build the compiled lexical model
    //

    let func: string;
    try {
      func = this.generateLexicalModelCode(model_id, modelSource, sourcePath);
    } catch (e) {
      if (e instanceof ModelSourceError) {
        this.logError(e.message);
        return false;
      }
      throw e;
    }

    //
    // Save full model to build folder as Javascript for use in KeymanWeb
    //

    fs.writeFileSync(modelFileName, func);

    //
    // Load .kps file and validate; prepare to create KMP package file
    //

    let kpsString: string = fs.readFileSync(kpsFileName, 'utf8');
    let kmpCompiler = new KmpCompiler();
    let kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsString);

    // Validate the model id from the folder name against the metadata
    // in the .kps file. A package source file must contain at least one
    // model, being the current model being compiled. Currently, the compiler
    // supports only a single model in the .kmp, but conceptually in the future
    // we could support multiple models. Given this, the code checks every
    // listed model rather than just the first.

    if(kmpJsonData.lexicalModels.find((e) => e.id === model_id) === undefined) {
      let ids = kmpJsonData.lexicalModels.map((e) => e.id).join(', ');
      this.logError(`Unable to find matching model ${model_id} in the file ${kpsFileName}; model id(s) found in the package are: ${ids}`);
      return false;
    }

    //
    // Build the KMP package file
    //

    kmpCompiler.buildKmpFile(kmpJsonData, kmpFileName);

    //
    // Write the merged .model_info file
    //

    // 0 = build
    // 1 = bcp47.uniq
    // 2 = author
    // 3 = group
    let groupPath = paths[3];
    let authorPath = paths[2];
    let bcp47Path = paths[1];

    //TODO: model_info.helpLink = model_info.helpLink || ... if source/help/id.php exists?
    let repoSourcePath = [groupPath, authorPath, bcp47Path].join('/');

    (new ModelInfoCompiler).writeMergedModelMetadataFile('../'+modelInfoFileName, '../build/'+modelInfoFileName, model_id, kmpJsonData, repoSourcePath, modelFileName, kmpFileName);

    return true;
  };

  
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
    let compiler = this;

    // TODO: add metadata in comment
    const filePrefix: string = `(function() {\n'use strict';\n`;
    const fileSuffix: string = `})();`;
    let func = filePrefix;

    // Figure out what word breaker the model is using, if any.
    let wordBreakerSpec = getWordBreakingSpec();
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
      } else if (wordBreakerSpec.sources) {
        compiler.logError('class-based word breaker is not officially supported :/');
        let wordBreakerSources: string[] = wordBreakerSpec.sources.map(function(source) {
          return fs.readFileSync(path.join(sourcePath, source), 'utf8');
        });

        wordBreakerSourceCode = this.transpileSources(wordBreakerSources).join('\n');
      }
    }

    function getWordBreakingSpec() {
      if (modelSource.wordBreaker) {
        return modelSource.wordBreaker;
      } else if (modelSource.wordBreaking) {
        compiler.logError('`wordBreaking` will be deleted; please use `wordBreaker` instead!');
        return modelSource.wordBreaking;
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
