/*
  index.ts: base file for lexical model compiler.
*/

/// <reference path="./lexical-model-compiler/lexical-model.ts" />
/// <reference path="./lexical-model-compiler/model-info-file.ts" />

import * as ts from "typescript";
import KmpCompiler from "./package-compiler/kmp-compiler";
import * as fs from "fs";
import * as path from "path";
import { createWordListDataStructure, createTrieDataStructure } from "./lexical-model-compiler/build-trie";

// The model ID MUST adhere to this pattern:
//                         author           .bcp47            .uniq
const MODEL_ID_PATTERN = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_]*$/;

export default class LexicalModelCompiler {
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

    /*
     * Model info looks like this:
     *
     *  {
     *    "name": "Example Template Model"
     *    "license": "mit",
     *    "version": "1.0.0",
     *    "languages": ["en"],
     *    "authorName": "Example Author",
     *    "authorEmail": "nobody@example.com",
     *    "description": "Example wordlist model"
     *  }
     * 
     * For full documentation, see:
     * https://help.keyman.com/developer/cloud/model_info/1.0/
     */
    let model_info: ModelInfoFile = JSON.parse(fs.readFileSync('../'+model_info_file, 'utf8'));

    //
    // Filename expectations
    //
    const kpsFileName = `../source/${model_id}.model.kps`;
    const kmpFileName = `${model_id}.model.kmp`;
    const modelFileName = `${model_id}.model.js`;
    const modelInfoFileName = `${model_id}.model_info`;
    const sourcePath = '../source';

    const minKeymanVersion = '12.0';

    //
    // Validate the model ID.
    //

    //
    // This script is run from folder group/author/bcp47.uniq/build/ folder. We want to
    // verify that author.bcp47.uniq is the same as the model identifier.
    //

    let paths = process.cwd().split(path.sep).reverse();
    if(paths.length < 4 || paths[0] != 'build' || model_id != paths[2] + '.' + paths[1]) {
      this.logError(`Unexpected model path ${paths[2]}.${paths[1]}, does not match model id ${model_id}`);
      return false;
    }

    // 0 = build
    // 1 = bcp47.uniq
    // 2 = author
    // 3 = group
    let groupPath = paths[3];
    let authorPath = paths[2];
    let bcp47Path = paths[1];

    //
    // Build the compiled lexical model
    //

    let sources: string[] = modelSource.sources.map(function(source) {
      return fs.readFileSync(path.join(sourcePath, source), 'utf8');
    });

    let oc: LexicalModelCompiled = {id: model_id, format: modelSource.format};

    // TODO: add metadata in comment
    const filePrefix: string = `(function() {\n'use strict';\n`;
    const fileSuffix: string = `})();`;
    let func = filePrefix;

    let wordBreakingSource: string = null;

    // Figure out what word breaker the model is using, if any.
    if (modelSource.wordBreaking) {
      if (typeof modelSource.wordBreaking === "string") {
        // It must be a builtin word breaker, so just instantiate it.
        wordBreakingSource = `wordBreakers['${modelSource.wordBreaking}']`;
      } else if (modelSource.wordBreaking.sources) {
        let wordBreakingSources: string[] = modelSource.wordBreaking.sources.map(function(source) {
          return fs.readFileSync(path.join(sourcePath, source), 'utf8');
        });

        wordBreakingSource = this.transpileSources(wordBreakingSources).join('\n');
      }
    }

    //
    // Emit the model as code and data
    //

    switch(modelSource.format) {
      case "custom-1.0":
        func += this.transpileSources(sources).join('\n');
        func += `LMLayerWorker.loadModel(new ${modelSource.rootClass}());\n`;
        break;
      case "fst-foma-1.0":
        (oc as LexicalModelCompiledFst).fst = Buffer.from(sources.join('')).toString('base64');
        this.logError('Unimplemented model format '+modelSource.format);
        return false;
      case "trie-1.0":
        func += `var model = {};\n`;
        func += `model.backingData = ${createWordListDataStructure(sources)};\n`;
        func += `LMLayerWorker.loadModel(new models.WordListModel(model.backingData`;
        if (wordBreakingSource) {
          func += `, {wordBreaking: ${wordBreakingSource}}`;
        }
        func += `));\n`;
        break;
      case 'trie-2.0':
        func += `LMLayerWorker.loadModel(new models.TrieModel(${
          createTrieDataStructure(sources)
        }, }`;
        if (wordBreakingSource) {
          func += `  wordBreaking: ${wordBreakingSource},`;
        }
        if (modelSource.searchTermToKey) {
          func += `  searchTermToKey: ${modelSource.searchTermToKey.toString()},`;
        }
        func += `}));\n`;
        break;
      default:
        this.logError('Unknown model format '+modelSource.format);
        return false;
    }

    //
    // Load custom wordbreak source files
    //


    func += fileSuffix;

    // Save full model to build folder as Javascript for use in KeymanWeb

    fs.writeFileSync(modelFileName, func);

    //
    // Create KMP file
    //

    let kpsString: string = fs.readFileSync(kpsFileName, 'utf8');
    let kmpCompiler = new KmpCompiler();
    let kmpJsonData = kmpCompiler.transformKpsToKmpObject(model_id, kpsString);
    kmpCompiler.buildKmpFile(kmpJsonData, kmpFileName);

    //
    // Build merged .model_info file
    // https://api.keyman.com/schemas/model_info.source.json and
    // https://api.keyman.com/schemas/model_info.distribution.json
    // https://help.keyman.com/developer/cloud/model_info/1.0
    //

    function set_model_metadata(field: string, expected: any, warn: boolean = true) {
      if(model_info[field] && model_info[field] !== expected) {
        if(warn || typeof warn === 'undefined')
          console.warn(`Warning: source ${modelInfoFileName} field ${field} value "${model_info[field]}" does not match "${expected}" found in source file metadata.`);
      }
      model_info[field] = model_info[field] || expected;
    }

    // Merge model info file -- some fields have "special" behaviours -- see below

    set_model_metadata('id', model_id);
    set_model_metadata('name', kmpJsonData.info.name.description);
    set_model_metadata('authorName', kmpJsonData.info.author.description);

    // we strip the mailto: from the .kps file for the .model_info
    set_model_metadata('authorEmail', kmpJsonData.info.author.url.match(/^(mailto\:)?(.+)$/)[2], false);
    
    // extract the language identifiers from the language metadata
    // arrays for each of the lexical models in the kmp.json file,
    // and merge into a single array of identifiers in the 
    // .model_info file.
    model_info.languages = model_info.languages || [].concat(kmpJsonData.lexicalModels.map((e) => e.languages.map((f) => f.id)));

    set_model_metadata('lastModifiedDate', (new Date).toISOString());
    set_model_metadata('packageFilename', kmpFileName);

    // Always overwrite with actual file size
    model_info.packageFileSize = fs.statSync(model_info.packageFilename).size; 

    set_model_metadata('jsFilename', modelFileName);

    // Always overwrite with actual file size
    model_info.jsFileSize = fs.statSync(model_info.jsFilename).size;

    // Always overwrite source data
    model_info.packageIncludes = kmpJsonData.files.filter((e) => !!e.name.match(/.[ot]tf$/i)).length ? ['fonts'] : []; 

    set_model_metadata('version', kmpJsonData.info.version.description);

    // The minimum Keyman version detected in the package file may be manually set higher by the developer
    set_model_metadata('minKeymanVersion', minKeymanVersion, false); 

    //TODO: model_info.helpLink = model_info.helpLink || ... if source/help/id.php exists?
    set_model_metadata('sourcePath', [groupPath, authorPath, bcp47Path].join('/'));

    fs.writeFileSync(modelInfoFileName, JSON.stringify(model_info, null, 2));
  };

  transpileSources(sources: Array<string>): Array<string> {
    return sources.map((source) => ts.transpileModule(source, {
        compilerOptions: { module: ts.ModuleKind.None }
      }).outputText
    );
  };

  logError(s) {
    console.error(require('chalk').red(s));
  };
};
