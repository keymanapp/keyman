/**
 * Merges a source .model_info file with metadata extracted from .kps file and
 * compiled files to produce a comprehensive .model_info file.
 */

/// <reference path="../lexical-model-compiler/lexical-model.ts" />
/// <reference path="../lexical-model-compiler/model-info-file.ts" />

import * as fs from "fs";
import * as path from "path";

const minKeymanVersion = '12.0';

export default class ModelInfoCompiler {

/**
   * Merges source .model_info file with metadata from the model and package source file.
   * This function is intended for use within the lexical-models repository. While many of the
   * parameters could be deduced from each other, they are specified here to reduce the 
   * number of places the filenames are constructed.
   *
   * @param sourceModelInfoFileName  Path for the source .model_info file
   * @param destModelInfoFileName    Path to write the merged .model_info file to
   * @param model_id                 The identifier for the model
   * @param kmpJsonData              The data from the .kps file, transformed to kmp.json
   * @param sourcePath               The path in the keymanapp/lexical-models repo where this model may be found
   * @param modelFileName            The compiled model filename and relative path (.js)
   * @param kmpFileName              The compiled package filename and relative path (.kmp)
   */
  writeMergedModelMetadataFile(
    sourceModelInfoFileName: string,
    destModelInfoFileName: string, 
    model_id: string, 
    kmpJsonData: KmpJsonFile, 
    sourcePath: string|null|undefined, 
    modelFileName: string, 
    kmpFileName: string
  ) {
    
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
    let model_info: ModelInfoFile = JSON.parse(fs.readFileSync(sourceModelInfoFileName, 'utf8'));

    //
    // Build merged .model_info file
    // https://api.keyman.com/schemas/model_info.source.json and
    // https://api.keyman.com/schemas/model_info.distribution.json
    // https://help.keyman.com/developer/cloud/model_info/1.0
    //

    function set_model_metadata(field: string, expected: any, warn: boolean = true) {
      if(model_info[field] && model_info[field] !== expected) {
        if(warn || typeof warn === 'undefined')
          console.warn(`Warning: source ${sourceModelInfoFileName} field ${field} value "${model_info[field]}" does not match "${expected}" found in source file metadata.`);
      }
      model_info[field] = model_info[field] || expected;
    }

    //
    // Merge model info file -- some fields have "special" behaviours -- see below
    //

    set_model_metadata('id', model_id);

    set_model_metadata('name', kmpJsonData.info.name.description);
    set_model_metadata('authorName', kmpJsonData.info.author.description);

    // we strip the mailto: from the .kps file for the .model_info
    set_model_metadata('authorEmail', kmpJsonData.info.author.url.match(/^(mailto\:)?(.+)$/)[2], false);

    // extract the language identifiers from the language metadata
    // arrays for each of the lexical models in the kmp.json file,
    // and merge into a single array of identifiers in the
    // .model_info file.

    model_info.languages = model_info.languages || kmpJsonData.lexicalModels.reduce((a, e) => [].concat(a, e.languages.map((f) => f.id)), []);

    set_model_metadata('lastModifiedDate', (new Date).toISOString());
    set_model_metadata('packageFilename', path.basename(kmpFileName));

    // Always overwrite with actual file size
    model_info.packageFileSize = fs.statSync(kmpFileName).size;

    set_model_metadata('jsFilename', path.basename(modelFileName));

    // Always overwrite with actual file size
    model_info.jsFileSize = fs.statSync(modelFileName).size;

    // Always overwrite source data
    model_info.packageIncludes = kmpJsonData.files.filter((e) => !!e.name.match(/.[ot]tf$/i)).length ? ['fonts'] : [];

    set_model_metadata('version', kmpJsonData.info.version.description);

    // The minimum Keyman version detected in the package file may be manually set higher by the developer
    set_model_metadata('minKeymanVersion', minKeymanVersion, false);

    if(sourcePath) {
      set_model_metadata('sourcePath', sourcePath);
    }

    fs.writeFileSync(destModelInfoFileName, JSON.stringify(model_info, null, 2));   
  };
}