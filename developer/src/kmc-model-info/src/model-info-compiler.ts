/**
 * Merges a source .model_info file with metadata extracted from .kps file and
 * compiled files to produce a comprehensive .model_info file.
 */

import { minKeymanVersion } from "./min-keyman-version.js";
import { ModelInfoFile } from "./model-info-file.js";
import { CompilerCallbacks, KmpJsonFile } from "@keymanapp/common-types";
import { ModelInfoCompilerMessages } from "./messages.js";

/* c8 ignore start */
export class ModelInfoOptions {
  /** The identifier for the model */
  model_id: string;

  /** The data from the .kps file, transformed to kmp.json */
  kmpJsonData: KmpJsonFile.KmpJsonFile;

  /** The path in the keymanapp/lexical-models repo where this model may be found (optional) */
  sourcePath?: string;

  /** The compiled model filename and relative path (.js) */
  modelFileName: string;

  /** The compiled package filename and relative path (.kmp) */
  kmpFileName: string;
};
/* c8 ignore stop */

/**
 * Merges source .model_info file with metadata from the model and package source file.
 * This function is intended for use within the lexical-models repository. While many of the
 * parameters could be deduced from each other, they are specified here to reduce the
 * number of places the filenames are constructed.
 *
 * @param sourceModelInfoFileName  Path for the source .model_info file
 * @param options                  Details on files from which to extract additional metadata
 */
export function writeMergedModelMetadataFile(
    sourceModelInfoFileName: string,
    callbacks: CompilerCallbacks,
    options: ModelInfoOptions
  ): Uint8Array {

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
  const dataInput = callbacks.loadFile(sourceModelInfoFileName);
  if(!dataInput) {
    callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileDoesNotExist({filename: sourceModelInfoFileName}));
    return null;
  }

  let model_info: ModelInfoFile = null;
  try {
    const jsonInput = new TextDecoder('utf-8', {fatal: true}).decode(dataInput);
    model_info = JSON.parse(jsonInput);
  } catch(e) {
    callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileIsNotValid({filename: sourceModelInfoFileName, e}));
    return null;
  }

  //
  // Build merged .model_info file
  // https://api.keyman.com/schemas/model_info.source.json and
  // https://api.keyman.com/schemas/model_info.distribution.json
  // https://help.keyman.com/developer/cloud/model_info/1.0
  //

  function setModelMetadata(field: keyof ModelInfoFile, expected: unknown, warn: boolean = true) {
    /* c8 ignore next 4 */
    if (model_info[field] && model_info[field] !== expected) {
      if (warn ?? true) {
        callbacks.reportMessage(ModelInfoCompilerMessages.Warn_MetadataFieldInconsistent({
          field, value:model_info[field], expected
        }));
      }

    }
    // TypeScript gets upset with this assignment, because it cannot deduce
    // the exact type of model_info[field] -- there are many possibilities!
    // So we assert that it's unknown so that TypeScript can chill.
    (<unknown> model_info[field]) = model_info[field] || expected;
  }

  //
  // Merge model info file -- some fields have "special" behaviours -- see below
  //

  setModelMetadata('id', options.model_id);

  setModelMetadata('name', options.kmpJsonData.info.name.description);

  let author = options.kmpJsonData.info.author;
  setModelMetadata('authorName', author.description);

  if (author.url) {
    // we strip the mailto: from the .kps file for the .model_info
    let match = author.url.match(/^(mailto\:)?(.+)$/);
    /* c8 ignore next 3 */
    if (match === null) {
      callbacks.reportMessage(ModelInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
      return null;
    }

    let email = match[2];
    setModelMetadata('authorEmail', email, false);
  }

  // extract the language identifiers from the language metadata
  // arrays for each of the lexical models in the kmp.json file,
  // and merge into a single array of identifiers in the
  // .model_info file.

  model_info.languages = model_info.languages || options.kmpJsonData.lexicalModels.reduce((a, e) => [].concat(a, e.languages.map((f) => f.id)), []);

  setModelMetadata('lastModifiedDate', (new Date).toISOString());
  setModelMetadata('packageFilename', callbacks.path.basename(options.kmpFileName));

  // Always overwrite with actual file size
  model_info.packageFileSize = callbacks.fileSize(options.kmpFileName);
  if(model_info.packageFileSize === undefined) {
    callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileDoesNotExist({filename:options.kmpFileName}));
    return null;
  }

  setModelMetadata('jsFilename', callbacks.path.basename(options.modelFileName));

  // Always overwrite with actual file size
  model_info.jsFileSize = callbacks.fileSize(options.modelFileName);
  if(model_info.jsFileSize === undefined) {
    callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileDoesNotExist({filename:options.modelFileName}));
    return null;
  }

  // Always overwrite source data
  model_info.packageIncludes = options.kmpJsonData.files.filter((e) => !!e.name.match(/.[ot]tf$/i)).length ? ['fonts'] : [];

  setModelMetadata('version', options.kmpJsonData.info.version.description);

  // The minimum Keyman version detected in the package file may be manually set higher by the developer
  setModelMetadata('minKeymanVersion', minKeymanVersion, false);

  if(options.sourcePath) {
    setModelMetadata('sourcePath', options.sourcePath);
  }

  const jsonOutput = JSON.stringify(model_info, null, 2);
  return new TextEncoder().encode(jsonOutput);
}
