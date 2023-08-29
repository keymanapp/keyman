/**
 * Builds a source .model_info file with metadata extracted from .kps file and
 * compiled files to produce a comprehensive .model_info file.
 */

import { minKeymanVersion } from "./min-keyman-version.js";
import { ModelInfoFile } from "./model-info-file.js";
import { CompilerCallbacks, KmpJsonFile } from "@keymanapp/common-types";
import { ModelInfoCompilerMessages } from "./messages.js";

const HelpRoot = 'https://help.keyman.com/model/';

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
 * Builds .model_info file with metadata from the model and package source file.
 * This function is intended for use within the lexical-models repository. While many of the
 * parameters could be deduced from each other, they are specified here to reduce the
 * number of places the filenames are constructed.
 *
 * @param callbacks
 * @param options                  Details on files from which to extract additional metadata
 */
export function writeModelMetadataFile(
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

  let model_info: ModelInfoFile = {
    languages: [],
    license: 'mit'
  };

  //
  // Build .model_info file -- some fields have "special" behaviours -- see below
  // https://api.keyman.com/schemas/model_info.source.json and
  // https://api.keyman.com/schemas/model_info.distribution.json
  // https://help.keyman.com/developer/cloud/model_info/1.0
  //

  // TODO: isrtl
  // TODO: license

  model_info.id = options.model_id;
  model_info.name = options.kmpJsonData.info.name.description;

  const author = options.kmpJsonData.info.author;
  model_info.authorName = author?.description ?? '';

  if (author?.url) {
    // we strip the mailto: from the .kps file for the .model_info
    const match = author.url.match(/^(mailto\:)?(.+)$/);
    /* c8 ignore next 3 */
    if (match === null) {
      callbacks.reportMessage(ModelInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
      return null;
    }

    model_info.authorEmail = match[2];
  }

  // description

  if(options.kmpJsonData.info.description?.description) {
    model_info.description = options.kmpJsonData.info.description.description.trim();
  }

  // extract the language identifiers from the language metadata
  // arrays for each of the lexical models in the kmp.json file,
  // and merge into a single array of identifiers in the
  // .model_info file.

  model_info.languages = options.kmpJsonData.lexicalModels.reduce((a, e) => [].concat(a, e.languages.map((f) => f.id)), []);

  // TODO: use git date
  model_info.lastModifiedDate = (new Date).toISOString();

  model_info.packageFilename = callbacks.path.basename(options.kmpFileName);
  model_info.packageFileSize = callbacks.fileSize(options.kmpFileName);
  if(model_info.packageFileSize === undefined) {
    callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileDoesNotExist({filename:options.kmpFileName}));
    return null;
  }

  model_info.jsFilename = callbacks.path.basename(options.modelFileName);
  model_info.jsFileSize = callbacks.fileSize(options.modelFileName);
  if(model_info.jsFileSize === undefined) {
    callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileDoesNotExist({filename:options.modelFileName}));
    return null;
  }

  model_info.packageIncludes = options.kmpJsonData.files.filter((e) => !!e.name.match(/.[ot]tf$/i)).length ? ['fonts'] : [];
  model_info.version = options.kmpJsonData.info.version.description;
  model_info.minKeymanVersion = minKeymanVersion;
  model_info.helpLink = HelpRoot + model_info.id;

  if(options.sourcePath) {
    model_info.sourcePath = options.sourcePath;
  }

  const jsonOutput = JSON.stringify(model_info, null, 2);
  return new TextEncoder().encode(jsonOutput);
}
