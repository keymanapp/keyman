/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import { KeymanTargets } from "@keymanapp/common-types";
import { GeneratorOptions } from "../src/abstract-generator.js";

export const options: GeneratorOptions = {
  // TODO-GENERATE: icon support
  // icon: false,
  id: 'sample',
  languageTags: ['en'],
  name: 'Sample Project',
  outPath: '.',
  targets: [KeymanTargets.KeymanTarget.windows],
  version: '1.0',
  author: 'Sample Author',
  copyright: 'TheAuthor',
  description: '# A mighty description',
  keymanVersion: '17.0.292'
};