import { BuildActivity } from './BuildActivity.js';
import { BuildKeyboardInfo } from './BuildKeyboardInfo.js';
import { BuildKmnKeyboard } from './BuildKmnKeyboard.js';
import { BuildLdmlKeyboard } from './BuildLdmlKeyboard.js';
import { BuildModel } from './BuildModel.js';
import { BuildModelInfo } from './BuildModelInfo.js';
import { BuildPackage } from './BuildPackage.js';

// These builders are listed in the order that files need to be built in
// projects. Packages depend on .kmn, .xml and .model.ts file types
export const buildActivities: BuildActivity[] = [
  new BuildKmnKeyboard(),
  new BuildLdmlKeyboard(),
  new BuildModel(),
  new BuildPackage(),
];

// These are built from the .kpj reference after all others
export const buildKeyboardInfoActivity = new BuildKeyboardInfo();
export const buildModelInfoActivity = new BuildModelInfo();


// Note: BuildProject is not listed here to avoid circular references,
// because it depends on the other activities here. This means that
// BuildProject must be separately checked.
