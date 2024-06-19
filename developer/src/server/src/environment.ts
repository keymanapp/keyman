import { extractVersionData } from './version-data.js';
// TODO: environment should be just KEYMAN_VERSION

import KEYMAN_VERSION from "@keymanapp/keyman-version";
export const environment = extractVersionData(KEYMAN_VERSION.VERSION_WITH_TAG);
