import { extractVersionData } from './version-data';
// TODO: environment should be just KEYMAN_VERSION

const KEYMAN_VERSION = require("@keymanapp/keyman-version").KEYMAN_VERSION;
export const environment = extractVersionData(KEYMAN_VERSION.VERSION_WITH_TAG);
