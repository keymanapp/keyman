import { extractVersionData } from './version-data';
// TODO: environment should be just KEYMAN_VERSION
export const environment = extractVersionData(KEYMAN_VERSION.VERSION_WITH_TAG);
