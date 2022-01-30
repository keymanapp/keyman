const pjson = require('../package.json');
import { extractVersionData } from './version-data';
export const environment = extractVersionData(pjson.version);
