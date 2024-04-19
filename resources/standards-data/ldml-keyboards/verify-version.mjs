import {constants} from '../../../core/include/ldml/build/keyman_core_ldml.js';
import { argv } from 'node:process';

const args = argv.slice(2);
const { cldr_version_latest } = constants;
const ver = args[0];

if (cldr_version_latest !== ver) {
    throw Error(`Requester CLDR version '${ver}' but cldr_version_latest is '${cldr_version_latest}' - check keyman_core_ldml.ts`);
} else {
    console.log(`CLDR version ${ver} OK`);
}
