/**
 * A temporary file to validate that the bundled version really is bundled and is usable in a
 * similar manner to its old format.
 */

// Loads `com` into the global namespace.
import * as _ from '../build/bundled/index.js';

console.log(`Int code for ALT: ${com.keyman.text.Codes.modifierCodes['ALT']}`);

console.log(new com.keyman.keyboards.Keyboard(null));

console.log();

// make sure we bundled `utils` as well!
console.log(`Verifying proper handling of version 16.0: ${new com.keyman.utils.Version([16, 0]).toString()}`);