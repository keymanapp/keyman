/* Prevents the unneeded deps.js SCRIPT auto-inject when doing uncompiled tests.

 * Because of how TypeScript's <reference/> tag works, this must be in its own file and declared above 
 * goog/base.js in order to prevent unfortunate side-effects from occuring.  We don't need the functionality
 * that the Closure library tries to import otherwise.
 */

declare var CLOSURE_NO_DEPS: boolean;
var CLOSURE_NO_DEPS = true;