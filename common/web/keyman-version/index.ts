/// <reference path="./version.inc.ts" />

// Aliases the namespaced type with the non-namespaced module export.
declare let KEYMAN_VERSION: typeof com.keyman.KEYMAN_VERSION;

// Add all namespaces defined here to the global scope:
if (typeof module != 'undefined' && typeof module.exports != 'undefined') {
  module.exports['KEYMAN_VERSION'] = com.keyman.KEYMAN_VERSION;
}
