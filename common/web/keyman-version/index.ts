/// <reference path="./version.inc.ts" />

// Add all namespaces defined here to the global scope:
if (typeof module != 'undefined' && typeof module.exports != 'undefined') {
  module.exports['KEYMAN_VERSION'] = com.keyman.KEYMAN_VERSION;
}
