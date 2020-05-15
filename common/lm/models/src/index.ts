/// <reference path="./placeholder.ts"/>

// Add all namespaces defined here to the global scope:
if (typeof module != 'undefined' && typeof module.exports != 'undefined') {
  module.exports['models'] = Models;
}
