/// <reference path="./trie-model.ts" />
/// <reference path="./quote-behavior.ts" />

// Add all namespaces defined here to the global scope:
if (typeof module != 'undefined' && typeof module.exports != 'undefined') {
  module.exports['models'] = models;
}
