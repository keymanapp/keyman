var assert = require('chai').assert;
var fs = require("fs");
var vm = require("vm");

let InputProcessor = require('../../dist');

// Required initialization setup.
global.com = InputProcessor.com; // exports all keyboard-processor namespacing.
global.keyman = {}; // So that keyboard-based checks against the global `keyman` succeed.
                    // 10.0+ dependent keyboards, like khmer_angkor, will otherwise fail to load.

// Initialize supplementary plane string extensions
String.kmwEnableSupplementaryPlane(false);    

// Test the KeyboardProcessor interface.
describe('LanguageProcessor', function() {
  it('attempts to run unit tests', function() {
    let languageProcessor = new com.keyman.text.prediction.LanguageProcessor();

    assert.isOk(languageProcessor);
  });
});