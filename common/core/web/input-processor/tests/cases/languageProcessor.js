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
  describe('[[constructor]]', function () {
    it('should initialize without errors', function () {
      let lp = new com.keyman.text.prediction.LanguageProcessor();
      assert.isNotNull(lp);
    });

    it('has expected default values after initialization', function () {
      let languageProcessor = new com.keyman.text.prediction.LanguageProcessor();

      // These checks are lifted from the keyboard-processor init checks found in
      // common/core/web/keyboard-processor/tests/cases/basic-init.js.
      assert.isUndefined(languageProcessor.activeModel);
      assert.isFalse(languageProcessor.isActive);
      assert.isTrue(languageProcessor.mayPredict);
    });
  });
});