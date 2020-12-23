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
describe('InputProcessor', function() {
  describe('[[constructor]]', function () {
    it('should initialize without errors', function () {
      let core = new InputProcessor();
      assert.isNotNull(core);
    });

    it('has expected default values after initialization', function () {
      let core = new InputProcessor();

      assert.isOk(core.keyboardProcessor);
      assert.isOk(core.languageProcessor);
      assert.isOk(core.keyboardInterface);
      assert.isUndefined(core.activeKeyboard); // No keyboard should be loaded yet.
      assert.isUndefined(core.activeModel);    // Same for the model.

      // These checks are lifted from the keyboard-processor init checks found in
      // common/core/web/keyboard-processor/tests/cases/basic-init.js.
      assert.equal('us', core.keyboardProcessor.baseLayout, 'KeyboardProcessor has unexpected base layout')
      assert.isNotNull(global.KeymanWeb, 'KeymanWeb global was not automatically installed');
      assert.equal('default', core.keyboardProcessor.layerId, 'Default layer is not set to "default"');
      assert.isUndefined(core.keyboardProcessor.activeKeyboard, 'Initialized with already-active keyboard');

      // Lifted from languageProcessor.js - the core should not be changing these with its init.
      assert.isUndefined(core.languageProcessor.activeModel);
      assert.isFalse(core.languageProcessor.isActive);
      assert.isTrue(core.languageProcessor.mayPredict);
    });
  });
});