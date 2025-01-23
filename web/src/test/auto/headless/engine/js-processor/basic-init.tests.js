import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { JSKeyboardProcessor } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';

global.keyman = {}; // So that keyboard-based checks against the global `keyman` succeed.
                    // 10.0+ dependent keyboards, like khmer_angkor, will otherwise fail to load.

// Initialize supplementary plane string extensions
String.kmwEnableSupplementaryPlane(false);

// Test the JSKeyboardProcessor interface.
describe('JSKeyboardProcessor', function() {
  describe('[[constructor]]', function () {
    it('should initialize without errors', function () {
      let kp = new JSKeyboardProcessor();
      assert.isNotNull(kp);
    });

    it('has expected default values after initialization', function () {
      let kp = new JSKeyboardProcessor();
      assert.equal('us', kp.baseLayout, 'JSKeyboardProcessor has unexpected base layout')
      assert.isNotNull(global.KeymanWeb, 'KeymanWeb global was not automatically installed');
      assert.equal('default', kp.layerId, 'Default layer is not set to "default"');
      assert.isUndefined(kp.activeKeyboard, 'Initialized with already-active keyboard');
    });
  });

  describe('activeKeyboard', function() {
    it('is automatically set (in headless) on keyboard load', async function () {
      let kp = new JSKeyboardProcessor();

      // These two lines will load a keyboard from its file; headless-mode `registerKeyboard` will
      // automatically set the keyboard as active.
      let keyboardLoader = new NodeKeyboardLoader();
      let keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/khmer_angkor.js'));

      assert.isDefined(keyboard, 'Keyboard failed to register on script load');
      assert.equal('Keyboard_khmer_angkor', keyboard.id, 'Unexpected keyboard id found after script load');
    });
  });
});
