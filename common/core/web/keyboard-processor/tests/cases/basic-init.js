var assert = require('chai').assert;
var fs = require("fs");
var vm = require("vm");

let KeyboardProcessor = require('../../dist');

// Required initialization setup.
global.com = KeyboardProcessor.com; // exports all keyboard-processor namespacing.
global.keyman = {}; // So that keyboard-based checks against the global `keyman` succeed.
                    // 10.0+ dependent keyboards, like khmer_angkor, will otherwise fail to load.

// Initialize supplementary plane string extensions
String.kmwEnableSupplementaryPlane(false);    

// Test the KeyboardProcessor interface.
describe('KeyboardProcessor', function() {
  describe('[[constructor]]', function () {
    it('should initialize without errors', function () {
      let kp = new KeyboardProcessor();
      assert.isNotNull(kp);
    });

    it('has expected default values after initialization', function () {
      let kp = new KeyboardProcessor();
      assert.equal('us', kp.baseLayout, 'KeyboardProcessor has unexpected base layout')
      assert.isNotNull(global.KeymanWeb, 'KeymanWeb global was not automatically installed');
      assert.equal('default', kp.layerId, 'Default layer is not set to "default"');
      assert.isUndefined(kp.activeKeyboard, 'Initialized with already-active keyboard');
    });
  });

  describe('activeKeyboard', function() {
    it('is automatically set (in headless) on keyboard load', function () {
      let kp = new KeyboardProcessor();

      // These two lines will load a keyboard from its file; headless-mode `registerKeyboard` will
      // automatically set the keyboard as active.
      var script = new vm.Script(fs.readFileSync('tests/resources/keyboards/khmer_angkor.js'));
      script.runInThisContext();

      assert.isDefined(kp.activeKeyboard, 'Keyboard failed to register on script load');
      assert.equal('Keyboard_khmer_angkor', kp.activeKeyboard.id, 'Unexpected keyboard id found after script load');
    });
  });

  describe('processKeystroke', function() {
    it('simple output, no context', function() {
      let kp = new KeyboardProcessor();

      // These two lines will load a keyboard from its file; headless-mode `registerKeyboard` will
      // automatically set the keyboard as active.
      var script = new vm.Script(fs.readFileSync('tests/resources/keyboards/khmer_angkor.js'));
      script.runInThisContext();

      // Fetch a key from the keyboard's layout.
      let defaultLayer = kp.activeKeyboard.layout('desktop').getLayer('default');
      assert.isNotNull(defaultLayer);
      let keyS = defaultLayer.getKey('K_S');
      assert.isNotNull(keyS);

      //Get the default synthetic key event for said key.
      let mock = new com.keyman.text.Mock();
      let keyEvent = keyS.constructKeyEvent(kp, mock, 
                                            new com.keyman.text.EngineDeviceSpec('chrome', 'desktop', 'windows', false)
      );

      assert.isNotNull(keyEvent);

      let ruleBehavior = kp.processKeystroke(keyEvent, mock);
      assert.isNotNull(ruleBehavior);
      assert.isNotNull(ruleBehavior.transcription);
      assert.equal(mock.getText(), 'ស', 'Unexpected output from key event');

      let transform = ruleBehavior.transcription.transform;
      assert.isNotNull(transform);
    })
  });
});
