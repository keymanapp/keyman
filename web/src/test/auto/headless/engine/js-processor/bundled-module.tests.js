import { assert } from "chai";
import * as JSProcessorModule from "keyman/engine/js-processor";
import * as KeyboardModule from "keyman/engine/keyboard";
const KMWString = KeyboardModule.KMWString;

// A few small tests to ensure that the ES Module bundle was successfully constructed and is usable.

var toSupplementaryPairString = function(code){
  var H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
  var L = (code - 0x10000) % 0x400 + 0xDC00;

  return String.fromCharCode(H, L);
}

let u = toSupplementaryPairString;

describe('Bundled ES Module for js-processor', function() {
  describe('JSKeyboardProcessor', function () {
    it('should initialize without errors', function () {
      let kp = new JSProcessorModule.JSKeyboardProcessor(null, JSProcessorModule.unitTestEndpoints.DEFAULT_OPTIONS);
      assert.isNotNull(kp);
    });
  });

});

describe('Bundled ES Module for keyboard', function () {
  describe('Keyboard', function () {
    it('should initialize without errors', function () {
      let kp = new KeyboardModule.JSKeyboard();
      assert.isNotNull(kp);
    });
  });

  describe("Imported `utils`", function () {
    it("should include `utils` package's Version class", () => {
      let v16 = new KeyboardModule.Version([16, 1]);
      assert.equal(v16.toString(), "16.1");
    });
  });

  describe('SyntheticTextStore', () => {
    it('basic functionality test', () => {
      let textStore = new KeyboardModule.SyntheticTextStore("aple", 2);  // ap | le
      textStore.insertTextBeforeCaret('p');
      assert.equal(textStore.getText(), "apple");
    });

    it('smp test', () => {
      KMWString.enableSupplementaryPlane(true); // Declared & defined in web-utils.
      try {
        let textStore = new KeyboardModule.SyntheticTextStore(u(0x1d5ba) + u(0x1d5c9) + u(0x1d5c5) + u(0x1d5be), 2);  // ap | le
        textStore.insertTextBeforeCaret(u(0x1d5c9));
        assert.equal(textStore.getText(), u(0x1d5ba) + u(0x1d5c9) + u(0x1d5c9) + u(0x1d5c5) + u(0x1d5be));
      } finally {
        KMWString.enableSupplementaryPlane(false);
      }
    });
  });
});
