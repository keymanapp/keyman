import { assert } from "chai";
import * as JSProcessorPackage from "keyman/engine/js-processor";
import * as KeyboardPackage from "keyman/engine/keyboard";
const KMWString = KeyboardPackage.KMWString;

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
      let kp = new JSProcessorPackage.JSKeyboardProcessor();
      assert.isNotNull(kp);
    });
  });

});

describe('Bundled ES Module for keyboard', function () {
  describe('Keyboard', function () {
    it('should initialize without errors', function () {
      let kp = new KeyboardPackage.JSKeyboard();
      assert.isNotNull(kp);
    });
  });

  describe("Imported `utils`", function () {
    it("should include `utils` package's Version class", () => {
      let v16 = new KeyboardPackage.Version([16, 1]);
      assert.equal(v16.toString(), "16.1");
    });
  });

  describe('Mock', () => {
    it('basic functionality test', () => {
      let target = new KeyboardPackage.Mock("aple", 2);  // ap | le
      target.insertTextBeforeCaret('p');
      assert.equal(target.getText(), "apple");
    });

    it('smp test', () => {
      KMWString.enableSupplementaryPlane(true); // Declared & defined in web-utils.
      try {
        let target = new KeyboardPackage.Mock(u(0x1d5ba) + u(0x1d5c9) + u(0x1d5c5) + u(0x1d5be), 2);  // ap | le
        target.insertTextBeforeCaret(u(0x1d5c9));
        assert.equal(target.getText(), u(0x1d5ba) + u(0x1d5c9) + u(0x1d5c9) + u(0x1d5c5) + u(0x1d5be));
      } finally {
        KMWString.enableSupplementaryPlane(false);
      }
    });
  });
});
