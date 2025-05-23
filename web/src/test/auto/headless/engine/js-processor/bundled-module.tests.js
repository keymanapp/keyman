import { assert } from "chai";
import * as Package from "keyman/engine/js-processor";
import * as Package2 from "keyman/engine/keyboard";
const KMWString = Package2.KMWString;

// A few small tests to ensure that the ES Module bundle was successfully constructed and is usable.

var toSupplementaryPairString = function(code){
  var H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
  var L = (code - 0x10000) % 0x400 + 0xDC00;

  return String.fromCharCode(H, L);
}

let u = toSupplementaryPairString;

describe('Bundled ES Module for js-processor', function() {
  describe('KeyboardProcessor', function () {
    it('should initialize without errors', function () {
      let kp = new Package.KeyboardProcessor();
      assert.isNotNull(kp);
    });
  });

  describe('Mock', () => {
    it('basic functionality test', () => {
      let target = new Package.Mock("aple", 2);  // ap | le
      target.insertTextBeforeCaret('p');
      assert.equal(target.getText(), "apple");
    });

    it('smp test', () => {
      KMWString.enableSupplementaryPlane(true); // Declared & defined in web-utils.
      try {
        let target = new Package.Mock(u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be), 2);  // ap | le
        target.insertTextBeforeCaret(u(0x1d5c9));
        assert.equal(target.getText(), u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be));
      } finally {
        KMWString.enableSupplementaryPlane(false);
      }
    });
  });
});

describe('Bundled ES Module for keyboard', function () {
  describe('Keyboard', function () {
    it('should initialize without errors', function () {
      let kp = new Package2.Keyboard();
      assert.isNotNull(kp);
    });
  });

  describe("Imported `utils`", function () {
    it("should include `utils` package's Version class", () => {
      let v16 = new Package2.Version([16, 1]);
      assert.equal(v16.toString(), "16.1");
    });
  });
});
