import { assert } from "chai";
import * as Package from "keyman/engine/keyboard";

// A few small tests to ensure that the ES Module bundle was successfully constructed and is usable.

var toSupplementaryPairString = function(code){
  var H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
  var L = (code - 0x10000) % 0x400 + 0xDC00;

  return String.fromCharCode(H, L);
}

let u = toSupplementaryPairString;

describe('Bundled ES Module', function() {
  describe('Keyboard', function () {
    it('should initialize without errors', function () {
      let kp = new Package.Keyboard();
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
      // Is installed as a _side effect_ from importing the module.
      // We could disable that and require a call of `extendString()` instead.
      String.kmwEnableSupplementaryPlane(true); // Declared & defined in web-utils.
      try {
        let target = new Package.Mock(u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be), 2);  // ap | le
        target.insertTextBeforeCaret(u(0x1d5c9));
        assert.equal(target.getText(), u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be));
      } finally {
        String.kmwEnableSupplementaryPlane(false);
      }
    });
  });

  describe("Imported `utils`", function() {
    it("should include `utils` package's Version class", () => {
      let v16 = new Package.Version([16, 1]);
      assert.equal(v16.toString(), "16.1");
    });
  })
});