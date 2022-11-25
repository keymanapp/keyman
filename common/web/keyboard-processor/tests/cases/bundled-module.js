import { assert } from "chai";
import * as Package from "../../build/lib/index.mjs";

// A few small tests to ensure that the ES Module bundle was successfully constructed and is usable.

describe('Bundled ES Module', function() {
  describe('KeyboardProcessor', function () {
    it('should initialize without errors', function () {
      let kp = new Package.KeyboardProcessor();
      assert.isNotNull(kp);
    });
  });

  describe('Mock', () => {
    it('basic functionality test', () => {
      let target = new Package.Mock("aple", 2);
      target.insertTextBeforeCaret('p');
      assert.equal(target.getText(), "apple");
    });
  });

  describe("Imported `utils`", function() {
    it("should include `utils` package's Version class", () => {
      let v16 = new Package.Version([16, 1]);
      assert.equal(v16.toString(), "16.1");
    });
  })
});