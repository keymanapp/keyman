import { assert } from 'chai'
import { GestureSource, InputSample } from '@keymanapp/gesture-recognizer';

// Should probably be a bit more thorough, but it's a start.
describe("GestureSource", function() {
  describe(".constructSubview", function() {
    it("properly propagates updates", () => {
      let source = new GestureSource<string>(0, null, true);
      let subview = source.constructSubview(true, true);

      assert.equal(subview.path.coords.length, 0);

      let sample: InputSample<string> = {
        targetX: 1,
        targetY: 2,
        item: 'hello',
        t: 101
      };

      source.update(sample);

      assert.equal(subview.path.coords.length, 1);
      assert.deepEqual(subview.currentSample, sample);
    });

    it("propagates path termination (complete)",  () => {
      let source = new GestureSource<string>(0, null, true);
      let subview = source.constructSubview(true, true);
      let subview2 = source.constructSubview(true, true);

      assert.equal(subview.path.coords.length, 0);

      let sample: InputSample<string> = {
        targetX: 1,
        targetY: 2,
        item: 'hello',
        t: 101
      };

      source.update(sample);
      subview.terminate(false);

      assert.equal(subview.path.coords.length, 1);
      assert.deepEqual(subview.currentSample, sample);
      assert.equal(subview.isPathComplete, true);
      assert.equal(subview.path.wasCancelled, false);
      assert.equal(subview2.isPathComplete, true);
      assert.equal(subview2.path.wasCancelled, false);
    });
  });
});