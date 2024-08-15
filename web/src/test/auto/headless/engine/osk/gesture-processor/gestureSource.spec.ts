import { assert } from 'chai'
import { GestureDebugSource, InputSample } from '@keymanapp/gesture-recognizer';

const helloSample: InputSample<string> = {
  targetX: 1,
  targetY: 2,
  item: 'hello',
  t: 101
};

const worldSample: InputSample<string> = {
  targetX: 2,
  targetY: 3,
  item: 'world',
  t: 121
};

// 1 -> 4, 'a' -> 'd'
const simpleSampleSequence: InputSample<string>[] = [1, 2, 3, 4].map((i) => {
  return {
    targetX: i,
    targetY: i,
    item: String.fromCharCode('a'.charCodeAt(0) + i - 1),
    t: 20 * i
  }
});

/**
 * A minimalist mock for a recognizer-configuration based at <0, 0>.
 */
const mockedOriginConfig = {
  targetRoot: {
    getBoundingClientRect: () => {
      return {
        x: 0,
        y: 0
      };
    }
  }
};

/**
 * A minimalist mock for a recognizer-configuration based at <-2, -2>.
 */
const mockedInitialConfig = {
  targetRoot: {
    getBoundingClientRect: () => {
      return {
        left: -2,
        top: -2
      };
    }
  }
};

/**
 * A minimalist mock for a recognizer-configuration based at <2, 2>.
 */
const mockedShiftedConfig = {
  targetRoot: {
    getBoundingClientRect: () => {
      return {
        left: 2,
        top: 2
      };
    }
  }
};

// Should probably be a bit more thorough, but it's a start.
describe("GestureSource", function() {
  describe("Subviews", function() {
    it("construction:  preserve current path & base item", () => {
      let source = new GestureDebugSource<string>(0, null, true);
      for(let i=0; i < simpleSampleSequence.length; i++) {
        source.update(simpleSampleSequence[i]);
      }

      let subview = source.constructSubview(false, true);
      assert.equal(subview.path.coords.length, simpleSampleSequence.length);
      assert.deepEqual(subview.path.coords, simpleSampleSequence);
      assert.equal(subview.baseItem, simpleSampleSequence[0].item);
    });

    it("construction:  preserve only most recent sample & base item", () => {
      let source = new GestureDebugSource<string>(0, null, true);
      for(let i=0; i < simpleSampleSequence.length; i++) {
        source.update(simpleSampleSequence[i]);
      }

      let subview = source.constructSubview(true, true);
      assert.equal(subview.path.coords.length, 1);

      const lastSample = simpleSampleSequence[simpleSampleSequence.length - 1];
      assert.deepEqual(subview.path.coords, [lastSample]);
      assert.equal(subview.baseItem, simpleSampleSequence[0].item);
    });

    it("construction:  preserve only most recent sample", () => {
      let source = new GestureDebugSource<string>(0, null, true);
      for(let i=0; i < simpleSampleSequence.length; i++) {
        source.update(simpleSampleSequence[i]);
      }

      let subview = source.constructSubview(true, false);
      assert.equal(subview.path.coords.length, 1);

      const lastSample = simpleSampleSequence[simpleSampleSequence.length - 1];
      assert.deepEqual(subview.path.coords, [lastSample]);
      assert.equal(subview.baseItem, lastSample.item);
    });

    it("construction:  preserve current path & base item, with translation", () => {
      let source = new GestureDebugSource<string>(0, mockedInitialConfig as any, true);
      for(let i=0; i < simpleSampleSequence.length; i++) {
        source.update(simpleSampleSequence[i]);
      }

      source.pushRecognizerConfig(mockedShiftedConfig as any);

      let subview = source.constructSubview(false, true);
      // When an alternate recognizer-config is pushed, the original source's version of
      // the path should remain unchanged.
      assert.deepEqual(source.path.coords, simpleSampleSequence);

      // Subviews, on the other hand, should use the most appropriate current recognizer-config.
      // It'll be "locked in" for the subview.
      assert.equal(subview.path.coords.length, simpleSampleSequence.length);
      assert.deepEqual(subview.path.coords, simpleSampleSequence.map((value) => {
        return {
          ...value,
          targetX: value.targetX - 4,
          targetY: value.targetY - 4
        }
      }));
      assert.equal(subview.baseItem, simpleSampleSequence[0].item);
    });

    it("properly propagate updates", () => {
      let source = new GestureDebugSource<string>(0, null, true);
      let subview = source.constructSubview(false, true);

      assert.equal(subview.path.coords.length, 0);
      source.update(helloSample);

      assert.equal(subview.path.coords.length, 1);
      assert.deepEqual(subview.currentSample, helloSample);
    });

    it("may be disconnected from further updates", () => {
      let source = new GestureDebugSource<string>(0, null, true);
      let subview = source.constructSubview(false, true);

      source.update(helloSample);

      subview.disconnect();

      source.update(worldSample);

      assert.equal(source.path.coords.length, 2);

      // Should not receive sample2.
      assert.equal(subview.path.coords.length, 1);
      assert.deepEqual(subview.currentSample, helloSample);
    });

    it('is updated if constructed from "current" intermediate subview', () => {
      let source = new GestureDebugSource<string>(0, null, true);
      let baseSubview = source.constructSubview(false, true);

      source.update(helloSample);

      // Is constructed from a 'current' subview - one including the base source's
      // most recent sample.
      let subview = baseSubview.constructSubview(false, true);

      assert.equal(subview.path.coords.length, 1);

      source.update(worldSample);

      // BOTH should update in this scenario.
      assert.equal(baseSubview.path.coords.length, 2);
      assert.equal(subview.path.coords.length, 2);
      assert.deepEqual(baseSubview.currentSample, worldSample);
      assert.deepEqual(subview.currentSample, worldSample);
    });

    it("propagate path termination (complete)",  () => {
      let source = new GestureDebugSource<string>(0, null, true);
      let subview = source.constructSubview(true, true);
      let subview2 = source.constructSubview(true, true);

      assert.equal(subview.path.coords.length, 0);

      source.update(helloSample);
      subview.terminate(false);

      assert.equal(subview.path.coords.length, 1);
      assert.deepEqual(subview.currentSample, helloSample);
      assert.equal(subview.isPathComplete, true);
      assert.equal(subview.path.wasCancelled, false);
      assert.equal(subview2.isPathComplete, true);
      assert.equal(subview2.path.wasCancelled, false);
    });

    it("acts read-only - does not allow direct mutations", () => {
      let source = new GestureDebugSource<string>(0, mockedInitialConfig as any, true);
      let subview = source.constructSubview(false, true);
      source.update(helloSample);
      source.pushRecognizerConfig(mockedShiftedConfig as any);

      assert.throws(() => subview.update(worldSample));
      assert.throws(() => subview.pushRecognizerConfig(mockedOriginConfig as any));
      assert.throws(() => subview.popRecognizerConfig());

      // Should NOT throw; this is on the base source.
      source.popRecognizerConfig();
    });
  });
});