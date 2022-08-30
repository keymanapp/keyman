const assert = require('chai').assert;
const sinon = require('sinon');

const fs = require('fs');

const GestureRecognizer = require('../../../../build/index.js');
const com = GestureRecognizer.com;
const PathSegmenter = com.keyman.osk.PathSegmenter;

describe("Segmentation", function() {
  // // File paths need to be from the package's / module's root folder
  // let testJSONtext = fs.readFileSync('src/test/resources/json/canaryRecording.json');

  it("Single-sample 'sequence'", function() {
    let spy = sinon.fake();

    const segmenter = new PathSegmenter(PathSegmenter.DEFAULT_CONFIG, spy);

    const sample = {
      targetX: 1,
      targetY: 1,
      t: 100
    };

    // Expected sequence:
    // 1:  on `segmenter.add(sample)`:
    //     a. 'start' segment
    // 2: on `segmenter.close()`:
    //     a.  unrecognized (null-type) segment - it doesn't last long enough
    //         for a subsegment to be formed before the `close()`.
    //     b. 'end' segment

    segmenter.add(sample);
    assert.isTrue(spy.calledOnce, "Segmenter callback was not called exactly once before the .close().");

    segmenter.close();
    assert.isTrue(spy.calledThrice, "Segmenter callback was not called exactly twice after the .close().");

    for(let i=0; i < 3; i++) {
      assert.equal(spy.args[i].length, 1, "Segmenter callback received an unexpected number of arguments");
    }

    // Is the first argument of each call's argument set.
    assert.equal(spy.args[0][0].type, 'start', "First call should receive a 'start'-type segment.");
    assert.equal(spy.args[1][0].type, undefined, "Second call's segment should not be classified.");
    assert.equal(spy.args[2][0].type, 'end', "Third call should receive an 'end'-type segment.");
  });
});