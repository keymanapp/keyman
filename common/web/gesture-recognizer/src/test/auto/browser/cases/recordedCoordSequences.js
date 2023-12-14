var assert = chai.assert;
var expect = chai.expect;

import {
  HostFixtureLayoutController,
  InputSequenceSimulator
} from '../../../../../build/tools/lib/index.mjs';

function isOnAndroid() {
  const agent=navigator.userAgent;
  return agent.indexOf('Android' >= 0);
}

describe("Layer one - DOM -> InputSequence", function() {
  this.timeout(testconfig.timeouts.standard);

  before(function() {
    fixture.setBase('');
  });

  beforeEach(function(done) {
    fixture.load('host-fixture.html');
    this.controller = new HostFixtureLayoutController();
    this.controller.connect().then(() => done());
  });

  afterEach(function() {
    this.controller.destroy();
    fixture.cleanup();
    fixture.cleanup();
  });

  describe('recorded input sequences', function() {
    beforeEach(function() {
      this.clock = sinon.useFakeTimers();
    });

    afterEach(function() {
      this.clock.restore(); // restores the default implementations.
    });

    // We rely on this function to have the same context as `it` - the test-definition function.
    let replayAndCompare = function(testObj) {
      let resultPromise;

      let playbackEngine = new InputSequenceSimulator(this.controller);

      // **********************************
      // Android-Chrome sequence simulation does not allow fractional values in MouseEvent clientX/clientY...
      // and it's impossible to coerce elements to have proper pixel alignment, it seems.  Fractional
      // values are fine for touch events' `Touch` objects, though.
      //
      // So, the best way forward for doing automated testing on Android devices... is to note if we're
      // Android and coerce test sequences to all run in 'touch' mode if so.
      // **********************************
      if(isOnAndroid()) {
        for(let input of testObj.inputs) {
          input.isFromTouch = true;
        }
      }

      resultPromise = playbackEngine.replayAsync(testObj);

      // replayAsync sets up timeouts against the `clock` object.
      // This will run through the simulated timeout queue asynchronously.
      this.clock.runAllAsync();

      // resultPromise resolves on the final timeout in the queue.
      return resultPromise.then((result) => {
        assert.equal(result.inputs.length, testObj.inputs.length);

        // Removes the timestamp element; we know that this component may not match perfectly.
        let sampleCleaner = (sample) => { return {targetX: sample.targetX, targetY: sample.targetY} };

        // Returns just the observed, cleaned samples for a sequence object.  The recorded coordinates
        // should match perfectly.
        let seqCleaner = (input) => {
          return input.path.coords.map(sampleCleaner);
        };

        let cleanOriginalSet = testObj.inputs.map(seqCleaner);
        let cleanResultSet   = result .inputs.map(seqCleaner);

        expect(cleanOriginalSet.length).to.equal(cleanResultSet.length, "Unexpected difference in number of touch contact points");

        for(let i=0; i < cleanOriginalSet.length; i++) {
          let resultContactPath = cleanResultSet[i];
          let originalContactPath = cleanOriginalSet[i];

          for(let j=0; j < Math.max(resultContactPath.length, originalContactPath.length); j++) {
            const sampleResult = resultContactPath[j];
            const sampleOriginal = originalContactPath[j];

            assert.isOk(sampleResult, `An expected sample was missing during simulation - failed at path entry ${j}`);
            assert.isOk(sampleOriginal, `An extra sample was generated during simulation - failed at path entry ${j}`);

            // During test runs against a real Android device, we tend to get almost, but not-quite, integer targetX and targetY values.
            expect(sampleResult.targetX).to.be.closeTo(sampleOriginal.targetX, 1e-4, `Mismatch in x-coord at path entry ${j}`);
            expect(sampleResult.targetY).to.be.closeTo(sampleOriginal.targetY, 1e-4, `Mismatch in y-coord at path entry ${j}`);
          }
        }

        // Now to compare just the timestamp elements.  We'll tolerate a difference of up to 1.
        // Note:  if using the `replaySync` function instead, disable this section!
        // (Through to the nested for-loop `assert.closeTo`)
        let sampleTimeExtractor = (sample) => sample.t;
        let inputTimeExtractor = (input) => {
          return input.path.coords.map(sampleTimeExtractor);
        }

        let originalTimeSet = testObj.inputs.map(inputTimeExtractor);
        let resultTimeSet   = result .inputs.map(inputTimeExtractor);

        for(let i = 0; i < originalTimeSet.length; i++) {
          for(let j = 0; j < originalTimeSet[i].length; j++) {
            assert.closeTo(resultTimeSet[i][j], originalTimeSet[i][j], 1);
          }
        }

        // The 'terminationEvent' property should match.  Any sequence that was "canceled" should still
        // cancel; that's a pretty critical detail!
        let terminationEventMapper = (seq) => {
          return seq.path.wasCancelled;
        }

        expect(result.inputs.map(terminationEventMapper)).to.deep.equal(testObj.inputs.map(terminationEventMapper));
      });
    }

    // List all relevant fixtures in src/test/resources/json.
    let testRecordings = [
      'desktopRoamAndReturn',
      'mobileSafeZoneCancel',
      'mobileProximityApproach',
      'embeddedBorderCancel',
      'hardBorderCancel',
      'popupLongRoamingEnd',
      'popupShimCancel',
      'popupSafePersistence',
      'basicMultitouch'
    ];

    for(let recordingID of testRecordings) {
      it(`${recordingID}.json`, function() {
        this.timeout(2 * testconfig.timeouts.standard);
        let testObj = __json__['receiver/' + recordingID];

        // 'describe' has a notably different `this` reference than `it`, `before`, etc,
        // hence the `.call` construction.
        return replayAndCompare.call(this, testObj);
      });
    }
  });
});