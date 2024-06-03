import { assert, expect } from 'chai';
import sinon from 'sinon';

import type { GestureDebugSource, InputSample, SerializedGestureSource } from '@keymanapp/gesture-recognizer';

import {
  HostFixtureLayoutController,
  InputSequenceSimulator
} from '#tools';

function isOnAndroid() {
  const agent=navigator.userAgent;
  return agent.indexOf('Android' >= 0);
}

const loc = document.location;
// config.testFile generally starts with a '/', with the path resembling the actual full local
// filesystem for the drive.
const domain = `${loc.protocol}/${loc.host}`

async function fetchRecording(jsonFilename) {
  const jsonResponse = await fetch(new URL(`${domain}/resources/json/${jsonFilename}.json`));
  return await jsonResponse.json();
}

describe("Layer one - DOM -> InputSequence", function() {
  this.timeout(20000);

  let controller: HostFixtureLayoutController;

  beforeEach(function(done) {
    controller = new HostFixtureLayoutController();
    controller.connect().then(() => done());
  });

  afterEach(function() {
    controller.destroy();
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
      let playbackEngine = new InputSequenceSimulator(controller);

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

      let resultPromise = playbackEngine.replayAsync(testObj);

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

            assert.isOk(sampleResult, `An expected sample was missing during simulation - failed at path entry ${j}, path ${i}`);
            assert.isOk(sampleOriginal, `An extra sample was generated during simulation - failed at path entry ${j}, path ${i}`);

            // During test runs against a real Android device, we tend to get almost, but not-quite, integer targetX and targetY values.
            expect(sampleResult.targetX).to.be.closeTo(sampleOriginal.targetX, 1e-4, `Mismatch in x-coord at path entry ${j}, path ${i}`);
            expect(sampleResult.targetY).to.be.closeTo(sampleOriginal.targetY, 1e-4, `Mismatch in y-coord at path entry ${j}, path ${i}`);
          }
        }

        // Now to compare just the timestamp elements.  We'll tolerate a difference of up to 1.
        // Note:  if using the `replaySync` function instead, disable this section!
        // (Through to the nested for-loop `assert.closeTo`)
        let sampleTimeExtractor = (sample: InputSample<any>) => sample.t;
        let inputTimeExtractor = (input: GestureDebugSource<any> | SerializedGestureSource) => {
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
      it(`${recordingID}.json`, async function() {
        let testObj = await fetchRecording('receiver/' + recordingID);

        // 'describe' has a notably different `this` reference than `it`, `before`, etc,
        // hence the `.call` construction.
        return replayAndCompare.call(this, testObj);
      });
    }
  });
});