var assert = chai.assert;
var expect = chai.expect;

describe("Layer one - DOM -> InputSequence", function() {
  this.timeout(testconfig.timeouts.standard);

  before(function() {
    fixture.setBase('');
  });

  beforeEach(function(done) {
    fixture.load('host-fixture.html');
    this.controller = new Testing.HostFixtureLayoutController();
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

      let playbackEngine = new Testing.InputSequenceSimulator(this.controller);
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
          return input.touchpoints[0].path.coords.map(sampleCleaner);
        };

        let cleanOriginalSet = testObj.inputs.map(seqCleaner);
        let cleanResultSet   = result .inputs.map(seqCleaner);

        expect(cleanResultSet).to.deep.equal(cleanOriginalSet);

        // Now to compare just the timestamp elements.  We'll tolerate a difference of up to 1.
        // Note:  if using the `replaySync` function instead, disable this section!
        // (Through to the nested for-loop `assert.closeTo`)
        let sampleTimeExtractor = (sample) => sample.t;
        let inputTimeExtractor = (input) => {
          return input.touchpoints[0].path.coords.map(sampleTimeExtractor);
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
          return seq.touchpoints[0].path.wasCancelled;
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