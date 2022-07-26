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
    // We rely on this function to have the same context as `it` - the test-definition function.
    let replayAndCompare = function(testObj) {
      let playbackEngine = new Testing.InputSequenceSimulator(this.controller);
      let result = playbackEngine.replay(testObj);

      assert.equal(result.inputs.length, testObj.set.length);

      // Removes the timestamp element; we know that this component won't match, as the playback engine
      // doesn't care about it.
      let sampleCleaner = (sample) => { return {targetX: sample.targetX, targetY: sample.targetY} };

      // Returns just the observed, cleaned samples for a sequence object.  The recorded coordinates
      // should match perfectly.
      let seqCleaner = (seq) => {
        if(seq.sequence) {
          // Old style
          return seq.sequence.samples.map(sampleCleaner);
        } else {
          // New style
          return seq.touchpoints[0].path.coords.map(sampleCleaner);
        }
      };
      let cleanOriginalSet = testObj.set.map(seqCleaner);

      // TEMP STATEMENT - logs the new JSON format for old-format JSON recordings.
      // console.log(JSON.stringify(result, null, 2));
      let cleanResultSet   = result .inputs.map(seqCleaner);

      expect(cleanResultSet).to.deep.equal(cleanOriginalSet);

      // The 'terminationEvent' property should match.  Any sequence that was "canceled" should still
      // cancel; that's a pretty critical detail!
      let terminationEventMapper = (seq) => {
        if(seq.terminationEvent) {
          // Old form
          return seq.terminationEvent == 'cancel';
        } else {
          // New form
          return seq.touchpoints[0].path.wasCancelled;
        }
      }
      expect(result.inputs.map(terminationEventMapper)).to.deep.equal(testObj.set.map(terminationEventMapper));
    }

    // List all relevant fixtures in src/test/resources/json.
    let testRecordings = [
      'canaryRecording',
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
        let testObj = __json__[recordingID];

        // 'describe' has a notably different `this` reference than `it`, `before`, etc.
        replayAndCompare.call(this, testObj);
      });
    }
  });
});