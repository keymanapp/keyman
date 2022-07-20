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

  describe('other tests', function() {
    it("starts in roaming zone are ignored", function() {
      let playbackEngine = new Testing.InputSequenceSimulator(this.controller);
      let recorder = new Testing.SequenceRecorder(this.controller);
      let layout = new Testing.FixtureLayoutConfiguration("screen2", "bounds1", "full", "safe-loose");
      this.controller.layoutConfiguration = layout;

      playbackEngine.replayTouchSample(/*relative coord:*/ {targetX: 10, targetY: -5},
                                        /*state:*/         "start",
                                        /*identifier:*/    1,
                                        /*otherTouches:*/  [],
                                        /*targetElement:*/ this.controller.recognizer.config.maxRoamingBounds
                                      );

      assert.equal(recorder.count, 0, "Input starting in roaming area was not ignored!");
    });
  });
});