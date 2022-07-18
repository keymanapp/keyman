var assert = chai.assert;

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
  });

  it('Initial attempt', function() {
    let playbackEngine = new Testing.InputSequenceSimulator(this.controller);
    let testObj = __json__.canaryRecording; // See src/test/resources/json/canaryRecording.json

    playbackEngine.replay(testObj.set, testObj.config);
  });
});