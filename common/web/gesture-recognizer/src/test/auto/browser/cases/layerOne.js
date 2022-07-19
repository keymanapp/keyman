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
  });

  it('First true unit test', function() {
    let playbackEngine = new Testing.InputSequenceSimulator(this.controller);
    let testObj = __json__.canaryRecording; // See src/test/resources/json/canaryRecording.json

    let result = playbackEngine.replay(testObj);
    assert.equal(testObj.set.length, result.set.length);

    // Removes the timestamp element; we know that this component won't match, as the playback engine
    // doesn't care about it.
    let sampleCleaner = (sample) => { return {targetX: sample.targetX, targetY: sample.targetY} };

    // Returns just the observed, cleaned samples for a sequence object.  The recorded coordinates
    // should match perfectly.
    let seqCleaner = (seq) => { return seq.sequence.samples.map(sampleCleaner) };
    let cleanOriginalSet = testObj.set.map(seqCleaner);
    let cleanResultSet   = result .set.map(seqCleaner);

    expect(cleanResultSet).to.deep.equal(cleanOriginalSet);

    // The 'terminationEvent' property should match.  Any sequence that was "canceled" should still
    // cancel; that's a pretty critical detail!
    let terminationEventMapper = (seq) => seq.terminationEvent;
    expect(result.set.map(terminationEventMapper)).to.deep.equal(testObj.set.map(terminationEventMapper));
  });
});