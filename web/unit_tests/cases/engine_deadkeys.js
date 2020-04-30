var assert = chai.assert;

// TODO:  This engine test currently relies upon a hand-converted version of the keyboard.
//        For production, this should be swapped to a Developer-compiled version.

//        See deadkeys.json, "filename" (line 5) for details.

describe('Engine - Deadkeys', function() {
  this.timeout(kmwconfig.timeouts.scriptLoad);

  before(function(done) {
    fixture.setBase('fixtures');
    setupKMW(null, done, kmwconfig.timeouts.scriptLoad);
  });

  beforeEach(function(done) {
    fixture.load("singleInput.html");
    
    window.setTimeout(function() {
      done()
    }, kmwconfig.timeouts.eventDelay);
  });
  
  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });

  it('Keyboard simulation', function(done) {
    runKeyboardTestFromJSON('/engine_tests/deadkeys.json', {usingOSK: false}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
  });

  it('OSK simulation', function(done) {
    runKeyboardTestFromJSON('/engine_tests/deadkeys.json', {usingOSK: true}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
  });
});