var assert = chai.assert;

describe('Compiler - Fixing Ladders', function() {
  before(function(done) {
    this.timeout(kmwconfig.timeouts.scriptLoad);

    fixture.setBase('unit_tests/fixtures');
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

  it('Hieroglyphic V9 key events simulation', function(done) {
    this.timeout(kmwconfig.timeouts.scriptLoad * 2);
    runKeyboardTestFromJSON('/engine_tests/ladder_hieroglyphic_v9.json', {usingOSK: false}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
  });

  it('Hieroglyphic V11 key events simulation', function(done) {
    this.timeout(kmwconfig.timeouts.scriptLoad * 2);
    runKeyboardTestFromJSON('/engine_tests/ladder_hieroglyphic_v11.json', {usingOSK: false}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
  });
});