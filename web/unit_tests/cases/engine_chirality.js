var assert = chai.assert;

describe('Chirality', function() {

  before(function(done) {
    this.timeout(10000);

    fixture.setBase('unit_tests/fixtures');
    setupKMW(null, done, 10000);
  });

  beforeEach(function(done) {
    fixture.load("singleInput.html");
    
    window.setTimeout(function() {
      done()
    }, 50);
  });
  
  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });

  describe('Sequence Testing', function() {
    this.timeout(20000);

    it('Keyboard simulation', function(done) {
      runKeyboardTestFromJSON('/engine_tests/chirality.json', {usingOSK: false}, function() {
          if(keyman.util.device.formFactor == "desktop") {
            runKeyboardTestFromJSON('/engine_tests/chirality.json', {usingOSK: true}, done, assert.equal, 10000);
          } else {
            done();
          }
        }, assert.equal, 10000);

    });
  });
});