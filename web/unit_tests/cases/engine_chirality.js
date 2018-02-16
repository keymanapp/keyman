var assert = chai.assert;

describe('Engine - Chirality', function() {
  this.timeout(20000);

  before(function(done) {
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

  it('Keyboard + OSK simulation', function(done) {
    /* Interestingly, this still works on iOS, probably because we're able to force-set
     * the 'location' property in the simulated event on mobile devices, even when iOS neglects to
     * set it for real events.
     */
    runKeyboardTestFromJSON('/engine_tests/chirality.json', {usingOSK: false}, function() {
        /* We only really care to test the 'desktop' OSK because of how it directly models the modifier keys.
         *
         * The 'phone' and 'layout' versions take shortcuts that bypass any tricky chiral logic;
         * a better test for those would be to ensure the touch OSK is constructed properly.
         */ 
        if(keyman.util.device.formFactor == "desktop") {
          runKeyboardTestFromJSON('/engine_tests/chirality.json', {usingOSK: true}, done, assert.equal, 10000);
        } else {
          done();
        }
      }, assert.equal, 10000);
  });
});