var assert = chai.assert;

describe('Engine', function() {

  before(function(done) {
    this.timeout(10000);

    fixture.setBase('unit_tests/fixtures');
    setupKMW();

    // Pass the initTimer method ousr 'done' callback so it can handle our initialization delays for us.
    initTimer(done);
  });

  beforeEach(function() {
    fixture.load("singleInput.html");
  });
  
  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });
  
  describe('Keyboards', function() {
    it('Successfully loads a locally-stored keyboard.', function(done) {
      var laoStub = fixture.load("/keyboards/lao_2008_basic.json", true);

      keyman.addKeyboards(laoStub);
      keyman.setActiveKeyboard("Keyboard_lao_2008_basic", "lao");

      window.setTimeout(function() {
        assert.isTrue(keyman.getActiveKeyboard() == "Keyboard_lao_2008_basic");
        done();
      }, 1000);
    });
  });
});