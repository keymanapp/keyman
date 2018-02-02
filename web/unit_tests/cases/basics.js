var assert = chai.assert;


describe('KeymanWeb Initialization', function() {
  beforeEach(function(done) {
    fixture.setBase('fixtures');

    fixture.load("keymanScripts.html", "singleInput.html");

    setTimeout(function() {
      // Special Mocha trick - this forces Mocha to allow a page initialization time.
      done();
    }, 100);
  });
  
  afterEach(function() {
    fixture.cleanup();
  });
  
  describe('Initialization', function() {
    it('window.keyman should exist.', function() {
      assert.isNotNull(window.keyman, "KeymanWeb's base object doesn't exist!");
    });
    it('KMW should attach to the input element.', function() {
      var singleton = document.getElementById('singleton');
      assert.isTrue(keyman.isAttached(singleton), "KeymanWeb did not automatically attach to the element!");
    });
  });
});