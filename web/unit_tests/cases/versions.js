var assert = chai.assert;

describe('Version Logic', function() {
  this.timeout(kmwconfig.timeouts.standard);

  before(function(done) {
    this.timeout(kmwconfig.timeouts.scriptLoad);
    setupKMW(null, done, kmwconfig.timeouts.scriptLoad);
  });
  
  after(function() {
    teardownKMW();
  });
  

  it('Should properly process a simple major.minor version string.', function() {
    var version = new com.keyman.utils.Version("1.2");
    assert.equal(version.major, 1);
    assert.equal(version.minor, 2);
  });

  it('Should handle long/deep version specifications.', function() {
    var version = new com.keyman.utils.Version("1.2.3.4.5.6");
    assert.equal(version.components.length, 6);
    assert.equal(version.major, 1);
    assert.equal(version.minor, 2);
  });

  it('Should properly compare two versions.', function() {
    var v9_0_1 = new com.keyman.utils.Version("9.0.1");
    var v9_1_0 = new com.keyman.utils.Version("9.1.0");
    var v10_0_0 = new com.keyman.utils.Version("10.0.0");

    assert.isTrue(v9_0_1.precedes(v9_1_0));
    assert.isTrue(v9_1_0.precedes(v10_0_0));
    assert.isTrue(v9_0_1.precedes(v10_0_0));
  });
    
});