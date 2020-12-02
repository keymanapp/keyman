var assert = require('chai').assert;
let KeyboardProcessor = require('../../dist');

// Required initialization setup.
global.com = KeyboardProcessor.com; // exports all keyboard-processor namespacing.

describe('Version Logic', function() {
  it('Should provide a default, fallback value when nothing is specified', function() {
    var fallback = new com.keyman.utils.Version(undefined);
    assert.isTrue(fallback.equals(com.keyman.utils.Version.DEVELOPER_VERSION_FALLBACK));
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
    var v10_0 = new com.keyman.utils.Version("10.0");
    var v10_0_0 = new com.keyman.utils.Version("10.0.0");

    // "Precede" checks
    assert.equal(v9_0_1.compareTo(v9_1_0), -1);
    assert.equal(v9_1_0.compareTo(v10_0_0), -1);
    assert.equal(v9_0_1.compareTo(v10_0_0), -1);

    // Equality checks
    assert.equal(v9_0_1.compareTo(v9_0_1), 0);
    // Tests equal versions where one omits the build number.
    assert.equal( v10_0.compareTo(v10_0_0), 0);

    // Ensures the first "precede" check's return is flipped when the order's flipped.
    assert.equal(v9_1_0.compareTo(v9_0_1), 1);
  });
    
});