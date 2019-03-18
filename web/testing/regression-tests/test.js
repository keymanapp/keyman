// test.js

var assert = chai.assert;

// fire the test runner.
console.log('Beginning');
describe('Regression Test Suite', () => {
  // we need to get a list of all keyboards to test

  describe('#indexOf()', () => {
    it('should return -1 when the value is not present', function() {
      return windowLoad
        .then(() => testRunner.loadTests('k/khmer_angkor'))
        .then(function() {
          // console.log('test.js');
          var k = keyman.getKeyboards();
          console.log(k);
          assert.ok(k.length > 0, 'Expected at least one keyboard');
          // We need to construct a set of tests based on loading KMW etc.
          testRunner.runAllTests();
          //assert.fail('argh');// equal([1,2,3].indexOf(4), 1);
          // We'll need to compare results to inputs next
          //done();
        });
    }).timeout(0);
  });
});