// var assert = chai.assert;

// describe('Engine - Deadkeys', function() {
//   this.timeout(20000);

//   before(function(done) {
//     fixture.setBase('unit_tests/fixtures');
//     setupKMW(null, done, 10000);
//   });

//   beforeEach(function(done) {
//     fixture.load("singleInput.html");
    
//     window.setTimeout(function() {
//       done()
//     }, 50);
//   });
  
//   after(function() {
//     teardownKMW();
//   });

//   afterEach(function() {
//     fixture.cleanup();
//   });

//   it('Keyboard simulation', function(done) {
//     runKeyboardTestFromJSON('/engine_tests/deadkeys.json', {usingOSK: false}, done, assert.equal, 10000);
//   });

//   it('OSK simulation', function(done) {
//     runKeyboardTestFromJSON('/engine_tests/deadkeys.json', {usingOSK: true}, done, assert.equal, 10000);
//   });
// });