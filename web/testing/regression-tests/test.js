// test.js
//***DEPRECATED***

var assert = chai.assert;

// fire the test runner.
console.log('Beginning test suite');

function getKeyboardsList() {
  console.log('Retrieving list of keyboards');
  return new Promise(function(resolve, reject) {
    //return windowLoad.then(() => {
      // Retrieve list of available tests
      let http = new XMLHttpRequest();
      http.onreadystatechange = function() {
        if(http.readyState === XMLHttpRequest.DONE) {
          if(http.status === 200) {
            availableKeyboards = JSON.parse(http.responseText);
            console.log(availableKeyboards.length.toString() + ' keyboard test suites found.');
            resolve();
          } else {
            reject('Failed to list keyboards: '+http.responseText);
          }
        }
      };
      http.open('GET', '/list-keyboards');
      http.send();
 //   });
  });
}

function createTestCases() {
  console.log('createTestCases');

  availableKeyboards.forEach((keyboard) => {
    describe(`Test keyboard ${keyboard.id}`, () => {
      it('should generate a set of results for "all" possible inputs', function() {
        return windowLoad
           .then(function() {
            // console.log('test.js');
            //var k = keyman.getKeyboards();
            //console.log(k);
            //assert.ok(k.length > 0, 'Expected at least one keyboard');
            // We need to construct a set of tests based on loading KMW etc.

        const locator = keyboard.s+'/'+keyboard.id;
        return testRunner.loadTests(locator)
          .then(() => testRunner.runTests(keyboard.id))
          .then(() => testRunner.saveTestResults(locator, testRunner.keyboards[keyboard.id].results));
           });
      }).timeout(0);
    });
  });
}

it('foo', function() {console.log('foo0')});

getKeyboardsList().then(function() {
  createTestCases();
  mocha.suite.run();
});