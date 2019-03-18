let allKeyboards = [];

function addKeyboardsList(keyboards) {
  let select = document.getElementById('keyboards');
  keyboards.forEach(element => {
    let option = document.createElement('option');
    option.value = element.s + '/' + element.id;
    option.innerText = option.value;
    select.appendChild(option);
    allKeyboards.push(option.value);
  });
}

window.addEventListener('load', function() {
  console.log('windowLoad');

  // Retrieve list of available tests
  let http = new XMLHttpRequest();
  http.onreadystatechange = function() {
    if(http.readyState === XMLHttpRequest.DONE) {
      if(http.status === 200) {
        addKeyboardsList(JSON.parse(http.responseText));
      } else {
        alert('Failed to list keyboards: '+http.responseText);
      }
    }
  };
  http.open('GET', '/list-keyboards');
  http.send();
});

function loadTests() {
  testRunner.loadTests(keyboards.value).then(function() {
    alert('Loaded');
  }, function(err) {
    alert(err.message+' in '+err.filename+':'+err.lineno);
  });
}

/**
 * Post test results to /save-result
 * @param {string} locator 
 * @param {string|object} results
 */
function saveTestResults(locator, results) {
  return new Promise(function(resolve, reject) {
    let json = testRunner.parseLocator(locator);
    if(!json.id) throw new Error('Invalid locator: '+locator);
    json.results = typeof results == 'string' ? JSON.parse(results) : results;

    // Post results to be saved to disk by index.js
    let http = new XMLHttpRequest();
    http.onreadystatechange = function() {
      if(http.readyState === XMLHttpRequest.DONE) {
        if(http.status === 200) {
          resolve(http.responseText);
        } else {
          reject('Failed to save test results: '+http.responseText);
        }
      }
    };
    http.open('POST', '/save-results');
    http.setRequestHeader('Content-Type', 'application/json');
    http.send(JSON.stringify(json));
  });
}

/**
 * Runs through every keyboard listed in allKeyboards,
 * runs the test, then saves the result
 */
function loadRunAndSaveAllTests() {
  //var k = 0;
  loadRunAndSaveTest(0);
}

function loadRunAndSaveTest(n) {
  let locator = allKeyboards[n];
  let {shortname, id} = testRunner.parseLocator(locator);
  keyboards.value = locator; // tracks progress visually
  testRunner.loadTests(locator)
    .then(() => testRunner.runTests(id))
    .then(() => saveTestResults(locator, testRunner.keyboards[id].results))
    .then(() => {
      if(++n < allKeyboards.length) {
        loadRunAndSaveTest(n);
      }
    }).catch(() => {
      console.log('Skipping test');
      if(++n < allKeyboards.length) {
        loadRunAndSaveTest(n);
      }
    });
}

function runTests() {
  let {shortname, id} = testRunner.parseLocator(keyboards.value);
  testRunner.runTests(id).then(function(results) {
    masterJSON.value = JSON.stringify(testRunner.keyboards[id].results, null, 2);
  });
}