// test-runner

const KEYBOARDS_RELATIVE_PATH = "/keyboards/"; 
const TEST_BATCH_SIZE = 100; // or Infinity

var receiver;

var windowLoad = new Promise(function(resolve, reject) {
  window.addEventListener('load', function() {
    console.log('windowLoad');

    // Initialise KeymanWeb
    keyman.init({
      attachType: 'auto',
      resources: '/web/'
    });

    // Configure receiver - for UI mode we have one already
    receiver = document.getElementById('receiver');
    if(!receiver) {
      // When running tests, we dynamically create the receiver
      receiver = document.createElement('input'); 
      document.body.appendChild(receiver);
    }
    keyman.attachToControl(receiver);
    keyman.setKeyboardForControl(receiver, '', '');
    resolve();
  });
});

function chunk(arr, chunkSize) {
  var R = [];
  for (var i=0,len=arr.length; i<len; i+=chunkSize)
    R.push(arr.slice(i,i+chunkSize));
  return R;
}

var testRunner = {
  modCodes: keyman.osk.modifierCodes,
  keyCodes: keyman.osk.keyCodes,
  keyboards: {},
  max: 0,

  /**
   * Loads a .tests file for the given keyboard, and the corresponding keyboard. Both the 
   * keyboard and the tests file are injected via script into the document.
   * 
   * @param {string} locator  a string in the form of shortname/id, e.g. k/kayan, sil/sil_ipa
   * @returns {Promise} A promise fulfilled when the keyboard and the tests file are ready
   */

  loadScript: function(path) {
    return new Promise(function(resolve, reject) {
      let script = document.createElement('script'), hadError = false;
      let errorHandler = function(err) {
        hadError = true;
        console.error(err);
        window.removeEventListener('error', errorHandler);
        reject(err);
      };

      script.src = path;
      script.addEventListener('load', function() {
        if(hadError) {
          document.head.removeChild(script);
          return;
        }
        resolve();
      });

      script.addEventListener('error', errorHandler); // Capture fs / network errors
      window.addEventListener('error', errorHandler); // Capture compile / run errors

      document.head.appendChild(script);
    });
  },

  loadTests: function(locator) {
    console.log('Loading tests + keyboard for '+locator);
    return new Promise(function(resolve, reject) {
      let {shortname, id} = testRunner.parseLocator(locator);
      if(!id) throw new Error('Invalid locator '+locator);

      //debugger;
      testRunner.loadScript(KEYBOARDS_RELATIVE_PATH + locator + '/build/' + id + '.tests').then(
        function() {
          testRunner.loadScript(KEYBOARDS_RELATIVE_PATH + locator + '/build/' + id + '.js').then(
            function() {
              console.log('Starting test for '+id);
              keyman.interface.registerStub({
                'KN': 'Stub',
                'KI': 'Keyboard_'+id,
                'KL': 'en',
                'KLC': 'en'
              });
              let k = testRunner.keyboards[id]; 
              /*let keyboardLoaded = function() {
                console.log('-- Tests and keyboard loaded');
                keyman.util.removeEventListener('kmw.keyboardregistered', keyboardLoaded);
                resolve();
              }*/
      
              //keyman.addEventListener('keyboardregistered', keyboardLoaded);
      
              //keyman.addKeyboards({name: k.keyboard.name, id: id, filename: KEYBOARDS_RELATIVE_PATH + locator + '/' + k.keyboard.filename, languages: k.keyboard.languages});
              keyman.setKeyboardForControl(receiver, id, k.keyboard.languages[0].id);
              document.body.focus();
              receiver.focus();
              resolve();
//              document.head.removeChild(scriptTests);
  //            window.removeEventListener('error', errorHandler);
      

            },
            function(err) {
              reject(err);
            }
          )
        },
        function(err) {
          reject(err);
        }
      );
    });
  },

  /**
   * Post test results to /save-result
   * @param {string} locator 
   * @param {string|object} results
   */
  saveTestResults: function(locator, results) {
    return new Promise(function(resolve, reject) {
      console.log('saving test '+locator);
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
  },

  /**
   * Runs through every keyboard listed in allKeyboards,
   * runs the test, then saves the result
   */
  loadRunAndSaveAllTests: function() {
    //var k = 0;
    return testRunner.loadRunAndSaveTest(0);
  }, 

  loadRunAndSaveTest: function(n) {
    let locator = 'a/aramaic_hebrew'; //// allKeyboards[n];
    let {shortname, id} = testRunner.parseLocator(locator);
    //keyboards.value = locator; // tracks progress visually
    console.log('loading, running and saving test '+locator);
    return testRunner.loadTests(locator)
      .then(() => testRunner.runTests(id))
      .then(() => testRunner.saveTestResults(locator, testRunner.keyboards[id].results))
      .then(() => {
        if(++n < 1) {//allKeyboards.length) {
          testRunner.loadRunAndSaveTest(n);
        }
      }).catch((err) => {
        console.error('Test failed');
        console.error(err);
        if(++n < 1) {//allKeyboards.length) {
          testRunner.loadRunAndSaveTest(n);
        }
      });
  },


  /**
   * Validates and parses a locator string (shortname/id, e.g. k/kayan) into an object
   * @param {string} locator 
   * @returns {object}
   */
  parseLocator: function(locator) {
    let m = locator.match(/^([a-z0-9_]+)\/([a-z0-9_]+)$/);
    return m ?
      {shortname: m[1], id: m[2]} :
      {shortname: null, id: null};
  },

  /**
   * Called by a .tests file to register a set of input tests
   * @param {object} data  
   */
  register: function(data) {
    this.keyboards[data.keyboard.id] = data;
  },

  /**
   * Removes a keyboard and associated tests from memory
   * @param {string} locator   (shortname/id, e.g. k/kayan)
   */
  unregister: function(locator) {
    let {shortname, id} = this.parseLocator(locator);
    if(!id) throw new Error('Invalid locator '+locator);

    keyman.removeKeyboards(id);
    delete this.keyboards[id];
  },

  /**
   * Run all tests for all loaded keyboards
   */
  runAllTests: function() {
    for(var k in this.keyboards) {
      this.runTests(k);
    }
  },

  /**
   * Run all tests for a given keyboard
   * @param {string} id Keyboard identifier
   */
  runTests: function(keyboardId) {
    return new Promise(function(resolve, reject) {
      this.keyboards[keyboardId].results = {};
      
      console.log('-- Running '+this.keyboards[keyboardId].inputTests.length+' tests for '+keyboardId+'.');
      var chunkSize = TEST_BATCH_SIZE;

      // Batch tests into groups of chunkSize. This means we don't
      // run into issues of the browser stalling on very long runs.

      var tests = chunk(this.keyboards[keyboardId].inputTests, chunkSize), n = 0;
      this.initProgress(tests.length);
      var f = function() {
        var testChunk = tests.shift();
        testChunk.forEach((v) => this.runTest(keyboardId, v.id));
        this.updateProgress(this.max - tests.length);
        if(tests.length) {
          window.setTimeout(f.bind(this), 1);
        } else {
          resolve(this.keyboards[keyboardId].results);
        }
      };

      if(tests.length) window.setTimeout(f.bind(this), 1);
    }.bind(this));
  },

  /**
   * Run a single test for a keyboard
   * @param {string}        id     Keyboard identifier
   * @param {string|number} index  Test identifier
   */
  runTest: function(keyboardId, testId) {
    var test = this.keyboards[keyboardId].inputTests.find((v) => v.id == testId);
    if(test !== undefined) {
      //console.log('Running test '+id+':'+index);
      receiver.value = test.context || '';
      var e = new com.keyman.KeyEvent();
      e.Ltarg = receiver;
      e.Lcode = test.key;
      e.Lstates = 0; // caps, etc TODO
      e.LmodifierChange = true;
      e.Lmodifiers = test.modifier ? test.modifier : 0;
      e.LisVirtualKeyCode = true;
      e.LisVirtualKey = true;
      e.vkCode = test.key;
      try {
        keyman['interface'].processKeystroke(keyman.util.physicalDevice, receiver, e);
        this.keyboards[keyboardId].results[testId] = receiver.value;
      } catch(err) {
        console.error(err);
        this.keyboards[keyboardId].results[testId] = {error: err.message, filename: err.filename, lineno: err.lineno};
      }
    }
  },

  /**
   * Initialises the progress bar when running tests in UI mode
   * @param {number} max Maximum length of progress bar
   */

  initProgress: function(max) {
    this.max = max;
    this.progressPosition = document.getElementById('progressPosition');
    if(this.progressPosition) this.progressPosition.style.width = '0px'; 
  },

  /**
   * Updates the progress bar when running tests in UI mode
   * @param {number} len Current position of progress bar
   */
  updateProgress: function(len) {
    if(this.progressPosition) this.progressPosition.style.width = Math.round(len / this.max * 100).toString() + '%'; 
  }
};

