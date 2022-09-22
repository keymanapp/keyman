// test-runner

const KEYBOARDS_RELATIVE_PATH = "/keyboards/";
const TEST_BATCH_SIZE = 100; // or Infinity

var receiver;

var windowLoad = new Promise(function(resolve, reject) {
  window.addEventListener('load', function() {
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
      let d0 = document.createElement('div');
      d0.id = 'progressWindow';
      d0.style.width = '100%';
      d0.style.margin = '8px 0';
      d0.style.height = '16px';
      d0.style.border = 'solid 1px #666666';
      d0.style.boxSizing = 'border-box';
      d0.style.padding = '2px';

      let d1 = document.createElement('div');
      d1.id = 'progressPosition';
      d1.style.background = '#44cc44';
      d1.style.height = '100%';
      d1.style.boxSizing = 'border-box';

      d0.appendChild(d1);
      document.body.insertBefore(d0, receiver);
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
  modCodes: com.keyman.text.Codes.modifierCodes,
  keyCodes: com.keyman.text.Codes.keyCodes,
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
    console.log(`Loading script ${path}`);
    return new Promise(function(resolve, reject) {
      let script = document.createElement('script'), hadError = false;

      let errorHandler = function(err) {
        hadError = true;
        console.error(`Error loading ${path}: ${err.message} at ${err.filename}:${err.lineno},${err.colno}`);
        window.onerror = null;
        reject(err.message);
        return true;
      };

      let scriptParseErrorHandler = function(message, source, lineno, columnNumber, error) {
        hadError = true;
        console.error(`Error loading ${path}: ${message} at ${source}:${lineno},${columnNumber}`);
        window.onerror = null;
        reject(message);
        return true;
      };

      window.onerror = scriptParseErrorHandler; // Capture compile / run errors
      script.addEventListener('error', errorHandler); // Capture fs / network errors
      script.src = path;
      script.addEventListener('load', function() {
        window.onerror = null;
        if(hadError) {
          document.head.removeChild(script);
          return;
        }
        resolve();
      });

      document.head.appendChild(script);
    });
  },

  /**
   * Load a tests file and a keyboard from the keyboards repository
   * @param {string} locator
   */
  loadTests: function(locator) {
    console.log('Loading tests + keyboard for '+locator);
    return new Promise(function(resolve, reject) {
      let {shortname, id} = testRunner.parseLocator(locator);
      if(!id) throw new Error('Invalid locator '+locator);

      fetch(KEYBOARDS_RELATIVE_PATH + locator + '/tests/' + id + '.tests')
      .then(function(response) {
        return response.json();
      })
      .then(function(data) {
        testRunner.register(data);
      })
      .then(function() {
        return testRunner.loadScript(KEYBOARDS_RELATIVE_PATH + locator + '/build/' + id + '.js');
      })
      .then(function() {
        console.log('Starting test for '+id);
        let stub = {
          'KN': 'Stub',
          'KI': 'Keyboard_'+id,
          'KL': 'en',
          'KLC': 'en'
        };
        // The KeymanWeb global will always exist, even when we otherwise change API.
        KeymanWeb.registerStub(stub);
        let k = testRunner.keyboards[id];
        keyman.setKeyboardForControl(receiver, id, k.keyboard.languages[0].id);
        document.body.focus();
        receiver.focus();
        keyman.osk.show(false);
        resolve(true);
      })
      .catch(function(reason) {
        // We only die if the keyboard isn't a known failure
        if(knownFailures[id]) {
          console.warn('FAILED TO LOAD '+locator+': '+reason);
          console.warn(`This keyboard is listed as a known failure because "${knownFailures[id].reason}". Not failing the test.`);
          resolve(false);
        } else {
          console.error('FAILED TO LOAD '+locator+': '+reason);
          reject(reason);
        }
      });
    });
  },

  /**
   * Post test results to /save-result
   * @param {string} locator   shortname/id, e.g. k/kayan
   * @param {string|object} results   Array or JSON-coded Array string of results
   */
  saveTestResults: function(locator, results) {
    return new Promise(function(resolve, reject) {
      console.log('Saving test result '+locator);
      let json = testRunner.parseLocator(locator);
      if(!json.id) throw new Error('Invalid locator: '+locator);

      if(results === null || results === undefined) results = {'0': {error: 'no results found'}};

      json.results = typeof results == 'string' ? JSON.parse(results) : results;
      json.engineVersion = keyman.build.toString();
      try {
        json.compilerVersion = eval('new Keyboard_'+json.id+'().KVER');
      } catch(e) {
        json.compilerVersion = '0';
      }

      // Post results to be saved to disk by the back end (test-host.js)
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
   * Validates and parses a locator string (shortname/id, e.g. k/kayan) into an object
   * @param {string} locator   "shortname/id", e.g. "k/kayan"
   * @returns {object}  object with `shortname` and `id` properties
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
    let keys = Object.keys(data.inputTests);
    // We are going to refactor .inputTests from an object into an array with "id" parameters.
    data.inputTests = keys.map((key) => Object.assign({id: key}, data.inputTests[key]));
    this.keyboards[data.keyboard.id] = data;
  },

  /**
   * Removes a keyboard and associated tests from memory
   * @param {string} locator   shortname/id, e.g. k/kayan
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

      // Batch tests into group s of chunkSize. This means we don't
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
    let test = this.keyboards[keyboardId].inputTests.find((v) => v.id == testId);
    if(test !== undefined) {
      //console.log('Running test '+id+':'+index);
      try {
        if(keyman.core) {
          keyman.core.resetContext();
        } else {
          // KMW 12, 13.
          keyman.interface.resetContext();
        }
        receiver.value = test.context || '';
        // Keyman 12 now uses com.keyman.text.KeyEvent
        let e = com.keyman.KeyEvent ? new com.keyman.KeyEvent() : new com.keyman.text.KeyEvent();
        e.Ltarg = receiver;
        e.Lcode = test.key;
        e.Lstates = 0; // caps, etc TODO
        e.LmodifierChange = true;
        e.Lmodifiers = test.modifier ? test.modifier : 0;
        e.LisVirtualKeyCode = true;
        e.LisVirtualKey = true;
        e.vkCode = test.key;

        // Keyman 14 changes the processKeystroke interface
        e.device = keyman.util.device.coreSpec;
        if(keyman.core) {
          keyman.core.keyboardInterface.processKeystroke(com.keyman.dom ? com.keyman.dom.Utils.getOutputTarget(receiver) : receiver, e);
        } else {
          keyman.interface.processKeystroke(keyman.util.physicalDevice, com.keyman.text ? com.keyman.text.Processor.getOutputTarget(receiver) : receiver, e)
        }
        this.keyboards[keyboardId].results[testId] = receiver.value;
      } catch(err) {
        console.warn(err.toString());
        this.keyboards[keyboardId].results[testId] = {error: err.message, filename: err.filename, lineno: err.lineno};
        if(!knownFailures[keyboardId]) {
          return false;
        }
      }
      return true;
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

