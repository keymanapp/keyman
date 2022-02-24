var assert = chai.assert;

describe('TextSelection', function() {
  this.timeout(5000); //kmwconfig.timeouts.standard);

  beforeEach(function(done) {
    // These tests require use of KMW's device-detection functionality.
    assert.isFalse(com.keyman.karma.DEVICE_DETECT_FAILURE, "Cannot run due to device detection failure.");
    fixture.setBase('fixtures');
    fixture.load("singleTextArea.html");

    this.timeout(kmwconfig.timeouts.scriptLoad*2);
    setupKMW(null, function() {
      loadKeyboardFromJSON("/keyboards/web_context_tests.json", function() {
        keyman.setActiveKeyboard("web_context_tests");
        done();
      }, kmwconfig.timeouts.scriptLoad);
    }, kmwconfig.timeouts.scriptLoad);
  });

  after(function() {
    keyman.removeKeyboards('web_context_tests');
    teardownKMW();
  });

  afterEach(function(done) {
    fixture.cleanup();
    window.setTimeout(function(){
      done();
    }, kmwconfig.timeouts.eventDelay);
  });

  /* Utility functions */

  function supportsInputEvent(done) {
    if(typeof InputEvent != 'function') {
      console.log("InputEvent not supported.");
      done();
      return false;
    }
    return true;
  }

  function setupElement(ele) {
    if(ele['kmw_ip']) {
      ele = ele['kmw_ip'];
      //aliasing = true;
    }

    // A bit of a force-hack to ensure the element is seen as active for the tests.
    com.keyman.dom['DOMEventHandlers'].states._lastActiveElement = ele;
    com.keyman.dom['DOMEventHandlers'].states._activeElement = ele;

    return ele;
  }

  function assertInputSteps(ele, count, setup) {
    return new Promise((resolve, reject) => {
      var i = 0;
      var listener = function() {
        i++;
        if(i == count) {
          ele.removeEventListener("input", listener);
          resolve();
        }
      };
      ele.addEventListener("input", listener);
      setup();
    });
  }

  function instantiateBrowserDriver(ele) {
    return new KMWRecorder.BrowserDriver(setupElement(ele));
  }

  /* Don't run tests on browsers that don't support Input event */

  if(!supportsInputEvent()) return;

  /* Define key event specs */

  var keys = {};
  for (var i = 0; i < 26; i++) {
    var simple = {"type":"key","key":String.fromCharCode(i+97),"code":"Key"+String.fromCharCode(i+65),"keyCode":i+65,"modifierSet":0,"location":0};
    var key = new KMWRecorder.PhysicalInputEventSpec(simple);
    keys[String.fromCharCode(i+65)] = key;
  }
  keys.Backspace = new KMWRecorder.PhysicalInputEventSpec({"type":"key","key":"Backspace","code":"Backspace","keyCode":8,"modifierSet":0,"location":0});

  /* --- Tests --- */

  /**
   * TEST_CONTEXT_BASE
   *
   * Using the web_context_tests keyboard, type abcd. The output after the final key should be '!'.
   */
  it('Should do a basic transform without selection involved', async () => {
    var ele = document.getElementById("singleton");
    var eventDriver = instantiateBrowserDriver(ele);

    await assertInputSteps(ele, 5, () => {
      eventDriver.simulateEvent(keys.E);
      eventDriver.simulateEvent(keys.A);
      eventDriver.simulateEvent(keys.B);
      eventDriver.simulateEvent(keys.C);
      eventDriver.simulateEvent(keys.D);
    });

    assert.strictEqual(ele.value, 'e!', "Expected keys eabcd to transform to e!");
    assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2,2], "Expected caret to be at end of text");
  });

  for(var direction of ['forward', 'backward']) {

    /**
     * TEST_SELECTION
     *
     * Using the web_context_tests keyboard, type abcx. Select bc and type q.
     * The output after the final key should be aqx.
     */
    it('Should do a basic selection replacement, in '+direction+' direction', async () => {
      var ele = document.getElementById("singleton");
      var eventDriver = instantiateBrowserDriver(ele);

      // Step 1: 'abcx'

      await assertInputSteps(ele, 4, () => {
        eventDriver.simulateEvent(keys.A);
        eventDriver.simulateEvent(keys.B);
        eventDriver.simulateEvent(keys.C);
        eventDriver.simulateEvent(keys.X);
      });

      assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

      // Step 2, select 'bc', replace with 'q'

      ele.selectionStart = 1;
      ele.selectionEnd = 3;
      ele.selectionDirection = direction;
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 3], "Sanity check: expected selection to be 'bc'");

      await assertInputSteps(ele, 1, () => {
        eventDriver.simulateEvent(keys.Q);
      });

      assert.strictEqual(ele.value, 'aqx', "Expected selection to have been replaced with 'q'");
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 2], "Expected caret to be after 'q'");
    });

    /**
     * TEST_CONTEXT_SELECTION
     *
     * Using the attached web_context_tests keyboard, type abcx. Select the x character, and type d.
     * The output after the final key should be 'abcd'.
     */
    it('Should ignore context when a selection is made', async () => {
      var ele = document.getElementById("singleton");
      var eventDriver = instantiateBrowserDriver(ele);

      // Step 1: 'abcx'

      await assertInputSteps(ele, 4, () => {
        eventDriver.simulateEvent(keys.A);
        eventDriver.simulateEvent(keys.B);
        eventDriver.simulateEvent(keys.C);
        eventDriver.simulateEvent(keys.X);
      });

      assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

      // Step 2, select 'bc', replace with 'q'

      ele.selectionStart = 3;
      ele.selectionEnd = 4;
      ele.selectionDirection = direction;
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 4], "Sanity check: expected selection to be 'x'");

      await assertInputSteps(ele, 1, () => {
        eventDriver.simulateEvent(keys.D);
      });

      assert.strictEqual(ele.value, 'abcd', "Expected selection to have been replaced with 'd'");
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [4, 4], "Expected caret to be after 'd'");
    });

    /**
     * TEST_CONTEXT_SELECTION_2
     *
     * Using the attached web_context_tests keyboard, type abcx. Select the x character, and
     * delete it with Backspace. Type d. The output after the final key should be !.
     */
     it('Should correctly delete selection with backspace and not lose sync', async () => {
      var ele = document.getElementById("singleton");
      var eventDriver = instantiateBrowserDriver(ele);

      // Step 1: 'abcx'

      await assertInputSteps(ele, 4, () => {
        eventDriver.simulateEvent(keys.A);
        eventDriver.simulateEvent(keys.B);
        eventDriver.simulateEvent(keys.C);
        eventDriver.simulateEvent(keys.X);
      });

      assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

      // Step 2, select 'x', press Backspace

      ele.selectionStart = 3;
      ele.selectionEnd = 4;
      ele.selectionDirection = direction;
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 4], "Sanity check: expected selection to be 'x'");

      await assertInputSteps(ele, 1, () => {
        eventDriver.simulateEvent(keys.Backspace);
      });


      assert.strictEqual(ele.value, 'abc', "Expected selection to have been deleted");
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 3], "Expected caret to be after 'c'");

      // Step 3, type 'd'

      await assertInputSteps(ele, 1, () => {
        eventDriver.simulateEvent(keys.D);
      });

      assert.strictEqual(ele.value, '!', "Expected text to have been transformed to '!'");
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 1], "Expected caret to be after '!'");
    });

    /**
     * TEST_CONTEXT_SELECTION_3
     *
     * Using the attached web_context_tests keyboard, type abcx. Select the x character, and type y.
     * Press Backspace, and type d. The output after the final key should be !.
     */
     it('Should correctly repplace selection and not lose sync', async () => {
      var ele = document.getElementById("singleton");
      var eventDriver = instantiateBrowserDriver(ele);

      // Step 1: 'abcx'

      await assertInputSteps(ele, 4, () => {
        eventDriver.simulateEvent(keys.A);
        eventDriver.simulateEvent(keys.B);
        eventDriver.simulateEvent(keys.C);
        eventDriver.simulateEvent(keys.X);
      });

      assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

      // Step 2, select 'x', press y

      ele.selectionStart = 3;
      ele.selectionEnd = 4;
      ele.selectionDirection = direction;
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 4], "Sanity check: expected selection to be 'x'");

      await assertInputSteps(ele, 1, () => {
        eventDriver.simulateEvent(keys.Y);
      });

      assert.strictEqual(ele.value, 'abcy', "Expected selection to have been replaced with y");
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [4, 4], "Expected caret to be after 'y'");

      // Step 3, press Backspace

      await assertInputSteps(ele, 1, () => {
        eventDriver.simulateEvent(keys.Backspace);
      });

      assert.strictEqual(ele.value, 'abc', "Expected y to have been deleted");
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 3], "Expected caret to be after 'c'");

      // Step 4, type 'd'

      await assertInputSteps(ele, 1, () => {
        eventDriver.simulateEvent(keys.D);
      });

      assert.strictEqual(ele.value, '!', "Expected text to have been transformed to '!'");
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 1], "Expected caret to be after '!'");
    });

    /**
     * TEST_CONTEXT_SELECTION_4
     *
     * Using the attached web_context_tests keyboard, type xabcx.
     * Select the abc characters, and type d. The output after the final key should be xdx.
     */
     it('Should not treat the selection as context', async () => {
      var ele = document.getElementById("singleton");
      var eventDriver = instantiateBrowserDriver(ele);

      // Step 1: 'xabcx'

      await assertInputSteps(ele, 5, () => {
        eventDriver.simulateEvent(keys.X);
        eventDriver.simulateEvent(keys.A);
        eventDriver.simulateEvent(keys.B);
        eventDriver.simulateEvent(keys.C);
        eventDriver.simulateEvent(keys.X);
      });

      assert.strictEqual(ele.value, 'xabcx', "Sanity check: expected text to be 'xabcx'");

      // Step 2, select 'abc', press 'd'

      ele.selectionStart = 1;
      ele.selectionEnd = 4;
      ele.selectionDirection = direction;
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 4], "Sanity check: expected selection to be 'abc'");

      await assertInputSteps(ele, 1, () => {
        eventDriver.simulateEvent(keys.D);
      });

      assert.strictEqual(ele.value, 'xdx', "Expected selection to have been replaced with d");
      assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 2], "Expected caret to be after 'd'");
    });

  }

});
