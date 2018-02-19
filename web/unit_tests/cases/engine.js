var assert = chai.assert;

/*
 *  Start definition of isolated rule tests for validity of `fullContextMatch` (KFCM) components.
 */
var RULE_1_TEST = {
  // Match condition for rule
  in: ['a', {d: 0}, {d: 1}, 'b'],
  // Start of context relative to cursor
  n: 5,
  // Resulting context map
  contextMap: [3, 2, 2, 2],
  deadkeyMatchDefs: [{
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","key":"c","code":"KeyC","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 1:  basic application of rule failed."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      // Should fail with inverted deadkey ordering at same KC_ position.
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","key":"c","code":"KeyC","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 1:  did not fail when deadkey ordering was inverted."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","key":"c","code":"KeyC","keyCode":66,"modifierSet":0,"location":0},
      // Should not fail with a deadkey out of context.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 1:  out-of-context deadkey caused rule match failure."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // Should fail with a deadkey appended to the rule's context.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","key":"c","code":"KeyC","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 1:  in-context unmatched deadkey did not cause rule match failure."
  }, {
    sequence: { "output": "ab", "inputs": [
      // Should not fail with a deadkey prepended to the context.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","key":"c","code":"KeyC","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 1:  prepended (out of context) deadkey caused rule match failure."
  }]
};

var RULE_2_TEST = {
  // Match condition for rule
  in: [{d: 0}, 'a', {d: 0}, {d: 0}, 'b'],
  // Start of context relative to cursor
  n: 5,
  // Resulting context map
  contextMap: [2, 2, 1, 1, 1],
  deadkeyMatchDefs: [{
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 2:  basic application of duplicate, same position deadkey rule match failed."
  }, {
    sequence: { "output": "ab", "inputs": [
      // Omission of the first deadkey should result in failure.
      //{"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 2:  required prepended deadkey omission did not prevent a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      // Omission of the one of the duplicated deadkeys should result in failure.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 2:  omitting one copy of a required duplicated deadkey did not prevent a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      // Triplifying deadkeys in the center should result in failure.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 2:  triple matching deadkeys where only two required did not prevent a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      // Should not fail with a duplicate deadkey prepended to the context.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 2:  duplicate prepended deadkey prevented a rule match when only one was required."
  }, {
    sequence: { "output": "ab", "inputs": [
      // Should not fail with an unrelated deadkey prepended to the context before the required one.
      {"type":"key","key":"2","code":"Digit2","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 2:  prepended deadkey placed before rule's deadkey prevented a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      // Should not fail with a duplicate deadkey prepended to the context.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 2:  prepended deadkey after rule's required deadkey failed to prevent a rule match."
  }]
};

var RULE_3_TEST = {
  // Match condition for rule
  in: ['a', {d: 0}, {d: 0}, 'b', {d: 0}],
  // Start of context relative to cursor
  n: 6,
  // Resulting context map
  contextMap: [3, 2, 2, 2, 1]
  
  // No specialized deadkeyMatchDefs here, as any appended deadkeys are automatically 'in context' for rules.
};

var RULE_SET = [ RULE_1_TEST, RULE_2_TEST, RULE_3_TEST ];

/*
 *  End definition of isolated rule testing.
 */

describe('Engine', function() {

  before(function(done) {
    this.timeout(10000);

    fixture.setBase('unit_tests/fixtures');
    setupKMW(null, done, 10000);
  });

  beforeEach(function(done) {
    fixture.load("singleInput.html");
    
    window.setTimeout(function() {
      done()
    }, 50);
  });
  
  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });
  
  describe('Keyboard Loading', function() {
    it('Local', function(done) {
      this.timeout(10000);

      var test_callback = function() {
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set correctly!");
        keyman.removeKeyboards('lao_2008_basic');
        assert.equal(keyman.getActiveKeyboard(), '', "Keyboard not removed correctly!");
        done();
      }

      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", test_callback, 10000);
    });
  });

  // Performs basic processing system checks/tests to ensure the sequence testing
  // is based on correct assumptions about the code.
  describe('Processing', function() {
    before(function(done){
      this.timeout = 10000;
      // We use this keyboard here because it defines a few deadkeys for us to work with.
      loadKeyboardFromJSON("/keyboards/test_simple_deadkeys.json", done, 10000);
    });

    beforeEach(function() {
      var inputElem = document.getElementById('singleton');
      inputElem.value = "";
    });

    after(function() {
      keyman.removeKeyboards('test_simple_deadkeys');
      fixture.cleanup();
    });

    // Tests "stage 1" of fullContextMatch - ensuring that a proper context index map is built.
    it('Context Index Mapping', function() {
      for(var i = 0; i < RULE_SET.length; i++) {
        var ruleDef = RULE_SET[i];
        var res = keyman.interface._BuildContextIndexMap(ruleDef.n, ruleDef.in);
        assert.sameMembers(res, ruleDef.contextMap);
      }
    });

    // Tests "stage 2" of fullContextMatch - ensuring that all deadkey conditions are met.
    it('Context Deadkey Matching', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      for(var i = 0; i < RULE_SET.length; i++) {
        var ruleDef = RULE_SET[i];
        if(!ruleDef.deadkeyMatchDefs) {
          continue;
        }

        for(var j = 0; j < ruleDef.deadkeyMatchDefs.length; j++) {
          // Prepare the context!
          var matchTest = ruleDef.deadkeyMatchDefs[j];
          var ruleSeq = new KMWRecorder.InputTestSequence(matchTest.sequence);
          ruleSeq.simulateSequenceOn(inputElem);

          // Now for the real test!
          var res = keyman.interface._FullDeadkeyMatch(ruleDef.contextMap, inputElem, ruleDef.in);

          var msg = matchTest.msg;
          if(!msg) {
            msg = "Deadkeys incorrectly reported as " + (matchTest.result ? "unmatched!" : "matched!"); 
          }
          assert.equal(res, matchTest.result, msg);

          // Cleanup the context!
          window['keyman'].resetContext();
        }
      }
    });

    // TODO:  add a 'resetContext' test!
  })

  // Performs basic processing system checks/tests to ensure the sequence testing
  // is based on correct assumptions about the code.
  describe('Simulation Checks', function() {
    before(function(done){
      this.timeout = 10000;
      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", done, 10000);
    });

    beforeEach(function() {
      var inputElem = document.getElementById('singleton');
      inputElem.value = "";
    });

    after(function() {
      keyman.removeKeyboards('lao_2008_basic');
      fixture.cleanup();
    });

    it('Simple Keypress', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      var lao_s_key_json = {"type": "key", "key":"s", "code":"KeyS","keyCode":83,"modifierSet":0,"location":0};
      var lao_s_event = new KMWRecorder.PhysicalInputEvent(lao_s_key_json);

      lao_s_event.simulateEventOn(inputElem);

      if(inputElem['base']) {
        inputElem = inputElem['base'];
      }
      assert.equal(inputElem.value, "ຫ");
    });

    it('Simple OSK click', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      var lao_s_osk_json = {"type": "osk", "keyID": 'shift-K_S'};
      var lao_s_event = new KMWRecorder.OSKInputEvent(lao_s_osk_json);

      lao_s_event.simulateEventOn(inputElem);

      if(inputElem['base']) {
        inputElem = inputElem['base'];
      }
      assert.equal(inputElem.value, ";");
    });
  })

  describe('Sequence Checks', function() {
    this.timeout(10000);

    it('Keyboard simulation', function(done) {
      runKeyboardTestFromJSON('/engine_tests/basic_lao_simulation.json', {usingOSK: false}, done, assert.equal, 10000);
    });

    it('OSK simulation', function(done) {
      runKeyboardTestFromJSON('/engine_tests/basic_lao_simulation.json', {usingOSK: true}, done, assert.equal, 10000);
    })
  });
});