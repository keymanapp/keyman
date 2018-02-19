var assert = chai.assert;

/*
 *  Start definition of isolated rule tests for validity of `fullContextMatch` (KFCM) components.
 */
var RULE_1_TEST = {
  id: 1,
  // Match condition for rule
  in: ['a', {d: 0}, {d: 1}, 'b'],
  // Start of context relative to cursor
  n: 5,
  ln: 4,
  // Resulting context map
  contextMap: [3, 2, 2, 2],
  contextCache: ['a', 0, 1, 'b'],
  baseSequence: { "output": "ab", "inputs": [
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    // The test has an extra character appended that's not part of the check.
    {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      // Should fail with inverted deadkey ordering at same KC_ position.
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 1:  did not fail when deadkey ordering was inverted."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 1:  failed when extra character context exists in history."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // The test has an extra deadkey appended that's not part of the check.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 1:  out-of-context deadkey caused rule match failure."
  }, {
    sequence: { "output": "ab", "inputs": [
      // Should not fail with a deadkey prepended to the context.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 1:  prepended (out of context) deadkey caused rule match failure."
  }]
};

var RULE_2_TEST = {
  id: 2,
  // Match condition for rule
  in: [{d: 0}, 'a', {d: 0}, {d: 0}, 'b'],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextMap: [2, 2, 1, 1, 1],
  contextCache: [0, 'a', 0, 0, 'b'],
  baseSequence: { "output": "ab", "inputs": [
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
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
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
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
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 2:  prepended deadkey after rule's required deadkey failed to prevent a rule match."
  }]
};

var RULE_3_TEST = {
  id: 3,
  // Match condition for rule
  in: ['a', {d: 0}, {d: 0}, 'b', {d: 0}],
  // Start of context relative to cursor
  n: 6,
  ln: 5,
  // Resulting context map
  contextMap: [3, 2, 2, 2, 1],
  contextCache: ['a', 0, 0, 'b', 0],

  baseSequence: { "output": "ab", "inputs": [
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    // The test has an extra character appended that's not part of the check.
    {"type":"key","key":"c","code":"KeyC","keyCode":66,"modifierSet":0,"location":0}
  ]}
  // No specialized fullMatchDefs here, as any appended deadkeys are automatically 'in context' for rules.
};

var RULE_4_TEST = {
  id: 4,
  // Match condition for rule
  in: ['a', 'b', 'b', 'a', 'c'],
  // Start of context relative to cursor
  n: 5,
  ln: 4,
  // Resulting context map
  contextMap: [5, 4, 3, 2, 1],
  contextCache: ['a', 'b', 'b', 'a'],

  baseSequence: { "output": "ab", "inputs": [
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    // The test has an extra character appended that's not part of the check.
    {"type":"key","key":"c","code":"KeyC","keyCode":66,"modifierSet":0,"location":0}
  ]}
};

var RULE_5_TEST = {
  id: 5,
  // Match condition for rule
  in: [{d: 1}, {d: 2}, {d: 0}, {d: 1}, {d: 2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextMap: [0, 0, 0, 0, 0],
  contextCache: [1, 2, 0, 1, 2],

  baseSequence: { "output": "ab", "inputs": [
    // Testing with an extra deadkey at the start.
    {"type":"key","key":"0","code":"Digit0","keyCode":48,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
    {"type":"key","key":"0","code":"Digit0","keyCode":48,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
  ]}
};

var RULE_SET = [ RULE_1_TEST, RULE_2_TEST, RULE_3_TEST, RULE_4_TEST ];

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
    it('Extended Context Mapping', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      for(var i = 0; i < RULE_SET.length; i++) {
        var ruleDef = RULE_SET[i];

        // Prepare the context!
        var ruleSeq = new KMWRecorder.InputTestSequence(ruleDef.baseSequence);
        ruleSeq.simulateSequenceOn(inputElem);

        // Now for the real test!
        var res = keyman.interface._BuildExtendedContext(ruleDef.n, ruleDef.ln, inputElem);

        assert.sameOrderedMembers(res, ruleDef.contextCache);

        // Cleanup the context!
        window['keyman'].resetContext();
      }
    });

    // Tests construction of index mapping, which translates extended indices to their original positions.
    it('Context Index Mapping', function() {
      for(var i = 0; i < RULE_SET.length; i++) {
        var ruleDef = RULE_SET[i];
        var res = keyman.interface._BuildContextIndexMap(ruleDef.n, ruleDef.in);
        assert.sameOrderedMembers(res, ruleDef.contextMap);
      }
    });

    // Tests "stage 3" of fullContextMatch - ensuring that all deadkey conditions are met.
    it('Context Matching - Deadkeys and Plain Text only', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      for(var i = 0; i < RULE_SET.length; i++) {
        var ruleDef = RULE_SET[i];
        if(!ruleDef.fullMatchDefs) {
          continue;
        }

        var matchDefs = [{
            sequence: ruleDef.baseSequence,
            result: true,
            msg: "Rule " + ruleDef.id + ":  basic application of rule failed."}].concat(ruleDef.fullMatchDefs);

        for(var j = 0; j < matchDefs.length; j++) {
          // Prepare the context!
          var matchTest = matchDefs[j];
          var ruleSeq = new KMWRecorder.InputTestSequence(matchTest.sequence);
          ruleSeq.simulateSequenceOn(inputElem);

          // Now for the real test!
          var res = keyman.interface.fullContextMatch(ruleDef.n, inputElem, ruleDef.in);

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