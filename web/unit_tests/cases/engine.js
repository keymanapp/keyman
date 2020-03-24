var assert = chai.assert;

function runEngineRuleSet(ruleSet, defaultNoun) {
  var inputElem = document.getElementById('singleton');
  keyman.setActiveElement(inputElem);
  
  if(inputElem['kmw_ip']) {
    inputElem = inputElem['kmw_ip'];
  }

  defaultNoun = defaultNoun ? defaultNoun : "Rule";

  for(var i = 0; i < ruleSet.length; i++) {
    var ruleDef = ruleSet[i];

    var matchDefs = [{
        sequence: ruleDef.baseSequence,
        result: true,
        msg: "Rule " + ruleDef.id + ":  basic application of rule failed."}
      ].concat(ruleDef.fullMatchDefs ? ruleDef.fullMatchDefs : []);

    for(var j = 0; j < matchDefs.length; j++) {
      // Prepare the context!
      var matchTest = matchDefs[j];
      var ruleSeq = new KMWRecorder.InputTestSequence(matchTest.sequence);
      ruleSeq.simulateSequenceOn(inputElem);

      // Now for the real test!
      var res = keyman.textProcessor.keyboardInterface.fullContextMatch(ruleDef.n, inputElem._kmwAttachment.interface, ruleDef.rule);

      var msg = matchTest.msg;
      if(!msg) {
        msg = defaultNoun + " incorrectly reported as " + (matchTest.result ? "unmatched!" : "matched!"); 
      }
      assert.equal(res, matchTest.result, msg);

      // Cleanup the context!
      window['keyman'].resetContext();
    }
  }
}

/*
 *  Start definition of isolated rule tests for validity of `fullContextMatch` (KFCM) components.
 */
var DEADKEY_TEST_1 = {
  id: 1,
  // Match condition for rule
  rule: [{t:'d', d: 1}],
  // Start of context relative to cursor
  n: 1,
  ln: 1,
  // Resulting context map
  contextCache: [1],
  baseSequence: { "output": "", "inputs": [
    {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      // Does it fail with a different deadkey in the position?
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 1:  did not fail when incorrect deadkey was present."
  }, {
    sequence: { "output": "", "inputs": [
      // Slightly out-of-context deadkey shouldn't affect the match.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 1:  failed when extra deadkey context exists in history."
  }, {
    sequence: { "output": "", "inputs": [
      // Slightly out-of-context deadkey shouldn't affect the match.
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 1:  did not fail upon incorrect deadkey ordering at top of context."
  }]
};

var DEADKEY_TEST_2 = {
  id: 2,
  // Match condition for rule
  rule: ['a', {t:'d', d: 0}, {t:'d', d: 1}, 'b'],
  // Start of context relative to cursor
  n: 5,
  ln: 4,
  // Resulting context map
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
    msg: "Rule 2:  did not fail when deadkey ordering was inverted."
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
    msg: "Rule 2:  failed when extra character context exists in history."
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
    msg: "Rule 2:  out-of-context deadkey caused rule match failure."
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
    msg: "Rule 2:  prepended (out of context) deadkey caused rule match failure."
  }]
};

var DEADKEY_TEST_3 = {
  id: 3,
  // Match condition for rule
  rule: [{t:'d', d: 0}, 'a', {t:'d', d: 0}, {t:'d', d: 0}, 'b'],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
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
    msg: "Rule 3:  required prepended deadkey omission did not prevent a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      // Omission of the one of the duplicated deadkeys should result in failure.
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 3:  omitting one copy of a required duplicated deadkey did not prevent a rule match."
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
    msg: "Rule 3:  triple matching deadkeys where only two required did not prevent a rule match."
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
    msg: "Rule 3:  duplicate prepended deadkey prevented a rule match when only one was required."
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
    msg: "Rule 3:  prepended deadkey placed before rule's deadkey prevented a rule match."
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
    msg: "Rule 3:  prepended deadkey after rule's required deadkey failed to prevent a rule match."
  }]
};

var DEADKEY_TEST_4 = {
  id: 4,
  // Match condition for rule
  rule: ['a', {t:'d', d: 0}, {t:'d', d: 0}, 'b', {t:'d', d: 0}],
  // Start of context relative to cursor
  n: 6,
  ln: 5,
  // Resulting context map
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

var DEADKEY_TEST_5 = {
  id: 5,
  // Match condition for rule
  rule: ['a', 'b', 'b', 'a'],
  // Start of context relative to cursor
  n: 5,
  ln: 4,
  // Resulting context map
  contextCache: ['a', 'b', 'b', 'a'],

  baseSequence: { "output": "ab", "inputs": [
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    // The test has an extra character appended that's not part of the check.
    {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0}
  ]}
};

var DEADKEY_TEST_6 = {
  id: 6,
  // Match condition for rule
  rule: [{t:'d', d: 1}, {t:'d', d: 2}, {t:'d', d: 0}, {t:'d', d: 1}, {t:'d', d: 2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: [1, 2, 0, 1, 2],

  baseSequence: { "output": "ab", "inputs": [
    // Testing with an extra deadkey at the start.
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
    {"type":"key","key":"3","code":"Digit3","keyCode":51,"modifierSet":0,"location":0},
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
    {"type":"key","key":"3","code":"Digit3","keyCode":51,"modifierSet":0,"location":0},
  ]}
};

var ANY_CONTEXT_TEST_1 = {
  id: 1,
  // Match condition for rule
  rule: ['c', "a", "b", {t:'c', c:3}, {t:'c', c:2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { "output": "cabba", "inputs": [
    {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 1: Plain text mismatch with successful context() statements is matching the rule."
  }, {
    sequence: { "output": "cabaa", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 1: mismatched context() rule component is not failing the rule."
  }]
};

var ANY_CONTEXT_TEST_2 = {
  id: 2,
  // Match condition for rule
  rule: ['c', 'a', {t:'a', a: "bc"}, {t:'c', c:3}, 'a'],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { "output": "cabba", "inputs": [
    {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 2: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { "output": "cabca", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 2: 'any' and 'context' correspond to mismatching characters, but matched the rule."
  }]
};

var ANY_CONTEXT_TEST_3 = {
  id: 3,
  // Match condition for rule
  rule: ['c', {t:'a', a: "ac"}, {t:'a', a: "bc"}, {t:'c', c:3}, {t:'c', c:2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { "output": "cabba", "inputs": [
    {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 3: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { "output": "ccccc", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 3: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { "output": "cabab", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 3: context() rule component is matching the incorrect any() component."
  }]
};

var ANY_INDEX_TEST_1 = {
  id: 1,
  // Match condition for rule
  rule: ['c', 'a', {t:'a', a: "bc"}, {t:'i', i:"bc", o:3}, 'a'],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { "output": "cabba", "inputs": [
    {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 1: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { "output": "cabca", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 1: 'any' and 'output' correspond to mismatching characters, but matched the rule."
  }]
};

var ANY_INDEX_TEST_2 = {
  id: 2,
  // Match condition for rule
  rule: ['c', {t:'a', a:"ab"}, {t:'i', i:"bc", o:2}, {t:'i', i:"bc", o:2}, {t:'i', i:"ab", o:2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { "output": "cabba", "inputs": [
    {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 2: Mismatch with secondary output store did not fail the rule."
  }, {
    sequence: { "output": "cbccb", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 2: Alternate 'any' store character failed to match the rule."
  }]
};

var ANY_INDEX_TEST_3 = {
  id: 3,
  // Match condition for rule
  rule: ['c', {t:'a', a:"ab"}, {t:'a', a:"bc"}, {t:'i', i:"bc", o:3}, {t:'i', i:"ab", o:2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { "output": "cabba", "inputs": [
    {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 3a: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { "output": "cbccb", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 3b: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { "output": "cbbbb", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 3c: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { "output": "cbccc", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 3: index() rule check matched the incorrect any()."
  }]
};

var DEADKEY_STORE_TEST_1 = {
  id: 1,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},{d:1},{d:2}]}],
  // Start of context relative to cursor
  n: 1,
  ln: 1,
  // Resulting context map
  contextCache: [0],

  baseSequence: { "output": "", "inputs": [
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"3","code":"Digit3","keyCode":51,"modifierSet":0,"location":0},
    ]},
    result: true,
    msg: "Rule 1: Alternate 'any' store deadkey character failed to match the rule."
  }, {
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"c","code":"KeyC","keyCode":67,"modifierSet":0,"location":0},
    ]},
    result: false,
    msg: "Rule 1: standard character mysteriously matched a deadkey from a store."
  }]
};

var DEADKEY_STORE_TEST_2 = {
  id: 2,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},'b',{d:2}]}],
  // Start of context relative to cursor
  n: 1,
  ln: 1,
  // Resulting context map
  contextCache: [0],

  baseSequence: { "output": "", "inputs": [
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
    ]},
    result: false,
    msg: "Rule 2: deadkey not in store mysteriously matched within an any(store) op."
  }, {
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    ]},
    result: true,
    msg: "Rule 2: standard character in store failed to match within an any(store) op."
  }]
};

var DEADKEY_STORE_TEST_3 = {
  id: 3,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},{d:1},{d:2}]},{t:'i',i:"abc", o:1}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [0,'a'],

  baseSequence: { "output": "", "inputs": [
    {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0},
    {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 3a: index in deadkey store not properly tracked by indexOutput."
  }, {
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: true,
    msg: "Rule 3a: index in deadkey store not properly tracked by indexOutput."
  }, {
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 3: incorrectly matched rule when deadkey and character were incorrectly ordered."
  }, {
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 3: triggered when rule is one regular character out of context."
  }, {
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 3: triggered when rule is one deadkey out of context."
  }]
};

var DEADKEY_STORE_TEST_4 = {
  id: 4,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},{d:1},{d:2}]},{t:'c',c:1}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [1,1],

  baseSequence: { "output": "", "inputs": [
    {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
    {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"2","code":"Digit2","keyCode":50,"modifierSet":0,"location":0},
      {"type":"key","key":"1","code":"Digit1","keyCode":49,"modifierSet":0,"location":0}
    ]},
    result: false,
    msg: "Rule 4: context matched to alternate (not selected) any(store) option erroneously."
  }, {
    sequence: { "output": "", "inputs": [
      {"type":"key","key":"3","code":"Digit3","keyCode":51,"modifierSet":0,"location":0},
      {"type":"key","key":"3","code":"Digit3","keyCode":51,"modifierSet":0,"location":0},
      {"type":"key","key":"b","code":"KeyB","keyCode":66,"modifierSet":0,"location":0},
    ]},
    result: false,
    msg: "Rule 4: triggered when rule is slightly out of context."
  }]
};

var DEADKEY_RULE_SET = [ DEADKEY_TEST_1, DEADKEY_TEST_2, DEADKEY_TEST_3, DEADKEY_TEST_4, 
  DEADKEY_TEST_5, DEADKEY_TEST_6 
];
var ANY_CONTEXT_RULE_SET = [ ANY_CONTEXT_TEST_1, ANY_CONTEXT_TEST_2, ANY_CONTEXT_TEST_3 ];
var ANY_INDEX_RULE_SET = [ ANY_INDEX_TEST_1, ANY_INDEX_TEST_2, ANY_INDEX_TEST_3 ];
var DEADKEY_STORE_RULE_SET = [ DEADKEY_STORE_TEST_1, DEADKEY_STORE_TEST_2, DEADKEY_STORE_TEST_3,
  DEADKEY_STORE_TEST_4 ];

var FULL_RULE_SET = [].concat(DEADKEY_RULE_SET, ANY_CONTEXT_RULE_SET, ANY_INDEX_RULE_SET,
  DEADKEY_STORE_RULE_SET);

/*
 *  End definition of isolated rule testing.
 */

describe('Engine', function() {
  this.timeout(kmwconfig.timeouts.scriptLoad);

  before(function(done) {
    fixture.setBase('unit_tests/fixtures');
    setupKMW(null, done, kmwconfig.timeouts.scriptLoad);
  });

  beforeEach(function(done) {
    fixture.load("singleInput.html");
    
    window.setTimeout(function() {
      done()
    }, kmwconfig.timeouts.eventDelay);
  });
  
  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });
  
  describe('Keyboard Loading', function() {
    it('Local', function(done) {
      this.timeout(kmwconfig.timeouts.scriptLoad);

      var test_callback = function() {
        assert.isNotNull(keyman.getKeyboard("lao_2008_basic", "lo"), "Keyboard stub was not registered!");
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set automatically!");
        keyman.removeKeyboards('lao_2008_basic');
        assert.equal(keyman.getActiveKeyboard(), '', "Keyboard not removed correctly!");
        done();
      }

      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", test_callback, kmwconfig.timeouts.scriptLoad, {passive: true});
    });
  });

  // Performs basic processing system checks/tests to ensure the sequence testing
  // is based on correct assumptions about the code.
  describe('Processing', function() {
    this.timeout(kmwconfig.timeouts.standard);

    before(function(done){
      this.timeout(kmwconfig.timeouts.scriptLoad);
      // We use this keyboard here because it defines a few deadkeys for us to work with.
      loadKeyboardFromJSON("/keyboards/test_simple_deadkeys.json", done, kmwconfig.timeouts.scriptLoad);
    });

    beforeEach(function() {
      var inputElem = document.getElementById('singleton');
      inputElem.value = "";
    });

    after(function() {
      keyman.removeKeyboards('test_simple_deadkeys');
      fixture.cleanup();
    });

    it('Store \'Explosion\'', function() {
      // Function defined at top of file; creates supplementary pairs for extended Unicode codepoints.
      var u = toSupplementaryPairString;
      
      var STORES = [
        {smp: false, in: "apple", out: ['a','p','p','l','e']},
        //In JS-escaped form:  "\\ud804\\udd12\\ud804\\udd0d\\ud804\\udd0f\\ud804\\udd10\\ud804\\udd0a\\ud804\\udd05"
        //(Supplementary pairs, copied from the easy_chakma keyboard.)
        {smp: true, in: "𑄒𑄍𑄏𑄐𑄊𑄅", out: ["𑄒","𑄍","𑄏","𑄐","𑄊","𑄅"]},
        // Built in-line via function.  Looks functionally equivalent to "apple", but with SMP characters.
        {smp: true, in: (u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be)), 
          out: [u(0x1d5ba), u(0x1d5c9), u(0x1d5c9), u(0x1d5c5), u(0x1d5be)]}
      ];

      for(var i=0; i < STORES.length; i++) {
        var s = STORES[i];

        String.kmwEnableSupplementaryPlane(s.smp);
        var result = keyman.textProcessor.keyboardInterface._ExplodeStore(s.in);
        assert.sameOrderedMembers(result, s.out, "Failure exploding " + (s.smp ? "SMP" : "non-SMP") + " string value \"" + s.in + "\"");
      }
      String.kmwEnableSupplementaryPlane(false);
    });

    // Tests "stage 1" of fullContextMatch - ensuring that a proper context index map is built.
    it('Extended Context Mapping', function() {
      var inputElem = document.getElementById('singleton');
      keyman.setActiveElement(inputElem);
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      for(var i = 0; i < FULL_RULE_SET.length; i++) {
        var ruleDef = FULL_RULE_SET[i];

        // Prepare the context!
        var ruleSeq = new KMWRecorder.InputTestSequence(ruleDef.baseSequence);
        ruleSeq.simulateSequenceOn(inputElem);

        // Now for the real test!
        var res = keyman.textProcessor.keyboardInterface._BuildExtendedContext(ruleDef.n, ruleDef.ln, inputElem._kmwAttachment.interface);

        assert.sameOrderedMembers(res.valContext, ruleDef.contextCache);

        // Cleanup the context!
        window['keyman'].resetContext();
      }
    });

    // Tests deadkey and plain key interactions for `fullContextMatch`.
    it('Deadkey + Plain Text Rules', function() {
      runEngineRuleSet(DEADKEY_RULE_SET, "Deadkeys");
    });

    // Tests any(), context(), and plain key interactions for `fullContextMatch`.
    it('Any + Context Rules', function() {
      runEngineRuleSet(ANY_CONTEXT_RULE_SET);
    });

    // Tests any(), index(), and plain key interactions for `fullContextMatch`.
    it('Any + Index Rules', function() {
      runEngineRuleSet(ANY_INDEX_RULE_SET);
    });

    // Tests the use of deadkeys IN store rules.
    it('Deadkeys in Stores', function() {
      runEngineRuleSet(DEADKEY_STORE_RULE_SET);
    });

    // TODO:  add a 'resetContext' test!
  })

  describe('Variable Stores', function() {
    this.timeout(kmwconfig.timeouts.scriptLoad + kmwconfig.timeouts.standard);

    beforeEach(function(done) {
      loadKeyboardFromJSON("/keyboards/options_with_save.json", done, kmwconfig.timeouts.scriptLoad);
    });

    it('Backing up and restoring (loadStore/saveStore)', function(done) {
      // Keyboard's default value is 0, corresponding to "no foo."
      var keyboardID = "options_with_save";
      var prefixedKeyboardID = "Keyboard_" + keyboardID;
      var storeName = "foo";

      keyman.setActiveKeyboard(keyboardID, 'en').then(function() {
        // Alas, saveStore itself requires the keyboard to be active!
        KeymanWeb.saveStore(storeName, 1);

        // First, ensure that we get the same thing if we load the value immediately.
        var value = KeymanWeb.loadStore(prefixedKeyboardID, storeName, 0);
        assert.equal(value, 1, "loadStore did not see the value saved to initialize the test before resetting keyboard");

        // Reload the keyboard so that we can test its loaded value.
        keyman.removeKeyboards(keyboardID, true);

        // Now we can reload the keyboard and run the test.
        var remainderOfTest = function() {
          // This requires proper storage to a cookie, as we'll be on a new instance of the same keyboard.
          var value = KeymanWeb.loadStore(prefixedKeyboardID, storeName, 0);
          assert.equal(value, 1, "Did not properly save and reload variable store setting");

          KeymanWeb.saveStore(storeName, 0);
          

          done();
        }

        loadKeyboardFromJSON("/keyboards/options_with_save.json", function() {
          keyman.setActiveKeyboard(keyboardID, 'en').then(remainderOfTest);
        }, kmwconfig.timeouts.scriptLoad);
      });
    });

    it("Multiple-sequence check", function(done) {
      this.timeout(kmwconfig.timeouts.standard + kmwconfig.timeouts.scriptLoad * 3);
      var keyboardID = "options_with_save";
      var storeName = "foo";

      keyman.setActiveKeyboard(keyboardID, 'en').then(function() {
        KeymanWeb.saveStore(storeName, 1);
        keyman.removeKeyboards(keyboardID, true);

        var finalCheck = function() {
          // Reset the keyboard... again.
          keyman.removeKeyboards(keyboardID, true);

          // Second test:  expects option to still be "off" b/c cookies.
          runKeyboardTestFromJSON('/engine_tests/options_with_save_2.json', {usingOSK: false}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
        };

        // First test:  expects option to be "on" from cookie-init setting, emitting "foo.", then turning option "off".
        runKeyboardTestFromJSON('/engine_tests/options_with_save_1.json', {usingOSK: false}, finalCheck, assert.equal, kmwconfig.timeouts.scriptLoad);
      });
    });
  });

  // Performs basic processing system checks/tests to ensure the sequence testing
  // is based on correct assumptions about the code.
  describe('Simulation Checks', function() {
    this.timeout(kmwconfig.timeouts.standard);

    before(function(done){
      this.timeout = kmwconfig.timeouts.scriptLoad;
      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", done, kmwconfig.timeouts.scriptLoad);
    });

    beforeEach(function() {
      var inputElem = document.getElementById('singleton');
      keyman.setActiveElement(inputElem);
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

  describe('Sequence Simulation Checks', function() {
    this.timeout(kmwconfig.timeouts.scriptLoad);

    it('Keyboard simulation', function(done) {
      runKeyboardTestFromJSON('/engine_tests/basic_lao_simulation.json', {usingOSK: false}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
    });

    it('OSK simulation', function(done) {
      runKeyboardTestFromJSON('/engine_tests/basic_lao_simulation.json', {usingOSK: true}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
    })
  });
});