import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { MinimalKeymanGlobal } from 'keyman/engine/keyboard';
import { JSKeyboardInterface, JSKeyboardProcessor, Mock } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';

import { NodeProctor, RecordedKeystrokeSequence } from '@keymanapp/recorder-core';

const NUL = '\uFFFE';

/*
 * ABOUT THIS TEST SUITE
 * ---------------------
 *
 * This suite contains two types of tests, both designed to test all possible variations
 * of behaviors that `JSKeyboardInterface.fullContextMatch` may be expected to handle.
 *
 * Type 1:  White-box tests for validity of the generated context-cache
 * - uses only the `baseSequence` of each test spec definition; does not
 *   use any `fullMatchDefs` entries.
 * - uses the specified `contextCache` entry of each test spec in assertions
 * - CTRL+F `Tests "stage 1" of fullContextMatch` for more details.
 *
 * Type 2:  Black-box rule-matching tests
 * - uses both `baseSequence` and `fullMatchDefs` entries of a test spec
 * - tests that each simulation sequence's output either passes or fails against
 *   the specified rule, asserting against the specified `result` value.
 *   - Currently, `result` is always `true` for each spec's `baseSequence`.
 *     It should be a removable limitation, though.
 */

let device = {
  formFactor: 'desktop',
  OS: 'windows',
  browser: 'native'
}

/** @type {KeyboardInterface} */
let keyboardWithHarness;

function runEngineRuleSet(ruleSet, defaultNoun) {
  defaultNoun = defaultNoun ? defaultNoun : "Rule";

  for(var i = 0; i < ruleSet.length; i++) {
    var ruleDef = ruleSet[i]; // for example, DEADKEY_TEST_1.
    keyboardWithHarness.resetContextCache();

    var matchDefs = [{
        sequence: ruleDef.baseSequence,
        result: true,
        msg: "Rule " + ruleDef.id + ":  basic application of rule failed."}
      ].concat(ruleDef.fullMatchDefs ? ruleDef.fullMatchDefs : []);

    for(var j = 0; j < matchDefs.length; j++) {
      // Prepare the context!
      var matchTest = matchDefs[j];
      var ruleSeq = new RecordedKeystrokeSequence(matchTest.sequence);
      let proctor = new NodeProctor(keyboardWithHarness, device, assert.equal);

      // We want to specify the OutputTarget for this test; our actual concern is the resulting context.
      var target = new Mock();
      ruleSeq.test(proctor, target);

      // Now for the real test!
      let processor = new JSKeyboardProcessor(device);
      processor.keyboardInterface = keyboardWithHarness;
      var res = processor.keyboardInterface.fullContextMatch(ruleDef.n, target, ruleDef.rule);

      var msg = matchTest.msg;
      if(!msg) {
        msg = defaultNoun + " incorrectly reported as " + (matchTest.result ? "unmatched!" : "matched!");
      }
      assert.equal(res, matchTest.result, msg);
    }
  }
}

//#region Test Spec Definitions

// Unfortunately, at present, this is all handwritten stuff crafted from partial Recorder use.  Might should make
// a page that writes this format instead.  Note that I've omitted the eventSpec part here, since these tests are
// Node-only.

/*
 *  Start definition of isolated rule tests for validity of `fullContextMatch` (KFCM) components.
 */

 /* Keyman language equivalent:
 *
 * dk(1) > 'success'
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
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      // Does it fail with a different deadkey in the position?
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}
    ]},
    result: false,
    msg: "Rule 1:  did not fail when incorrect deadkey was present."
  }, {
    sequence: { "output": "", "inputs": [
      // Slightly out-of-context deadkey shouldn't affect the match.
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true},
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}
    ]},
    result: true,
    msg: "Rule 1:  failed when extra deadkey context exists in history."
  }, {
    sequence: { "output": "", "inputs": [
      // Slightly out-of-context deadkey shouldn't affect the match.
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true},
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}
    ]},
    result: false,
    msg: "Rule 1:  did not fail upon incorrect deadkey ordering at top of context."
  }]
};

/* Keyman language equivalent:
 *
 * 'a' dk(0) dk(1) 'b' > 'success'
 */
var DEADKEY_TEST_2 = {
  id: 2,
  // Match condition for rule
  rule: ['a', {t:'d', d: 0}, {t:'d', d: 1}, 'b'],
  // Start of context relative to cursor
  n: 5,
  ln: 4,
  // Resulting context map
  contextCache: ['a', 0, 1, 'b'],
  baseSequence: { "output": "abc", "inputs": [
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    // The test has an extra character appended that's not part of the check.
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //c
  ]},
  fullMatchDefs: [{
    sequence: { "output": "abc", "inputs": [
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true},
      // Should fail with inverted deadkey ordering at same KC_ position.
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true},
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true},
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true},
      // The test has an extra character appended that's not part of the check.
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}
    ]},
    result: false,
    msg: "Rule 2:  did not fail when deadkey ordering was inverted."
  }, {
    sequence: { "output": "cabc", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      // The test has an extra character appended that's not part of the check.
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //c
    ]},
    result: true,
    msg: "Rule 2:  failed when extra character context exists in history."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      // The test has an extra deadkey appended that's not part of the check.
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //1
    ]},
    result: true,
    msg: "Rule 2:  out-of-context deadkey caused rule match failure."
  }, {
    sequence: { "output": "abc", "inputs": [
      // Should not fail with a deadkey prepended to the context.
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      // The test has an extra character appended that's not part of the check.
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //c
    ]},
    result: true,
    msg: "Rule 2:  prepended (out of context) deadkey caused rule match failure."
  }]
};

/* Keyman language equivalent:
 *
 * dk(0) 'a' dk(0) dk(0) 'b' > 'success'
 */
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
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
  ]},
  fullMatchDefs: [{
    sequence: { "output": "ab", "inputs": [
      // Omission of the first deadkey should result in failure.
      //{"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: false,
    msg: "Rule 3:  required prepended deadkey omission did not prevent a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      // Omission of the one of the duplicated deadkeys should result in failure.
      //{"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //b
    ]},
    result: false,
    msg: "Rule 3:  omitting one copy of a required duplicated deadkey did not prevent a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      // Triplifying deadkeys in the center should result in failure.
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //b
    ]},
    result: false,
    msg: "Rule 3:  triple matching deadkeys where only two required did not prevent a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      // Should not fail with a duplicate deadkey prepended to the context.
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: true,
    msg: "Rule 3:  duplicate prepended deadkey prevented a rule match when only one was required."
  }, {
    sequence: { "output": "ab", "inputs": [
      // Should not fail with an unrelated deadkey prepended to the context before the required one.
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: true,
    msg: "Rule 3:  prepended deadkey placed before rule's deadkey prevented a rule match."
  }, {
    sequence: { "output": "ab", "inputs": [
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      // Should fail when unrelated, prepended deadkey actually comes after the required one.
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: false,
    msg: "Rule 3:  prepended deadkey after rule's required deadkey failed to prevent a rule match."
  }]
};

/* Keyman language equivalent:
 *
 * 'a' dk(0) dk(0) 'b' dk(0) > 'success'
 */
var DEADKEY_TEST_4 = {
  id: 4,
  // Match condition for rule
  rule: ['a', {t:'d', d: 0}, {t:'d', d: 0}, 'b', {t:'d', d: 0}],
  // Start of context relative to cursor
  n: 6,
  ln: 5,
  // Resulting context map
  contextCache: ['a', 0, 0, 'b', 0],

  baseSequence: { "output": "abc", "inputs": [
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    // The test has an extra character appended that's not part of the check.
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //c
  ]}
  // No specialized fullMatchDefs here, as any appended deadkeys are automatically 'in context' for rules.
};

/* Keyman language equivalent:
 *
 * 'a' 'b' 'b' 'a' > 'success'
 */
var DEADKEY_TEST_5 = {
  id: 5,
  // Match condition for rule
  rule: ['a', 'b', 'b', 'a'],
  // Start of context relative to cursor
  n: 5,
  ln: 4,
  // Resulting context map
  contextCache: ['a', 'b', 'b', 'a'],

  baseSequence: { "output": "abbac", "inputs": [
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    // The test has an extra character appended that's not part of the check.
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //c
  ]}
};

/* Keyman language equivalent:
 *
 * dk(1) dk(2) dk(0) dk(1) dk(2) > 'success'
 */
var DEADKEY_TEST_6 = {
  id: 6,
  // Match condition for rule
  rule: [{t:'d', d: 1}, {t:'d', d: 2}, {t:'d', d: 0}, {t:'d', d: 1}, {t:'d', d: 2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: [1, 2, 0, 1, 2],

  baseSequence: { "output": "", "inputs": [
    // Testing with an extra deadkey at the start.
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
    {"type":"key","keyCode":51,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //3
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
    {"type":"key","keyCode":51,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //3
  ]}
};

/* Keyman language equivalent:
 *
 * 'c' 'a' 'b' context(3) context(2) > 'success'
 */
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
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 1: Plain text mismatch with successful context() statements is matching the rule."
  }, {
    sequence: { "output": "cabaa", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 1: mismatched context() rule component is not failing the rule."
  }]
};

/* Keyman language equivalent:
 *
 * store(bc) 'bc'
 * 'c' 'a' any(bc) context(3) 'a' > 'success'
 */
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
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: true,
    msg: "Rule 2: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { "output": "cabca", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 2: 'any' and 'context' correspond to mismatching characters, but matched the rule."
  }]
};

/* Keyman language equivalent:
 *
 * store(ac) 'ac'
 * store(bc) 'bc'
 *
 * 'c' any(ac) any(bc) context(3) context(2) > 'success'
 */
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
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: true,
    msg: "Rule 3: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { "output": "ccccc", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //c
    ]},
    result: true,
    msg: "Rule 3: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { "output": "cabab", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: false,
    msg: "Rule 3: context() rule component is matching the incorrect any() component."
  }]
};

/* Keyman language equivalent:
 *
 * store(ab) 'ab'
 * store(bc) 'bc'
 *
 * 'c' any(ab) index(bc, 2) 'a' > 'success'
 */
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
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: true,
    msg: "Rule 1: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { "output": "cabca", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 1: 'any' and 'output' correspond to mismatching characters, but matched the rule."
  }]
};

/* Keyman language equivalent:
 *
 * store(ab) 'ab'
 * store(bc) 'bc'
 *
 * 'c' any(ab) index(bc, 2) index(bc, 2) index(ab, 2) > 'success'
 */
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
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 2: Mismatch with secondary output store did not fail the rule."
  }, {
    sequence: { "output": "cbccb", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: true,
    msg: "Rule 2: Alternate 'any' store character failed to match the rule."
  }]
};

/* Keyman language equivalent:
 *
 * store(ab) 'ab'
 * store(bc) 'bc'
 *
 * 'c' any(ab) any(bc) index(bc, 3) index(ab, 2) > 'success'
 */
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
    {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "cacca", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: true,
    msg: "Rule 3a: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { "output": "cbccb", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: true,
    msg: "Rule 3b: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { "output": "cbbbb", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: true,
    msg: "Rule 3c: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { "output": "cbccc", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //c
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //c
    ]},
    result: false,
    msg: "Rule 3: index() rule check matched the incorrect any()."
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) dk(1) dk(2)
 * any(match) > 'success'
 */
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
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //1
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      {"type":"key","keyCode":51,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //3
    ]},
    result: true,
    msg: "Rule 1: Alternate 'any' store deadkey character failed to match the rule."
  }, {
    sequence: { "output": "c", "inputs": [
      {"type":"key","keyCode":67,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //c
    ]},
    result: false,
    msg: "Rule 1: standard character mysteriously matched a deadkey from a store."
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) 'b' dk(2)
 * any(match) > 'success'
 */
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
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //1
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //2
    ]},
    result: false,
    msg: "Rule 2: deadkey not in store mysteriously matched within an any(store) op."
  }, {
    sequence: { "output": "b", "inputs": [
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //b
    ]},
    result: true,
    msg: "Rule 2: standard character in store failed to match within an any(store) op."
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) dk(1) dk(2)
 * store(abc) 'abc'
 *
 * any(match) index(abc, 1) > 'success'
 */
var DEADKEY_STORE_TEST_3 = {
  id: 3,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},{d:1},{d:2}]},{t:'i',i:"abc", o:1}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [0,'a'],

  baseSequence: { "output": "a", "inputs": [
    {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "a", "inputs": [
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 3a: index in deadkey store not properly tracked by indexOutput."
  }, {
    sequence: { "output": "b", "inputs": [
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: true,
    msg: "Rule 3a: index in deadkey store not properly tracked by indexOutput."
  }, {
    sequence: { "output": "a", "inputs": [
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //2
    ]},
    result: false,
    msg: "Rule 3: incorrectly matched rule when deadkey and character were incorrectly ordered."
  }, {
    sequence: { "output": "bb", "inputs": [
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: false,
    msg: "Rule 3: triggered when rule is one regular character out of context."
  }, {
    sequence: { "output": "b", "inputs": [
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //2
    ]},
    result: false,
    msg: "Rule 3: triggered when rule is one deadkey out of context."
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) dk(1) dk(2)
 * any(match) context(1) > 'success'
 */
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
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //2
  ]},
  fullMatchDefs: [{
    sequence: { "output": "", "inputs": [
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //2
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //1
    ]},
    result: false,
    msg: "Rule 4: context matched to alternate (not selected) any(store) option erroneously."
  }, {
    sequence: { "output": "b", "inputs": [
      {"type":"key","keyCode":51,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //3
      {"type":"key","keyCode":51,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //3
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //b
    ]},
    result: false,
    msg: "Rule 4: triggered when rule is slightly out of context."
  }]
};

/* Keyman language equivalent:
 *
 * nul > 'success'
 */
var NUL_TEST_1 = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'n'}],
  // Start of context relative to cursor
  n: 1,
  ln: 1,
  // Resulting context map
  contextCache: [ NUL ],

  baseSequence: { "output": "", "inputs": []},
  fullMatchDefs: []
};

/* Keyman language equivalent:
 *
 * nul any(abc) > 'success'
 */
var NUL_TEST_2 = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'n'}, {t: 'a', a: ['a', 'b', 'c']}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [ NUL, 'a' ],

  baseSequence: { "output": "a", "inputs": [
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
  ]},
  fullMatchDefs: []
};

/* Keyman language equivalent:
 *
 * nul nul any(abc) context(3) > 'success'
 * 
 * This one may... "stretch" what's actually allowed by Keyman language rules,
 * but we wish to ensure that the actual context management is capable of
 * handling this.
 */
var NUL_TEST_3 = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'n'}, {t: 'n'}, {t: 'a', a: ['a', 'b', 'c']}, {t: 'c', c: 3}],
  // Start of context relative to cursor
  n: 4,
  ln: 4,
  // Resulting context map
  contextCache: [ NUL, NUL, 'a', 'a' ],

  baseSequence: { "output": "a", "inputs": [
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
  ]},
  fullMatchDefs: [
    {
      sequence: { "output": "a", "inputs": [
        {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
        {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
        {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //a
      ]},
      result: false,
      msg: "Rule 2: matched deadkey with nul"
    }, {
      sequence: { "output": "a", "inputs": [
        {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
        {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
        {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //a
      ]},
      result: false,
      msg: "Rule 2: matched character with nul"
    }
  ]
};

/* Keyman language equivalent:
 *
 * nul nul dk(1) any(abc) > 'success'
 * 
 * This may also "stretch" what's actually allowed by Keyman language rules,
 * but we wish to ensure that the actual context management is capable of
 * handling this.
 */
var NUL_TEST_4 = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'n'}, {t: 'n'}, {t: 'd', d: 1}, {t: 'a', a: ['a', 'b', 'c']}],
  // Start of context relative to cursor
  n: 4,
  ln: 4,
  // Resulting context map
  contextCache: [ NUL, NUL, 1, 'a' ],

  baseSequence: { "output": "a", "inputs": [
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
  ]},
  fullMatchDefs: [
    {
      sequence: { "output": "a", "inputs": [
        {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
        {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
        {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true} //a
      ]},
      result: false,
      msg: "Rule 2: matched extra deadkey with nul"
    }
  ]
};

/* Keyman language equivalent:
 *
 * store(match) 'abc'
 * notany(match) any(match) > 'success'
 */
var NOTANY_NUL_TEST_1 = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'a', a:['a','b','c'], n: 1},{t:'a',a:['a', 'b', 'c']}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [1, 'a'],

  baseSequence: { "output": "a", "inputs": [
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "aa", "inputs": [
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 1: did not fail notany on matched character"
  }, {
    sequence: { "output": "a", "inputs": [
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 1: did not fail notany on nul context"
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) dk(1) dk(2)
 * store(abc) 'abc'
 *
 * notany(match) any(abc) > 'success'
 */
var NOTANY_NUL_TEST_2 = {
  id: 2,
  // Match condition for rule
  rule: [{t: 'a', a:[{d:0},{d:1},{d:2}], n: 1},{t:'a',a:['a', 'b', 'c']}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: ['a', 'a'],

  baseSequence: { "output": "aa", "inputs": [
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "a", "inputs": [
      {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 2: did not fail notany on matched deadkey"
  }, {
    sequence: { "output": "a", "inputs": [
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 2: did not fail notany on nul context"
  }]
};

/* Keyman language equivalent:
 *
 * store(first) dk(0) 'b' dk(2)
 * store(second) 'a' dk(1) 'c'
 *
 * notany(first) any(second) > 'success'
 */
var NOTANY_NUL_TEST_3 = {
  id: 3,
  // Match condition for rule
  rule: [{t: 'a', a:[{d:0},'b',{d:2}], n: 1},{t:'a',a:['a', 'b', 'c']}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [1, 'a'],

  baseSequence: { "output": "a", "inputs": [
    {"type":"key","keyCode":50,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //1
    {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
  ]},
  fullMatchDefs: [{
    sequence: { "output": "a", "inputs": [
      {"type":"key","keyCode":49,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //0
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 3: did not properly match a deadkey within a mixed notany store"
  }, {
    sequence: { "output": "ba", "inputs": [
      {"type":"key","keyCode":66,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //b
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 3: did not properly match a character within a mixed notany store"
  }, {
    sequence: { "output": "aa", "inputs": [
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}, //a
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: true,
    msg: "Rule 3: context incorrectly matched a character within a mixed notany store"
  }, {
    sequence: { "output": "a", "inputs": [
      {"type":"key","keyCode":65,"states":10752,"modifiers":0,"modifierChanged":false,"isVirtualKey":true}  //a
    ]},
    result: false,
    msg: "Rule 3: did not fail notany on nul context"
  }]
};

var DEADKEY_RULE_SET = [ DEADKEY_TEST_1, DEADKEY_TEST_2, DEADKEY_TEST_3, DEADKEY_TEST_4,
  DEADKEY_TEST_5, DEADKEY_TEST_6
];
var ANY_CONTEXT_RULE_SET = [ ANY_CONTEXT_TEST_1, ANY_CONTEXT_TEST_2, ANY_CONTEXT_TEST_3 ];
var ANY_INDEX_RULE_SET = [ ANY_INDEX_TEST_1, ANY_INDEX_TEST_2, ANY_INDEX_TEST_3 ];
var DEADKEY_STORE_RULE_SET = [ DEADKEY_STORE_TEST_1, DEADKEY_STORE_TEST_2, DEADKEY_STORE_TEST_3,
   DEADKEY_STORE_TEST_4 ];

var NUL_RULE_SET = [ NUL_TEST_1, NUL_TEST_2, NUL_TEST_3, NUL_TEST_4 ];

var NOTANY_NUL_RULE_SET = [ NOTANY_NUL_TEST_1, NOTANY_NUL_TEST_2, NOTANY_NUL_TEST_3 ];

 var FULL_RULE_SET = [].concat(DEADKEY_RULE_SET, ANY_CONTEXT_RULE_SET, ANY_INDEX_RULE_SET,
   DEADKEY_STORE_RULE_SET, NUL_RULE_SET, NOTANY_NUL_RULE_SET);

  // -----------

//#endregion

describe('Engine - Context Matching', function() {
  before(async function() {
    let keyboardLoader = new NodeKeyboardLoader(new JSKeyboardInterface({}, MinimalKeymanGlobal));
    const keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/test_simple_deadkeys.js'));
    keyboardWithHarness = keyboardLoader.harness;
    keyboardWithHarness.activeKeyboard = keyboard;
  });

  // Tests "stage 1" of fullContextMatch - ensuring that a proper context index map is built.
  it('properly generates extended context data needed by context-matching checks below', function() {
    let matchDefs = FULL_RULE_SET;

    for(var j = 0; j < matchDefs.length; j++) {
      // Prepare the context!
      var ruleDef = matchDefs[j];
      var ruleSeq = new RecordedKeystrokeSequence(ruleDef.baseSequence);
      let proctor = new NodeProctor(keyboardWithHarness, device, assert.equal);

      // We want to specify the OutputTarget for this test; our actual concern is the resulting context.
      var target = new Mock();
      ruleSeq.test(proctor, target);

      // Now for the real test!
      let processor = new JSKeyboardProcessor(device);
      processor.keyboardInterface = keyboardWithHarness;
      var res = processor.keyboardInterface._BuildExtendedContext(ruleDef.n, ruleDef.ln, target);

      assert.sameOrderedMembers(res.valContext, ruleDef.contextCache);
    }
  });

  describe('handles simple deadkey contexts', function() {
    it('for the most basic cases:  DEADKEY_TEST_1', function() {
      runEngineRuleSet([DEADKEY_TEST_1], "Deadkeys");
    });

    it('with deadkey ordering:  DEADKEY_TEST_2', function() {
      runEngineRuleSet([DEADKEY_TEST_2], "Deadkeys");
    });

    it('with repeated deadkeys:  DEADKEY_TEST_3', function() {
      runEngineRuleSet([DEADKEY_TEST_3], "Deadkeys");
    });

    it('with caret after deadkey:  DEADKEY_TEST_4', function() {
      runEngineRuleSet([DEADKEY_TEST_4], "Deadkeys");
    });

    it('with no deadkeys, mid-text:  DEADKEY_TEST_5', function() {
      runEngineRuleSet([DEADKEY_TEST_5], "Deadkeys");
    });

    it('with long sequence of only deadkeys:  DEADKEY_TEST_6', function() {
      runEngineRuleSet([DEADKEY_TEST_6], "Deadkeys");
    });
  });

  describe('handles any + context rule interactions', function() {
    it('for basic context matches, no any:  ANY_CONTEXT_TEST_1', function() {
      runEngineRuleSet([ANY_CONTEXT_TEST_1]);
    });

    it('for any + context-of-any matches:  ANY_CONTEXT_TEST_2', function() {
      runEngineRuleSet([ANY_CONTEXT_TEST_2]);
    });

    it('for rules with multiple any + context-of-any pairs:  ANY_CONTEXT_TEST_3', function() {
      runEngineRuleSet([ANY_CONTEXT_TEST_3]);
    });
  });

  describe('handles any + index context interactions', function() {
    it('for basic any + index matches in context:  ANY_INDEX_TEST_1', function() {
      runEngineRuleSet([ANY_INDEX_TEST_1]);
    });

    it('for one-any to many-index matches in context:  ANY_INDEX_TEST_2', function() {
      runEngineRuleSet([ANY_INDEX_TEST_2]);
    });

    it('for two any + index pairs in rule:  ANY_INDEX_TEST_3', function() {
      runEngineRuleSet([ANY_INDEX_TEST_3]);
    });
  });

  describe('handles interactions with deadkeys in stores', function() {
    it('for any on pure deadkey store:  DEADKEY_STORE_TEST_1', function() {
      runEngineRuleSet([DEADKEY_STORE_TEST_1]);
    });

    it('for any on mixed char+deadkey store:  DEADKEY_STORE_TEST_2', function() {
      runEngineRuleSet([DEADKEY_STORE_TEST_2]);
    });

    it('for index matching any on pure deadkey store:  DEADKEY_STORE_TEST_3', function() {
      runEngineRuleSet([DEADKEY_STORE_TEST_3]);
    });

    it('for context matching any on pure deadkey store:  DEADKEY_STORE_TEST_4', function() {
      runEngineRuleSet([DEADKEY_STORE_TEST_4]);
    });
  });

  describe('handles interactions with nul in requested context', function() {
    it('with only a single nul in context range; no text:  NUL_TEST_1', function () {
      runEngineRuleSet([NUL_TEST_1]);
    });

    it(`with context [nul, 'a'] in range:  NUL_TEST_2`, function () {
      runEngineRuleSet([NUL_TEST_2]);
    });

    it(`with context [nul, 'a', 'a'] in range:  NUL_TEST_3`, function () {
      runEngineRuleSet([NUL_TEST_3]);
    });

    it(`with context [nul, dk(1), 'a'] in range:  NUL_TEST_4`, function () {
      runEngineRuleSet([NUL_TEST_4]);
    });
  });

  describe('handles interactions between notany and nul in context', function() {
    it('with notany against a store with pure characters:  NOTANY_NUL_TEST_1', function() {
      runEngineRuleSet([NOTANY_NUL_TEST_1]);
    });

    it('with notany against a store with pure deadkeys:  NOTANY_NUL_TEST_2', function() {
      runEngineRuleSet([NOTANY_NUL_TEST_2]);
    });

    it('with notany against a store with mixed characters and deadkeys:  NOTANY_NUL_TEST_2', function() {
      runEngineRuleSet([NOTANY_NUL_TEST_3]);
    });
  });
});