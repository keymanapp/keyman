/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { DeviceSpec } from 'keyman/common/web-utils';
import { JSKeyboard, MinimalKeymanGlobal, SyntheticTextStore } from 'keyman/engine/keyboard';
import { ContextEntry, JSKeyboardInterface, JSKeyboardProcessor, KeyboardStoreElement } from 'keyman/engine/js-processor';
import { DEFAULT_PROCESSOR_INIT_OPTIONS, NodeKeyboardLoader } from 'keyman/test/resources';
import { VariableStoreTestSerializer } from 'keyman/test/headless-resources';

import { NodeProctor, RecordedKeystrokeSequence, RecordedPhysicalKeystroke } from '@keymanapp/recorder-core';

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
 * - CTRL+F `Tests 'stage 1' of fullContextMatch` for more details.
 *
 * Type 2:  Black-box rule-matching tests
 * - uses both `baseSequence` and `fullMatchDefs` entries of a test spec
 * - tests that each simulation sequence's output either passes or fails against
 *   the specified rule, asserting against the specified `result` value.
 *   - Currently, `result` is always `true` for each spec's `baseSequence`.
 *     It should be a removable limitation, though.
 */

const device = {
  formFactor: 'desktop',
  OS: 'windows',
  browser: 'native'
} as DeviceSpec;

let keyboardWithHarness: JSKeyboardInterface;

function runEngineRuleSet(ruleSet: TestSpec[], defaultNoun?: string) {
  defaultNoun = defaultNoun || 'Rule';

  for(let i = 0; i < ruleSet.length; i++) {
    const ruleDef = ruleSet[i]; // for example, DEADKEY_TEST_1.
    keyboardWithHarness.resetContextCache();

    const matchDefs = [{
        sequence: ruleDef.baseSequence,
        result: true,
        msg: 'Rule ' + ruleDef.id + ':  basic application of rule failed.'}
      ].concat(ruleDef.fullMatchDefs || []);

    for(let j = 0; j < matchDefs.length; j++) {
      // Prepare the context!
      const matchTest = matchDefs[j];
      const ruleSeq = new RecordedKeystrokeSequence(matchTest.sequence as RecordedKeystrokeSequence);
      const proctor = new NodeProctor(keyboardWithHarness, device, assert.equal);

      // We want to specify the TextStore for this test; our actual concern is the resulting context.
      const textStore = new SyntheticTextStore();
      ruleSeq.test(proctor, textStore);

      const processorInitOptions = { ...DEFAULT_PROCESSOR_INIT_OPTIONS };
      processorInitOptions.keyboardInterface = keyboardWithHarness;

      // Now for the real test!
      const processor = new JSKeyboardProcessor(device, processorInitOptions);
      const res = processor.keyboardInterface.fullContextMatch(ruleDef.n, textStore, ruleDef.rule);

      let msg = matchTest.msg;
      if(!msg) {
        msg = defaultNoun + ' incorrectly reported as ' + (matchTest.result ? 'unmatched!' : 'matched!');
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

interface FullMatchDef {
  sequence: Partial<RecordedKeystrokeSequence>;
  result: boolean;
  msg: string;
}

interface TestSpec {
  id: number;
  rule: ContextEntry[];
  n: number;
  ln: number;
  contextCache: (string | number)[];
  baseSequence: Partial<RecordedKeystrokeSequence>;
  fullMatchDefs?: FullMatchDef[];
}

/* Keyman language equivalent:
 *
 * dk(1) > 'success'
 */
const DEADKEY_TEST_1: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: [{t:'d', d: 1}],
  // Start of context relative to cursor
  n: 1,
  ln: 1,
  // Resulting context map
  contextCache: [1],
  baseSequence: { 'output': '', 'inputs': [
    { 'type': 'key', 'keyCode': 50, 'states': 10752, 'modifiers': 0, 'modifierChanged': false, 'isVirtualKey': true }
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': '', 'inputs': [
      // Does it fail with a different deadkey in the position?
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 1:  did not fail when incorrect deadkey was present.'
  }, {
    sequence: { 'output': '', 'inputs': [
      // Slightly out-of-context deadkey shouldn't affect the match.
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true},
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: 'Rule 1:  failed when extra deadkey context exists in history.'
  }, {
    sequence: { 'output': '', 'inputs': [
      // Slightly out-of-context deadkey shouldn't affect the match.
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true},
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 1:  did not fail upon incorrect deadkey ordering at top of context.'
  }]
};

/* Keyman language equivalent:
 *
 * 'a' dk(0) dk(1) 'b' > 'success'
 */
const DEADKEY_TEST_2: TestSpec = {
  id: 2,
  // Match condition for rule
  rule: ['a', {t:'d', d: 0}, {t:'d', d: 1}, 'b'],
  // Start of context relative to cursor
  n: 5,
  ln: 4,
  // Resulting context map
  contextCache: ['a', 0, 1, 'b'],
  baseSequence: { 'output': 'abc', 'inputs': [
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    // The test has an extra character appended that's not part of the check.
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //c
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'abc', 'inputs': [
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true},
      // Should fail with inverted deadkey ordering at same KC_ position.
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true},
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true},
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true},
      // The test has an extra character appended that's not part of the check.
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 2:  did not fail when deadkey ordering was inverted.'
  }, {
    sequence: { 'output': 'cabc', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      // The test has an extra character appended that's not part of the check.
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //c
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: 'Rule 2:  failed when extra character context exists in history.'
  }, {
    sequence: { 'output': 'ab', 'inputs': [
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      // The test has an extra deadkey appended that's not part of the check.
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //1
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: 'Rule 2:  out-of-context deadkey caused rule match failure.'
  }, {
    sequence: { 'output': 'abc', 'inputs': [
      // Should not fail with a deadkey prepended to the context.
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      // The test has an extra character appended that's not part of the check.
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //c
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: 'Rule 2:  prepended (out of context) deadkey caused rule match failure.'
  }]
};

/* Keyman language equivalent:
 *
 * dk(0) 'a' dk(0) dk(0) 'b' > 'success'
 */
const DEADKEY_TEST_3: TestSpec = {
  id: 3,
  // Match condition for rule
  rule: [{t:'d', d: 0}, 'a', {t:'d', d: 0}, {t:'d', d: 0}, 'b'],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: [0, 'a', 0, 0, 'b'],
  baseSequence: { 'output': 'ab', 'inputs': [
    { 'type': 'key', 'keyCode': 49, 'states': 10752, 'modifiers': 0, 'modifierChanged': false, 'isVirtualKey': true }, //1
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'ab', 'inputs': [
      // Omission of the first deadkey should result in failure.
      //{'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 3:  required prepended deadkey omission did not prevent a rule match.'
  }, {
    sequence: { 'output': 'ab', 'inputs': [
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      // Omission of the one of the duplicated deadkeys should result in failure.
      //{'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //b
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 3:  omitting one copy of a required duplicated deadkey did not prevent a rule match.'
  }, {
    sequence: { 'output': 'ab', 'inputs': [
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      // Triplifying deadkeys in the center should result in failure.
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //b
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 3:  triple matching deadkeys where only two required did not prevent a rule match.'
  }, {
    sequence: { 'output': 'ab', 'inputs': [
      // Should not fail with a duplicate deadkey prepended to the context.
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: 'Rule 3:  duplicate prepended deadkey prevented a rule match when only one was required.'
  }, {
    sequence: { 'output': 'ab', 'inputs': [
      // Should not fail with an unrelated deadkey prepended to the context before the required one.
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 3:  prepended deadkey placed before rule's deadkey prevented a rule match."
  }, {
    sequence: { 'output': 'ab', 'inputs': [
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      // Should fail when unrelated, prepended deadkey actually comes after the required one.
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 3:  prepended deadkey after rule's required deadkey failed to prevent a rule match."
  }]
};

/* Keyman language equivalent:
 *
 * 'a' dk(0) dk(0) 'b' dk(0) > 'success'
 */
const DEADKEY_TEST_4: TestSpec = {
  id: 4,
  // Match condition for rule
  rule: ['a', {t:'d', d: 0}, {t:'d', d: 0}, 'b', {t:'d', d: 0}],
  // Start of context relative to cursor
  n: 6,
  ln: 5,
  // Resulting context map
  contextCache: ['a', 0, 0, 'b', 0],

  baseSequence: { 'output': 'abc', 'inputs': [
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    // The test has an extra character appended that's not part of the check.
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //c
  ] as RecordedPhysicalKeystroke[] }
  // No specialized fullMatchDefs here, as any appended deadkeys are automatically 'in context' for rules.
};

/* Keyman language equivalent:
 *
 * 'a' 'b' 'b' 'a' > 'success'
 */
const DEADKEY_TEST_5: TestSpec = {
  id: 5,
  // Match condition for rule
  rule: ['a', 'b', 'b', 'a'],
  // Start of context relative to cursor
  n: 5,
  ln: 4,
  // Resulting context map
  contextCache: ['a', 'b', 'b', 'a'],

  baseSequence: { 'output': 'abbac', 'inputs': [
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    // The test has an extra character appended that's not part of the check.
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //c
  ] as RecordedPhysicalKeystroke[] }
};

/* Keyman language equivalent:
 *
 * dk(1) dk(2) dk(0) dk(1) dk(2) > 'success'
 */
const DEADKEY_TEST_6: TestSpec = {
  id: 6,
  // Match condition for rule
  rule: [{t:'d', d: 1}, {t:'d', d: 2}, {t:'d', d: 0}, {t:'d', d: 1}, {t:'d', d: 2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: [1, 2, 0, 1, 2],

  baseSequence: { 'output': '', 'inputs': [
    // Testing with an extra deadkey at the start.
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
    {'type':'key','keyCode':51,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //3
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
    {'type':'key','keyCode':51,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //3
  ] as RecordedPhysicalKeystroke[] }
};

/* Keyman language equivalent:
 *
 * 'c' 'a' 'b' context(3) context(2) > 'success'
 */
const ANY_CONTEXT_TEST_1: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: ['c', 'a', 'b', {t:'c', c:3}, {t:'c', c:2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { 'output': 'cabba', 'inputs': [
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'cacca', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 1: Plain text mismatch with successful context() statements is matching the rule.'
  }, {
    sequence: { 'output': 'cabaa', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 1: mismatched context() rule component is not failing the rule.'
  }]
};

/* Keyman language equivalent:
 *
 * store(bc) 'bc'
 * 'c' 'a' any(bc) context(3) 'a' > 'success'
 */
const ANY_CONTEXT_TEST_2: TestSpec = {
  id: 2,
  // Match condition for rule
  rule: ['c', 'a', {t:'a', a: 'bc', n: 0}, {t:'c', c:3}, 'a'],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { 'output': 'cabba', 'inputs': [
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'cacca', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 2: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { 'output': 'cabca', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
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
const ANY_CONTEXT_TEST_3: TestSpec = {
  id: 3,
  // Match condition for rule
  rule: ['c', { t: 'a', a: 'ac', n: 0 }, { t: 'a', a: 'bc', n: 0 }, {t:'c', c:3}, {t:'c', c:2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { 'output': 'cabba', 'inputs': [
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'cacca', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 3: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { 'output': 'ccccc', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //c
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 3: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { 'output': 'cabab', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
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
const ANY_INDEX_TEST_1: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: ['c', 'a', { t: 'a', a: 'bc', n: 0 }, {t:'i', i:'bc', o:3}, 'a'],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { 'output': 'cabba', 'inputs': [
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'cacca', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 1: Alternate 'any' store character failed to match the rule."
  }, {
    sequence: { 'output': 'cabca', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
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
const ANY_INDEX_TEST_2: TestSpec = {
  id: 2,
  // Match condition for rule
  rule: ['c', { t: 'a', a: 'ab', n: 0 }, {t:'i', i:'bc', o:2}, {t:'i', i:'bc', o:2}, {t:'i', i:'ab', o:2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { 'output': 'cabba', 'inputs': [
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'cacca', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 2: Mismatch with secondary output store did not fail the rule."
  }, {
    sequence: { 'output': 'cbccb', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
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
const ANY_INDEX_TEST_3: TestSpec = {
  id: 3,
  // Match condition for rule
  rule: ['c', { t: 'a', a: 'ab', n: 0 }, { t: 'a', a: 'bc', n: 0 }, {t:'i', i:'bc', o:3}, {t:'i', i:'ab', o:2}],
  // Start of context relative to cursor
  n: 5,
  ln: 5,
  // Resulting context map
  contextCache: ['c', 'a', 'b', 'b', 'a'],

  baseSequence: { 'output': 'cabba', 'inputs': [
    {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'cacca', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 3a: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { 'output': 'cbccb', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 3b: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { 'output': 'cbbbb', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 3c: Error with index() when a rule has multiple any() checks."
  }, {
    sequence: { 'output': 'cbccc', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //c
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //c
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 3: index() rule check matched the incorrect any()."
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) dk(1) dk(2)
 * any(match) > 'success'
 */
const DEADKEY_STORE_TEST_1: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},{d:1},{d:2}]  as KeyboardStoreElement[], n:0}],
  // Start of context relative to cursor
  n: 1,
  ln: 1,
  // Resulting context map
  contextCache: [0],

  baseSequence: { 'output': '', 'inputs': [
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //1
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': '', 'inputs': [
      {'type':'key','keyCode':51,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //3
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 1: Alternate 'any' store deadkey character failed to match the rule."
  }, {
    sequence: { 'output': 'c', 'inputs': [
      {'type':'key','keyCode':67,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //c
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 1: standard character mysteriously matched a deadkey from a store."
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) 'b' dk(2)
 * any(match) > 'success'
 */
const DEADKEY_STORE_TEST_2: TestSpec = {
  id: 2,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},'b',{d:2}] as KeyboardStoreElement[], n:0}],
  // Start of context relative to cursor
  n: 1,
  ln: 1,
  // Resulting context map
  contextCache: [0],

  baseSequence: { 'output': '', 'inputs': [
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //1
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': '', 'inputs': [
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //2
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 2: deadkey not in store mysteriously matched within an any(store) op."
  }, {
    sequence: { 'output': 'b', 'inputs': [
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //b
    ] as RecordedPhysicalKeystroke[] },
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
const DEADKEY_STORE_TEST_3: TestSpec = {
  id: 3,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},{d:1},{d:2}] as KeyboardStoreElement[], n:0},{t:'i',i:'abc', o:1}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [0,'a'],

  baseSequence: { 'output': 'a', 'inputs': [
    {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'a', 'inputs': [
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 3a: index in deadkey store not properly tracked by indexOutput."
  }, {
    sequence: { 'output': 'b', 'inputs': [
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 3a: index in deadkey store not properly tracked by indexOutput."
  }, {
    sequence: { 'output': 'a', 'inputs': [
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //2
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 3: incorrectly matched rule when deadkey and character were incorrectly ordered."
  }, {
    sequence: { 'output': 'bb', 'inputs': [
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 3: triggered when rule is one regular character out of context."
  }, {
    sequence: { 'output': 'b', 'inputs': [
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //2
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 3: triggered when rule is one deadkey out of context."
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) dk(1) dk(2)
 * any(match) context(1) > 'success'
 */
const DEADKEY_STORE_TEST_4: TestSpec = {
  id: 4,
  // Match condition for rule
  rule: [{t:'a',a:[{d:0},{d:1},{d:2}] as KeyboardStoreElement[], n:0},{t:'c',c:1}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [1,1],

  baseSequence: { 'output': '', 'inputs': [
    {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
    {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //2
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': '', 'inputs': [
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //2
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //1
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 4: context matched to alternate (not selected) any(store) option erroneously.'
  }, {
    sequence: { 'output': 'b', 'inputs': [
      {'type':'key','keyCode':51,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //3
      {'type':'key','keyCode':51,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //3
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //b
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 4: triggered when rule is slightly out of context."
  }]
};

/* Keyman language equivalent:
 *
 * nul > 'success'
 */
const NUL_TEST_1: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'n'}],
  // Start of context relative to cursor
  n: 1,
  ln: 1,
  // Resulting context map
  contextCache: [ NUL ],

  baseSequence: { 'output': '', 'inputs': [] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: []
};

/* Keyman language equivalent:
 *
 * nul any(abc) > 'success'
 */
const NUL_TEST_2: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'n'}, {t: 'a', a: ['a', 'b', 'c'], n: 0}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [ NUL, 'a' ],

  baseSequence: { 'output': 'a', 'inputs': [
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: []
};

/* Keyman language equivalent:
 *
 * nul nul any(abc) context(3) > 'success'
 *
 * This one may... 'stretch' what's actually allowed by Keyman language rules,
 * but we wish to ensure that the actual context management is capable of
 * handling this.
 */
const NUL_TEST_3: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'n'}, {t: 'n'}, {t: 'a', a: ['a', 'b', 'c'], n: 0}, {t: 'c', c: 3}],
  // Start of context relative to cursor
  n: 4,
  ln: 4,
  // Resulting context map
  contextCache: [ NUL, NUL, 'a', 'a' ],

  baseSequence: { 'output': 'a', 'inputs': [
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [
    {
      sequence: { 'output': 'a', 'inputs': [
        {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
        {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
        {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //a
      ] as RecordedPhysicalKeystroke[] },
      result: false,
      msg: "Rule 2: matched deadkey with nul"
    }, {
      sequence: { 'output': 'a', 'inputs': [
        {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
        {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
        {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //a
      ] as RecordedPhysicalKeystroke[] },
      result: false,
      msg: "Rule 2: matched character with nul"
    }
  ]
};

/* Keyman language equivalent:
 *
 * nul nul dk(1) any(abc) > 'success'
 *
 * This may also 'stretch' what's actually allowed by Keyman language rules,
 * but we wish to ensure that the actual context management is capable of
 * handling this.
 */
const NUL_TEST_4: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'n'}, {t: 'n'}, {t: 'd', d: 1}, {t: 'a', a: ['a', 'b', 'c'], n: 0}],
  // Start of context relative to cursor
  n: 4,
  ln: 4,
  // Resulting context map
  contextCache: [ NUL, NUL, 1, 'a' ],

  baseSequence: { 'output': 'a', 'inputs': [
    {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [
    {
      sequence: { 'output': 'a', 'inputs': [
        {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
        {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
        {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true} //a
      ] as RecordedPhysicalKeystroke[] },
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
const NOTANY_NUL_TEST_1: TestSpec = {
  id: 1,
  // Match condition for rule
  rule: [{t: 'a', a:['a','b','c'], n: 1},{t:'a',a:['a', 'b', 'c'], n: 0}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [1, 'a'],

  baseSequence: { 'output': 'a', 'inputs': [
    {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'aa', 'inputs': [
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 1: did not fail notany on matched character"
  }, {
    sequence: { 'output': 'a', 'inputs': [
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 1: did not fail notany on nul context'
  }]
};

/* Keyman language equivalent:
 *
 * store(match) dk(0) dk(1) dk(2)
 * store(abc) 'abc'
 *
 * notany(match) any(abc) > 'success'
 */
const NOTANY_NUL_TEST_2: TestSpec = {
  id: 2,
  // Match condition for rule
  rule: [{t: 'a', a:[{d:0},{d:1},{d:2}] as KeyboardStoreElement[], n: 1},{t:'a',a:['a', 'b', 'c'], n: 0}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: ['a', 'a'],

  baseSequence: { 'output': 'aa', 'inputs': [
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'a', 'inputs': [
      {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 2: did not fail notany on matched deadkey"
  }, {
    sequence: { 'output': 'a', 'inputs': [
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 2: did not fail notany on nul context'
  }]
};

/* Keyman language equivalent:
 *
 * store(first) dk(0) 'b' dk(2)
 * store(second) 'a' dk(1) 'c'
 *
 * notany(first) any(second) > 'success'
 */
const NOTANY_NUL_TEST_3: TestSpec = {
  id: 3,
  // Match condition for rule
  rule: [{t: 'a', a:[{d:0},'b',{d:2}] as KeyboardStoreElement[], n: 1},{t:'a',a:['a', 'b', 'c'], n: 0}],
  // Start of context relative to cursor
  n: 2,
  ln: 2,
  // Resulting context map
  contextCache: [1, 'a'],

  baseSequence: { 'output': 'a', 'inputs': [
    {'type':'key','keyCode':50,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //1
    {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
  ] as RecordedPhysicalKeystroke[] },
  fullMatchDefs: [{
    sequence: { 'output': 'a', 'inputs': [
      {'type':'key','keyCode':49,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //0
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 3: did not properly match a deadkey within a mixed notany store"
  }, {
    sequence: { 'output': 'ba', 'inputs': [
      {'type':'key','keyCode':66,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //b
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: "Rule 3: did not properly match a character within a mixed notany store"
  }, {
    sequence: { 'output': 'aa', 'inputs': [
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}, //a
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: true,
    msg: "Rule 3: context incorrectly matched a character within a mixed notany store"
  }, {
    sequence: { 'output': 'a', 'inputs': [
      {'type':'key','keyCode':65,'states':10752,'modifiers':0,'modifierChanged':false,'isVirtualKey':true}  //a
    ] as RecordedPhysicalKeystroke[] },
    result: false,
    msg: 'Rule 3: did not fail notany on nul context'
  }]
};

const DEADKEY_RULE_SET = [ DEADKEY_TEST_1, DEADKEY_TEST_2, DEADKEY_TEST_3, DEADKEY_TEST_4,
  DEADKEY_TEST_5, DEADKEY_TEST_6
];
const ANY_CONTEXT_RULE_SET = [ ANY_CONTEXT_TEST_1, ANY_CONTEXT_TEST_2, ANY_CONTEXT_TEST_3 ];
const ANY_INDEX_RULE_SET = [ ANY_INDEX_TEST_1, ANY_INDEX_TEST_2, ANY_INDEX_TEST_3 ];
const DEADKEY_STORE_RULE_SET = [ DEADKEY_STORE_TEST_1, DEADKEY_STORE_TEST_2, DEADKEY_STORE_TEST_3,
   DEADKEY_STORE_TEST_4 ];

const NUL_RULE_SET = [ NUL_TEST_1, NUL_TEST_2, NUL_TEST_3, NUL_TEST_4 ];

const NOTANY_NUL_RULE_SET = [ NOTANY_NUL_TEST_1, NOTANY_NUL_TEST_2, NOTANY_NUL_TEST_3 ];

const FULL_RULE_SET = [DEADKEY_RULE_SET, ANY_CONTEXT_RULE_SET, ANY_INDEX_RULE_SET,
   DEADKEY_STORE_RULE_SET, NUL_RULE_SET, NOTANY_NUL_RULE_SET].flat();

  // -----------

//#endregion

describe('Engine - Context Matching', function() {
  before(async function() {
    const keyboardLoader = new NodeKeyboardLoader(new JSKeyboardInterface({}, MinimalKeymanGlobal, new VariableStoreTestSerializer()));
    const keyboard = await keyboardLoader.loadKeyboardFromPath(require.resolve('@keymanapp/common-test-resources/keyboards/test_simple_deadkeys.js'));
    keyboardWithHarness = keyboardLoader.harness as JSKeyboardInterface;
    keyboardWithHarness.activeKeyboard = keyboard as JSKeyboard;
  });

  // Tests 'stage 1' of fullContextMatch - ensuring that a proper context index map is built.
  it('properly generates extended context data needed by context-matching checks below', function() {
    const matchDefs = FULL_RULE_SET;

    for(const ruleDef of matchDefs) {
      // Prepare the context!
      const ruleSeq = new RecordedKeystrokeSequence(ruleDef.baseSequence as RecordedKeystrokeSequence);
      const proctor = new NodeProctor(keyboardWithHarness, device, assert.equal);

      // We want to specify the TextStore for this test; our actual concern is the resulting context.
      const textStore = new SyntheticTextStore();
      ruleSeq.test(proctor, textStore);

      const processorInitOptions = { ...DEFAULT_PROCESSOR_INIT_OPTIONS };
      processorInitOptions.keyboardInterface = keyboardWithHarness;

      // Now for the real test!
      const processor = new JSKeyboardProcessor(device, processorInitOptions);
      const res = processor.keyboardInterface['_BuildExtendedContext'](ruleDef.n, ruleDef.ln, textStore);

      assert.sameOrderedMembers(res.valContext, ruleDef.contextCache);
    }
  });

  describe('handles simple deadkey contexts', function() {
    it('for the most basic cases:  DEADKEY_TEST_1', function() {
      runEngineRuleSet([DEADKEY_TEST_1], 'Deadkeys');
    });

    it('with deadkey ordering:  DEADKEY_TEST_2', function() {
      runEngineRuleSet([DEADKEY_TEST_2], 'Deadkeys');
    });

    it('with repeated deadkeys:  DEADKEY_TEST_3', function() {
      runEngineRuleSet([DEADKEY_TEST_3], 'Deadkeys');
    });

    it('with caret after deadkey:  DEADKEY_TEST_4', function() {
      runEngineRuleSet([DEADKEY_TEST_4], 'Deadkeys');
    });

    it('with no deadkeys, mid-text:  DEADKEY_TEST_5', function() {
      runEngineRuleSet([DEADKEY_TEST_5], 'Deadkeys');
    });

    it('with long sequence of only deadkeys:  DEADKEY_TEST_6', function() {
      runEngineRuleSet([DEADKEY_TEST_6], 'Deadkeys');
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