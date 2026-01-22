/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import 'mocha';
import { AnalyzeOskCharacterUse } from '../src/osk-character-use/index.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

describe('AnalyzeOskCharacterUse output formats', function() {
  const callbacks = new TestCompilerCallbacks();

  const dummyStrings = {
    'a': [{ filename: 'file1.kvks', count: 1 }],
    'b': [{ filename: 'file2.kvks', count: 2 }]
  };

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if (this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('generates .txt format correctly', function() {
    const a = new AnalyzeOskCharacterUse(callbacks, { includeCounts: true });
    a.unitTestEndPoints.addStrings(['a', 'b'], 'testfile.kvks');
    const txt = a.getStrings('.txt');
    assert.isTrue(txt.some(line => line.includes('U+')));
    assert.match(txt[0], /^U\+[A-F0-9]{4}/);
  });

  it('generates .md format correctly', function() {
    const a = new AnalyzeOskCharacterUse(callbacks, { includeCounts: true });
    (a as any)._strings = dummyStrings;
    const md = a.getStrings('.md');
    // test header
    assert.match(md[0], /^PUA\s+\|\s+Code Points\s+\|\s+Key Caps$/);
    // test data
    assert.match(md[2], /^U\+[A-F0-9]{4}\s+\|\s+U\+[A-F0-9]{4}\s+\|\s+\S/);
  });

  it('generates .json format correctly', function() {
    const a = new AnalyzeOskCharacterUse(callbacks, { includeCounts: true });
    (a as any)._strings = dummyStrings;
    const json = a.getStrings('.json').join('\n');
    const parsed = JSON.parse(json);
    assert.isArray(parsed.map);
    assert.lengthOf(parsed.map, 2);
    assert.equal(parsed.map[0].usages[0].filename, 'file1.kvks');
    assert.equal(parsed.map[1].usages[0].filename, 'file2.kvks');
  });

  describe('unitTestEndPoints', function() {
    it('converts BMP strings (U+0000–U+FFFF) correctly', function() {
      const a = new AnalyzeOskCharacterUse(callbacks);
      const seq = a.unitTestEndPoints.stringToUnicodeSequence('ab');
      assert.equal(seq, 'U+0061 U+0062');
    });

    it('converts supplementary characters (U+10000–U+10FFFF) correctly', function() {
      const a = new AnalyzeOskCharacterUse(callbacks);
      // 😀 (U+1F600) and 🦊 (U+1F98A)
      const seq = a.unitTestEndPoints.stringToUnicodeSequence('😀🦊');
      assert.equal(seq, 'U+1F600 U+1F98A');
    });
  });
});