/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import 'mocha';
import { AnalyzeOskCharacterUse } from '../src/osk-character-use/index.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

describe('AnalyzeOskCharacterUse output formats', function () {
  const callbacks = new TestCompilerCallbacks();

  const dummyStrings = {
    'a': [{ filename: 'file1.kvks', count: 1 }],
    'b': [{ filename: 'file2.kvks', count: 2 }]
  };

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('generates .txt format correctly', function() {
    const a = new AnalyzeOskCharacterUse(callbacks, { includeCounts: true });
    (a as any)._strings = dummyStrings;
    const txt = a.getStrings('.txt');
    assert.isTrue(txt.some(line => line.includes('U+')));
  });

  it('generates .md format correctly', function() {
    const a = new AnalyzeOskCharacterUse(callbacks, { includeCounts: true });
    (a as any)._strings = dummyStrings;
    const md = a.getStrings('.md');
    assert.isTrue(md[0].includes('PUA') && md[2].includes('|'));
  });

  it('generates .json format correctly', function() {
    const a = new AnalyzeOskCharacterUse(callbacks, { includeCounts: true });
    (a as any)._strings = dummyStrings;
    const json = a.getStrings('.json').join('\n');
    const parsed = JSON.parse(json);
    assert.isArray(parsed.map);
  });

  it('converts strings to Unicode sequences correctly', function() {
    const seq = (AnalyzeOskCharacterUse as any).stringToUnicodeSequence('ab');
    assert.equal(seq, 'U+0061 U+0062');
  });
});