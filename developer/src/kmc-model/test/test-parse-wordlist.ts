import {parseWordListFromContents, parseWordListFromFilename, WordList} from '../src/build-trie.js';
import {assert} from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers/index.js';
import { ModelCompilerMessages } from '../src/model-compiler-messages.js';
import { setCompilerCallbacks } from '../src/compiler-callbacks.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

const BOM = '\ufeff';
const SENCOTEN_WORDLIST = {
  'TŦE': 13644,
  'E': 9134,
  'SEN': 4816,
  'Ȼ': 3479,
  'SW̱': 2621,
  'NIȽ': 2314,
  'U¸': 2298,
  'I¸': 1988,
  'ȻSE': 1925,
  'I': 1884
};


describe('parsing a word list', function () {
  let testCallbacks = new TestCompilerCallbacks();

  beforeEach(function () {
    testCallbacks.clear();
    setCompilerCallbacks(testCallbacks);
  });

  afterEach(function () {
    setCompilerCallbacks(null);
  });

  it('should remove the UTF-8 byte order mark from files', function () {
    let word = 'hello';
    let count = 1;
    let expected: WordList = {};
    expected[word] = count;

    let file = `# this is a comment\n${word}\t${count}`;
    let withoutBOM: WordList = {};
    parseWordListFromContents(withoutBOM, file);
    assert.deepEqual(withoutBOM, expected, "expected regular file to parse properly");
    assert.isEmpty(testCallbacks.messages);

    let withBOM: WordList = {};
    parseWordListFromContents(withBOM, `${BOM}${file}`)
    assert.deepEqual(withBOM, expected, "expected BOM to be ignored");
    assert.isEmpty(testCallbacks.messages);
  });

  it('should read word lists in UTF-8', function () {
    // N.B.: this is the format exported by Google Drive when selecting "TSV".
    const filename = makePathToFixture('example.qaa.sencoten', 'wordlist.tsv');
    let wordlist: WordList = {};
    parseWordListFromFilename(wordlist, filename);

    assert.deepEqual(wordlist, SENCOTEN_WORDLIST);
    assert.isEmpty(testCallbacks.messages);
  });

  it('should read word lists in UTF-16 little-endian (with BOM)', function () {
    // N.B.: this is the format exported by MS Excel when selecting
    // "UTF-16" text (tested on Excel for macOS).
    const filename = makePathToFixture('example.qaa.utf16le', 'wordlist.txt');
    let wordlist: WordList = {};
    parseWordListFromFilename(wordlist, filename);

    assert.deepEqual(wordlist, SENCOTEN_WORDLIST);
    assert.isEmpty(testCallbacks.messages);
  });

  it('should NOT read word lists in UTF-16 big-endian (with BOM)', function () {
    // N.B.: Does anything output this format...?
    const filename = makePathToFixture('example.qaa.utf16be', 'wordlist.txt');
    let wordlist: WordList = {};
    assert.throws(() => {
      parseWordListFromFilename(wordlist, filename);
    }, 'UTF-16BE is unsupported');
  });

  it('should merge duplicate entries in a wordlist', function () {
    // Tests that we merge NFC+NFD entries and identical entries, trimming whitespace
    // Note building the wordlist from an array to make clear that we have unnormalised inputs
    const words = [
      'hello',       //1
      'hello\u0301', //2, NFD helló
      'hell\u00f3',  //3, NFC helló
      ' hello ',     //4, expect to trim whitespace
      'hello'];      //5

    const expected: WordList = {
        'hello': 10,     /* 1+4+5 trimmed and identical */
        'hell\u00f3': 5, /* 2+3 normalised to NFC */
    };

    // Build a wordlist from the array
    let file = `# this is a comment\n`;
    for(let i = 0; i < words.length; i++) {
      file += `${words[i]}\t${i+1}\n`;
    }
    let repeatedWords: WordList = {};
    parseWordListFromContents(repeatedWords, file);

    assert.deepEqual(repeatedWords, expected);

    assert.lengthOf(testCallbacks.messages, 4);
    // hello has been seen multiple times:
    assert.isTrue(testCallbacks.hasMessage(ModelCompilerMessages.HINT_DuplicateWordInSameFile));
    // helló and hello + U+0301 have both been seen:
    assert.isTrue(testCallbacks.hasMessage(ModelCompilerMessages.HINT_MixedNormalizationForms));

    // Let's parse another file:

    testCallbacks.clear();
    // Now, parse a DIFFERENT file, but with an NFD entry.
    parseWordListFromContents(repeatedWords, "hello\u0301\t5\n");
    assert.lengthOf(testCallbacks.messages, 1);
    // hello + U+0301 (NFD) has been seen, but...
    assert.isTrue(testCallbacks.hasMessage(ModelCompilerMessages.HINT_MixedNormalizationForms));
    // BUT! We have not seen a duplicate **within the same file**
    assert.isFalse(testCallbacks.hasMessage(ModelCompilerMessages.HINT_DuplicateWordInSameFile));

    assert.deepEqual(repeatedWords, {
      hello: expected['hello'],
      // should have seen more of this entry:
      "hell\u00f3": expected["hell\u00f3"] + 5,
    });
  });
});
