import {parseWordList, parseWordListFromFilename, WordList} from '../dist/lexical-model-compiler/build-trie';
import {assert} from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers';

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

describe('parseWordList', function () {
  it('should remove the UTF-8 byte order mark from files', function () {
    let word = 'hello';
    let count = 1;
    let expected: WordList = {};
    expected[word] = count;

    let file = `# this is a comment\n${word}\t${count}`;
    let withoutBOM: WordList = {};
    parseWordList(withoutBOM, file);
    assert.deepEqual(withoutBOM, expected, "expected regular file to parse properly");
    let withBOM: WordList = {};
    parseWordList(withBOM, `${BOM}${file}`)
    assert.deepEqual(withBOM, expected, "expected BOM to be ignored");
  });

  it('should read word lists in UTF-8', function () {
    // N.B.: this is the format exported by Google Drive when selecting "TSV".
    const filename = makePathToFixture('example.qaa.sencoten', 'wordlist.tsv');
    let wordlist: WordList = {};
    parseWordListFromFilename(wordlist, filename);
    assert.deepEqual(wordlist, SENCOTEN_WORDLIST);
  });

  it('should read word lists in UTF-16 little-endian (with BOM)', function () {
    // N.B.: this is the format exported by MS Excel when selecting
    // "UTF-16" text (tested on Excel for macOS).
    const filename = makePathToFixture('example.qaa.utf16le', 'wordlist.txt');
    let wordlist: WordList = {};
    parseWordListFromFilename(wordlist, filename);
    assert.deepEqual(wordlist, SENCOTEN_WORDLIST);
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
      'hello', //1
      'hello'+String.fromCharCode(0x0301), //2, NFD helló
      'hell'+String.fromCharCode(0x00F3), //3, NFC helló
      ' hello ', //4, expect to trim whitespace
      'hello']; //5

    const expected: WordList = {};
    expected['hello'] = 10; /* 1+4+5 trimmed and identical */
    expected['hell'+String.fromCharCode(0x00F3)] = 5 /* 2+3 normalised to NFC */

    // Build a wordlist from the array
    let file = `# this is a comment\n`;
    for(let i = 0; i < words.length; i++) {
      file += `${words[i]}\t${i+1}\n`;
    }
    let repeatedWords: WordList = {};
    parseWordList(repeatedWords, file);

    assert.deepEqual(repeatedWords, expected);
  });
});

