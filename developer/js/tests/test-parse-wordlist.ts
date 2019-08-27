import {parseWordList, parseWordListFromFilename} from '../dist/lexical-model-compiler/build-trie';
import {assert} from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers';

const BOM = '\ufeff';

describe('parseWordList', function () {
  it('should remove the UTF-8 byte order mark from files', function () {
    let word = 'hello';
    let count = 1;
    let expected = [
      [word, count]
    ];
    let file = `# this is a comment\n${word}\t${count}`;
    let withoutBOM = parseWordList(file);
    assert.deepEqual(withoutBOM, expected, "expected regular file to parse properly");
    let withBOM = parseWordList(`${BOM}${file}`)
    assert.deepEqual(withBOM, expected, "expected BOM to be ignored");
  });

  it('should read wordlists in UTF-16 LE (with BOM)', function () {
    // N.B.: this is the format exported by MS Excel when selecting
    // "UTF-16" text.
    const filename = makePathToFixture('example.qaa.utf16le', 'wordlist.txt');
    let wordlist = parseWordListFromFilename(filename);
    assert.lengthOf(wordlist, 10);
  });
});

