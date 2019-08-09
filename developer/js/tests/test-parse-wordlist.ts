import {parseWordList} from '../lexical-model-compiler/build-trie';
import {assert} from 'chai';
import 'mocha';

import path = require('path');

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
});

