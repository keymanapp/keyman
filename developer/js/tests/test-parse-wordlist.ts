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
    let withoutBOM = parseWordList(`${word}\t${count}\n`);
    assert.deepEqual(withoutBOM, expected);
    let withBOM = parseWordList(`${BOM}${word}\t${count}\n`)
    assert.deepEqual(withBOM, expected);
  });
});

