
import 'mocha';
import {assert} from 'chai';
import { defaultWordform2Key } from '../dist/lexical-model-compiler/build-trie';


describe('The default searchTermToKey() function', function () {
  it('should lowercase and THEN normalize', function() {
    // "İstanbul" has a DOTTED-I. This should be lowercased,
    // then have its dot removed! 
    assert.equal(defaultWordform2Key('İstanbul'), 'istanbul');
    assert.equal(defaultWordform2Key('Diyarbakır'), 'diyarbakır');

    // "skýlos" is Greek for dog 🇬🇷🐶
    // starts with an 's' and ends with an 's'
    // which are DIFFERENT CHARACTERS in lowercased Greek!
    assert.equal(defaultWordform2Key('σκύλος'), 'σκυλος');
    assert.equal(defaultWordform2Key('ΣΚΥΛΟΣ'), 'σκυλος');
  });
});