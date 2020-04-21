
import 'mocha';
import {assert} from 'chai';
import { defaultSearchTermToKey } from '../dist/lexical-model-compiler/build-trie';


describe('The default searchTermToKey() function', function () {
  it('should lowercase and THEN normalize', function() {
    // "İstanbul" has a DOTTED-I. This should be lowercased,
    // then have its dot removed! 
    assert.equal(defaultSearchTermToKey('İstanbul'), 'istanbul');
    assert.equal(defaultSearchTermToKey('Diyarbakır'), 'diyarbakır');

    // "skýlos" is Greek for dog 🇬🇷🐶
    // starts with an 's' and ends with an 's'
    // which are DIFFERENT CHARACTERS in lowercased Greek!
    assert.equal(defaultSearchTermToKey('σκύλος'), 'σκυλος');
    assert.equal(defaultSearchTermToKey('ΣΚΥΛΟΣ'), 'σκυλος');
  });
});