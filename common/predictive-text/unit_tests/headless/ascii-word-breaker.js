var assert = require('chai').assert;
var sinon = require('sinon');

let LMLayer = require('../../build');
var breakASCIIWords = require('../../build/intermediate').wordBreakers['ascii'];

describe('The ASCIIWordBreaker', function() {
    it('breaks simple English sentences', function () {
        let breaks = breakASCIIWords('Look! -- The quick brown fox jumps... over the lazy dog!');
        let words = breaks.map(span => span.text);
        assert.deepEqual(words, ['Look', 'The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog']);
    });
});