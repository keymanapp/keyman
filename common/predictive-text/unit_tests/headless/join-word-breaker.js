var assert = require('chai').assert;

var defaultWordBreaker = require('../../build/intermediate').wordBreakers['default'];
var decorateWithJoin = require('../../build/intermediate').wordBreakers['join_'];

describe('The join word breaker decorator', function () {
  it('should decorate an existing word breaker', function () {
    let breakWords = decorateWithJoin(defaultWordBreaker, ['-']);
    assert.isFunction(breakWords);
  });

  it('should join spans at the given delimiter', function () {
    let phrase = 'khui-chhùi';
    let breakWords = decorateWithJoin(defaultWordBreaker, ['-']);

    let undecoratedResult = defaultWordBreaker(phrase).map(onlyText);
    let actualResult = breakWords(phrase).map(onlyText);
    assert.deepEqual(undecoratedResult, ["khui", "-", "chhùi"]);
    assert.deepEqual(actualResult, ["khui-chhùi"]);
  });

  /**
   * Get just the text from a span.
   * @param {Span} span 
   */
  function onlyText(span) {
    return span.text;
  }
});