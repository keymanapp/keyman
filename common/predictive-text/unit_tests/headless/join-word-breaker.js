var assert = require('chai').assert;

var defaultWordBreaker = require('../../build/intermediate').wordBreakers['default'];
var decorateWithJoin = require('../../build/intermediate').wordBreakers['join_'];

describe('The join word breaker decorator', function () {
  it('should decorate an existing word breaker', function () {
    let breakWords = decorateWithJoin(defaultWordBreaker, ['-']);
    assert.isFunction(breakWords);
  });

  const TEST_CASES = [
    /* input,      joiners,  default breaks,        breaks with joins */
    // Original test case from https://github.com/keymanapp/keyman/issues/2753
    ['khui-chhùi', ['-'],   ["khui", "-", "chhùi"], ["khui-chhùi"]], 

    // Plains Cree SRO: 
    ['ê-kotiskâwêyâhk', ['-'], ['ê', '-', 'kotiskâwêyâhk'], ['ê-kotiskâwêyâhk']],

    // Edge cases:

    // Joiner alone:
    ['-', ['-'], ['-'], ['-']],
    // Joiner at the end: 
    ['ni-', ['-'], ['ni', '-'], ['ni-']],
    // Joiner at the end: 
    ['-ân', ['-'], ['-', 'ân'], ['-ân']],
  ]

  for (let [phrase, joiners, unjoined, expected] of TEST_CASES) {
    it(`should break «${[phrase]}» as [${expected.join(' ;; ')}]`, function () {
      let breakWords = decorateWithJoin(defaultWordBreaker, joiners);
      let unjoinedResult = defaultWordBreaker(phrase).map(onlyText);
      let actualResult = breakWords(phrase).map(onlyText);
      assert.deepEqual(unjoinedResult, unjoined);
      assert.deepEqual(actualResult, expected);
    });
  }

  /**
   * Get just the text from a span.
   * @param {Span} span 
   */
  function onlyText(span) {
    return span.text;
  }
});