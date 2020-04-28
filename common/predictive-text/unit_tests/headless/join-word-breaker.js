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

    // This was my guiding test case:
    [
      "-yâhk ê-nitawi-kotiskâwêyâhk ni-",
      ["-"],
      ["-", "yâhk", "ê", "-", "nitawi", "-", "kotiskâwêyâhk", "ni", "-"],
      ["-yâhk", "ê-nitawi-kotiskâwêyâhk", "ni-"]
    ],

    // Do not perform any joins:
    ["hello world", "-", ["hello", "world"], ["hello", "world"]],

    // Joining using multiple joiners
    [
      "Email: no-body@example.com",
      ["@", "-"],
      ["Email", ":", "no", "-", "body", "@", "example.com"],
      ["Email", ":", "no-body@example.com"]
    ],
    
    // Joining with two or more joiners in a row 
    [
      "nobody@@example.com",
      ["@"],
      ["nobody", "@", "@", "example.com"],
      ["nobody@@example.com"]
    ],

    // it should NOT join non-contiguous spans:
    [
      "this- is -bad",
      ["-"],
      ["this", "-", "is", "-", "bad"],
      ["this-", "is", "-bad"]
    ],

    // different but adjacent joiners
    [
      "kawé:-conjugator",
      ["-", ":"],
      ["kawé", ":", "-", "conjugator"],
      ["kawé:-conjugator"],
    ],
  ]

  for (let [phrase, joiners, unjoined, expected] of TEST_CASES) {
    it(`should break «${[phrase]}» as [${expected.join(' ;; ')}]`, function () {
      let breakWords = decorateWithJoin(defaultWordBreaker, joiners);
      let unjoinedResult = defaultWordBreaker(phrase).map(onlyText);
      assert.deepEqual(unjoinedResult, unjoined);
      let actualResult = breakWords(phrase).map(onlyText);
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