var assert = require('chai').assert;

const defaultWordBreaker = require('@keymanapp/models-wordbreakers').wordBreakers['default'];
import {decorateWithScriptOverrides} from '../dist/lexical-model-compiler/script-overrides-decorator';


describe('The join word breaker decorator', function () {
  it('should decorate an existing word breaker', function () {
    let breakWords = decorateWithScriptOverrides(defaultWordBreaker, 'break-words-at-spaces')
    assert.isFunction(breakWords);
  });

  // I do not read this script or language so I have NO idea what this says
  // ¯\_(ツ)_/¯
  const phrase = "ຈາກ ກ໌ນິ ສນາ ເກ໌າະ ຢັອຫ ລະ ບຣອມ ເຢາະ,";
  const expectedNumSpans = 9;

  it(`should break «${[phrase]}» as ${expectedNumSpans} words`, function () {
    let breakWords = decorateWithScriptOverrides(defaultWordBreaker, 'break-words-at-spaces');
    let defaultResult = defaultWordBreaker(phrase);

    assert.isAbove(defaultResult.length, expectedNumSpans);
    let actualResult = breakWords(phrase);
    assert.lengthOf(actualResult, expectedNumSpans);
  });
});
