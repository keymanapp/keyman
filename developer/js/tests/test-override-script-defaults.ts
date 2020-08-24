var assert = require('chai').assert;

const defaultWordBreaker = require('@keymanapp/models-wordbreakers').wordBreakers['default'];
import {decorateWithScriptOverrides} from '../dist/lexical-model-compiler/script-overrides-decorator';


describe('The join word breaker decorator', function () {
  it('should decorate an existing word breaker', function () {
    let breakWords = decorateWithScriptOverrides(defaultWordBreaker, 'spaces-break-words')
    assert.isFunction(breakWords);
  });

  // I do not read this script or language so I have NO idea what this says
  // ¯\_(ツ)_/¯
  const phrase = "ຈາກ ກ໌ນິ ສນາ ເກ໌າະ ຢັອຫ ລະ ບຣອມ ເຢາະ,";
  const expectedNumWords = 8;

  it(`should break «${[phrase]}» as ${expectedNumWords} words`, function () {
    let breakWords = decorateWithScriptOverrides(defaultWordBreaker, 'spaces-break-words');
    let defaultResult = defaultWordBreaker(phrase);

    assert.isAbove(defaultResult.length, expectedNumWords);
    let actualResult = breakWords(phrase);
    assert.lengthOf(actualResult, expectedNumWords);
  });
});
