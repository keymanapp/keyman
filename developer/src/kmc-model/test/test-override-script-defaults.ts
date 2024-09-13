import { assert } from "chai";
import defaultWordBreaker from '@keymanapp/models-wordbreakers';
import {decorateWithScriptOverrides} from '../src/script-overrides-decorator.js';
import { Span } from '@keymanapp/common-types';

const THIN_SPACE = "\u2009";

describe('The script overrides word breaker decorator', function () {
  it('should decorate an existing word breaker', function () {
    let breakWords = decorateWithScriptOverrides(defaultWordBreaker, 'break-words-at-spaces')
    assert.isFunction(breakWords);
  });

  // I do not read this script or language so I have NO idea what this says
  // ¯\_(ツ)_/¯
  const phraseComponents = ["ຈາກ", THIN_SPACE, "ກ໌ນິ", THIN_SPACE, "ສນາ", THIN_SPACE, "ເກ໌າະ", THIN_SPACE, "ຢັອຫ", THIN_SPACE, "ລະ", THIN_SPACE, "ບຣອມ", THIN_SPACE, "ເຢາະ", ","];
  const phraseSpans = phraseComponents.filter(span => span !== THIN_SPACE);
  const phrase = phraseComponents.join("");
  const expectedNumSpans = phraseSpans.length;

  it(`should break «${[phrase]}» as ${expectedNumSpans} spans`, function () {
    let breakWords = decorateWithScriptOverrides(defaultWordBreaker, 'break-words-at-spaces');
    let defaultResult = defaultWordBreaker(phrase);

    assert.isAbove(defaultResult.length, expectedNumSpans);
    let actualResult = breakWords(phrase);
    assert.lengthOf(actualResult, expectedNumSpans);
    assert.deepEqual(actualResult.map(grabText), phraseSpans);
  });

  function grabText(span: Span) {
    return span.text;
  }
});
