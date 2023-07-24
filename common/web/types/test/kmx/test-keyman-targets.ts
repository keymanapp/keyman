import 'mocha';
import { assert } from 'chai';
import { AllKeymanTargets, KeymanTarget, keymanTargetsFromString } from "../../src/kmx/keyman-targets.js";

describe('keyman-targets', function () {
  it('should parse a string of targets', function() {
    assert.deepEqual(keymanTargetsFromString('any', {expandAny: false}), [KeymanTarget.any]);
    assert.deepEqual(keymanTargetsFromString('any', {expandAny: true}), AllKeymanTargets);
    assert.deepEqual(keymanTargetsFromString('windows linux'), [KeymanTarget.windows, KeymanTarget.linux]);
    assert.deepEqual(keymanTargetsFromString('windows    macosx'), [KeymanTarget.windows, KeymanTarget.macosx]);
    assert.deepEqual(keymanTargetsFromString('windows macosx any', {expandAny: true}), AllKeymanTargets);
  });
});
