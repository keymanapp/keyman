import 'mocha';
import { assert } from 'chai';
import { AllKeymanTargets, KeymanTarget, keymanTargetsFromString } from "../../src/kmx/keyman-targets.js";

describe('keyman-targets', function () {
  it('should parse a string of targets', function() {
    assert.deepEqual(keymanTargetsFromString('any', {expandTargets: false}), [KeymanTarget.any]);
    assert.deepEqual(keymanTargetsFromString('any', {expandTargets: true}), AllKeymanTargets);
    assert.deepEqual(keymanTargetsFromString('windows linux'), [KeymanTarget.windows, KeymanTarget.linux]);
    assert.deepEqual(keymanTargetsFromString('windows    macosx'), [KeymanTarget.windows, KeymanTarget.macosx]);
    assert.deepEqual(keymanTargetsFromString('windows macosx any', {expandTargets: true}), AllKeymanTargets);
    assert.deepEqual(keymanTargetsFromString('desktop mobile', {expandTargets: true}),
      [KeymanTarget.androidphone, KeymanTarget.iphone, KeymanTarget.linux, KeymanTarget.macosx, KeymanTarget.windows]);
    assert.deepEqual(keymanTargetsFromString('tablet windows', {expandTargets: true}),
      [KeymanTarget.windows, KeymanTarget.androidtablet, KeymanTarget.ipad]);
  });
});
