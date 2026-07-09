/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import 'mocha';
import { assert } from 'chai';
import { ModifierKeyConstant, modifierStringToState } from '../../src/consts/modifier-key-constants.js';

describe('modifierStringToState', function() {
  it('should map modifiers correctly', function() {
    assert.equal(modifierStringToState(''), 0);
    assert.equal(modifierStringToState('shift'), ModifierKeyConstant.K_SHIFTFLAG);
    assert.equal(modifierStringToState('ctrl'), ModifierKeyConstant.K_CTRLFLAG);
    assert.equal(modifierStringToState('rightctrl'), ModifierKeyConstant.RCTRLFLAG);
    assert.equal(modifierStringToState('ctrl-shift'), ModifierKeyConstant.K_CTRLFLAG | ModifierKeyConstant.K_SHIFTFLAG);
    assert.equal(modifierStringToState('ctrl-shift-alt'), ModifierKeyConstant.K_CTRLFLAG | ModifierKeyConstant.K_SHIFTFLAG | ModifierKeyConstant.K_ALTFLAG);
    assert.equal(modifierStringToState('ctrl-shift-alt-caps'), ModifierKeyConstant.K_CTRLFLAG | ModifierKeyConstant.K_SHIFTFLAG | ModifierKeyConstant.K_ALTFLAG | ModifierKeyConstant.CAPITALFLAG);
    assert.equal(modifierStringToState('caps-alt-ctrl-shift'), ModifierKeyConstant.K_CTRLFLAG | ModifierKeyConstant.K_SHIFTFLAG | ModifierKeyConstant.K_ALTFLAG | ModifierKeyConstant.CAPITALFLAG);
  });

  it('should map legacy modifiers correctly', function() {
    assert.equal(modifierStringToState('altshift'), ModifierKeyConstant.K_ALTFLAG | ModifierKeyConstant.K_SHIFTFLAG);
    assert.equal(modifierStringToState('ctrlaltshift'), ModifierKeyConstant.K_CTRLFLAG | ModifierKeyConstant.K_SHIFTFLAG | ModifierKeyConstant.K_ALTFLAG);
  });

  it('should ignore unknown modifiers', function() {
    assert.equal(modifierStringToState('ctrl-shift-none'), ModifierKeyConstant.K_CTRLFLAG | ModifierKeyConstant.K_SHIFTFLAG);
    assert.equal(modifierStringToState('ctrlraltshift'), 0);
    assert.equal(modifierStringToState('ncaps'), 0);
  });
});
