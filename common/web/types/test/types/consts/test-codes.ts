import 'mocha';
import { assert } from 'chai';
import { Codes } from '../../../src/consts/codes.js';

describe('Codes', function () {
  describe('test isFrameKey()', function () {
    it('should properly identify frame keys', () => {
      [
        'K_SHIFT',
        'K_LOPT',
        'K_ROPT',
        'K_NUMLOCK',
        'K_CAPS',
        'K_NUMERALS', // not explicitly listed, but included because > 50000
      ].forEach(keyId => assert.isTrue(Codes.isFrameKey(keyId), `isFrameKey(${keyId})`));

      [
        'K_ESC',
        'K_OE2',
        'K_ALT',  // not currently included in isFrameKey
        'K_CTRL', // not currently included in isFrameKey
      ].forEach(keyId => assert.isFalse(Codes.isFrameKey(keyId), `!isFrameKey(${keyId})`));
    });
  });

  describe('test getModifierState()', function () {
    it('should properly identify modifiers from layer id', () => {
      [
        { layerId: 'shift', expected: Codes.modifierCodes['SHIFT'] },
        { layerId: 'leftshift', expected: Codes.modifierCodes['SHIFT'] },
        { layerId: 'rightshift', expected: Codes.modifierCodes['SHIFT'] },
        { layerId: 'ctrlshift', expected: Codes.modifierCodes['CTRL'] | Codes.modifierCodes['SHIFT'] },
        { layerId: 'shiftctrl', expected: Codes.modifierCodes['CTRL'] | Codes.modifierCodes['SHIFT'] },
        { layerId: 'fooshift', expected: Codes.modifierCodes['SHIFT'] },

        { layerId: 'leftctrl', expected: Codes.modifierCodes['LCTRL'] },
        { layerId: 'rightctrl', expected: Codes.modifierCodes['RCTRL'] },
        { layerId: 'ctrl', expected: Codes.modifierCodes['CTRL'] },
        { layerId: 'right_ctrl', expected: Codes.modifierCodes['CTRL'] },
        { layerId: 'leftctrlrightctrlshift', expected: Codes.modifierCodes['LCTRL'] | Codes.modifierCodes['RCTRL'] | Codes.modifierCodes['SHIFT'] },
        { layerId: 'leftctrl-rightctrl-shift', expected: Codes.modifierCodes['LCTRL'] | Codes.modifierCodes['RCTRL'] | Codes.modifierCodes['SHIFT'] },
        { layerId: 'ctrl-rightctrl-shift', expected: Codes.modifierCodes['RCTRL'] | Codes.modifierCodes['SHIFT'] },

        { layerId: 'leftalt', expected: Codes.modifierCodes['LALT'] },
        { layerId: 'rightalt', expected: Codes.modifierCodes['RALT'] },
        { layerId: 'alt', expected: Codes.modifierCodes['ALT'] },
        { layerId: 'leftaltrightaltshift', expected: Codes.modifierCodes['LALT'] | Codes.modifierCodes['RALT'] | Codes.modifierCodes['SHIFT'] },
        { layerId: 'leftaltrightalt', expected: Codes.modifierCodes['LALT'] | Codes.modifierCodes['RALT'] },
        { layerId: 'shiftleftctrlrightctrlleftaltrightalt', expected: Codes.modifierCodes['LCTRL'] | Codes.modifierCodes['RCTRL'] | Codes.modifierCodes['SHIFT'] | Codes.modifierCodes['LALT'] | Codes.modifierCodes['RALT'] },
        { layerId: 'shiftctrlalt', expected: Codes.modifierCodes['SHIFT'] | Codes.modifierCodes['CTRL'] | Codes.modifierCodes['ALT'] },

        { layerId: 'foo', expected: 0 },

      ].forEach(testdata => assert.equal(Codes.getModifierState(testdata.layerId), testdata.expected, `getModifierState(${testdata.layerId})`));
    });
  });

  describe('test getStateFromLayer()', function () {
    it('should properly identify caps state from layer id', () => {
      [
        { layerId: 'caps', expected: Codes.modifierCodes['CAPS'] },
        { layerId: 'altcaps', expected: Codes.modifierCodes['CAPS'] },
        { layerId: 'capsalt', expected: Codes.modifierCodes['CAPS'] },

        { layerId: 'shift', expected: Codes.modifierCodes['NO_CAPS'] },
        { layerId: 'foo', expected: Codes.modifierCodes['NO_CAPS'] },

      ].forEach(testdata => { assert.equal(Codes.getStateFromLayer(testdata.layerId), testdata.expected, `getStateFromLayer(${testdata.layerId})`); });
    });
  });
});
