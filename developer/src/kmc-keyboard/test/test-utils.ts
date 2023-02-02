import 'mocha';
import {assert} from 'chai';
import { isValidEnumValue, calculateUniqueKeys, allUsedKeyIdsInLayers, translateLayerAttrToModifier, validModifier } from '../src/util/util.js';
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { LDMLKeyboard } from '@keymanapp/common-types';

describe('test of util/util.ts', () => {
  describe('isValidEnumValue()', () => {
    enum MyFruit {
      apple, peach, pear, plum
    };
    enum MyNumbers {
      wieħed=1, tnejn=2, tlieta=3
    };
    enum MyLetters {
      akka='h', magħtugha='ħ'
    };

    it('should handle a bare enum', ()=> {
      assert.ok(isValidEnumValue(MyFruit, 'apple'));
      assert.notOk(isValidEnumValue(MyFruit, 'banana'));
      assert.notOk(isValidEnumValue(MyFruit, ''));
      assert.notOk(isValidEnumValue(MyFruit, undefined));
      assert.notOk(isValidEnumValue(MyFruit, null));
      assert.notOk(isValidEnumValue(MyFruit, 'unknown'));
    });

    it('should handle a numerical enum', ()=> {
      assert.ok(isValidEnumValue(MyNumbers, 'tlieta'));
      assert.notOk(isValidEnumValue(MyNumbers, '3'));
      assert.notOk(isValidEnumValue(MyNumbers, 'seventy-six'));
      assert.notOk(isValidEnumValue(MyNumbers, '76'));
    });

    it('should handle a string enum', ()=> {
      assert.ok(isValidEnumValue(MyLetters, 'h'));
      assert.ok(isValidEnumValue(MyLetters, 'ħ'));
      assert.notOk(isValidEnumValue(MyLetters, 'akka'));
      assert.notOk(isValidEnumValue(MyLetters, 'z'));
      assert.notOk(isValidEnumValue(MyLetters, 'zed'));
    });
  });

  describe('calculateUniqueKeys()', () => {
    it('should handle some null cases', () => {
      assert.sameDeepMembers(calculateUniqueKeys(null),
        []);
      assert.sameDeepMembers(calculateUniqueKeys([]),
        []);
    });
    it('should handle some real cases', () => {
      assert.sameDeepMembers(calculateUniqueKeys([
        { id: 'a', to: 'a' },
        { id: 'a', to: 'a' }, // dup
        { id: 'b', to: 'b' },
        { id: 'a', to: 'å' }, // override
      ]),[
        { id: 'b', to: 'b' },
        { id: 'a', to: 'å' },
      ]);
    });
  });

  describe('allUsedKeyIdsInLayers()', () => {
    it('should handle a null case', () => {
      assert.sameDeepMembers(Array.from(allUsedKeyIdsInLayers(null).values()),
        []);
    });
    it('should handle a real case', () => {
      assert.sameDeepMembers(Array.from(allUsedKeyIdsInLayers([
        {
          layer: [
            {
              row: [
                { keys: 'q w e r t y' },
                { keys: 'a s d f' },
              ]
            },
            {
              row: [
                { keys: 'Q W E R T Y' },
                { keys: 'A S D F' },
              ]
            }
          ]
        },
        {
          layer: [
            {
              row: [
                { keys: 'q w e r t y' },
                { keys: '0 1 2 3' }
              ]
            },
          ]
        }
      ]).values()),
        'q w e r t y Q W E R T Y a s d f A S D F 0 1 2 3'.split(' '));
    });
  });
  describe('translateLayerAttrToModifier', () => {
    it('should map from layer info to modifier number', () => {
      assert.equal(translateLayerAttrToModifier({
        id: 'base',
      }), constants.keys_mod_none);
      assert.equal(translateLayerAttrToModifier({
        id: 'base',
        modifier: '',
      }), constants.keys_mod_none);
      assert.equal(translateLayerAttrToModifier({
        id: 'base',
        modifier: 'none',
      }), constants.keys_mod_none);
      assert.equal(translateLayerAttrToModifier({
        id: 'shift',
        modifier: 'shift',
      }), constants.keys_mod_shift);
      assert.equal(translateLayerAttrToModifier({
        id: 'shift',
        modifier: 'shiftL shiftR',
      }), constants.keys_mod_shift);
      assert.equal(translateLayerAttrToModifier({
        id: 'shiftL',
        modifier: 'shiftL',
      }), constants.keys_mod_shiftL);
      assert.equal(translateLayerAttrToModifier({
        id: 'altshift',
        modifier: 'alt shift',
      }), constants.keys_mod_alt | constants.keys_mod_shift);
      assert.equal(translateLayerAttrToModifier({
        id: 'altlshiftl',
        modifier: 'alt shiftL',
      }), constants.keys_mod_alt | constants.keys_mod_shiftL);
    });
    it('should round trip each possible modifier', () => {
      for(let str of constants.keys_mod_map.keys()) {
        const layer : LDMLKeyboard.LKLayer = {
          id: str,
          modifier: `${str}`,
        };
        assert.equal(translateLayerAttrToModifier(layer),
          constants.keys_mod_map.get(str), str);
      }
    });
    it('should round trip each possible modifier with altL', () => {
      for(let str of constants.keys_mod_map.keys()) {
        const layer : LDMLKeyboard.LKLayer = {
          id: str,
          modifier: `${str} altL`,
        };
        assert.equal(translateLayerAttrToModifier(layer),
          constants.keys_mod_map.get(str) | constants.keys_mod_altL, str);
      }
    });
  });
  describe('isValidModifier()', () => {
    it('should treat falsy values as valid', () => {
      for(let str of [
        null, undefined, '', 'none'
      ]) {
        assert.ok(validModifier(str), `validModifier(${JSON.stringify(str)})`);
      }
    });
    it('should treat bad values as invalid', () => {
      for(let str of [
        'asdfasdf', 'shift asdfasdf', 'altR-shift', 'altR-shift shift'
      ]) {
        assert.notOk(validModifier(str), `validModifier(${JSON.stringify(str)})`);
      }
    });
  });
});
