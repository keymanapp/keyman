import 'mocha';
import {assert} from 'chai';
import {
  allUsedKeyIdsInFlick, allUsedKeyIdsInKey,
  isValidEnumValue, calculateUniqueKeys, allUsedKeyIdsInLayers,
  translateLayerAttrToModifier, validModifier, verifyValidAndUnique } from '../src/util/util.js';
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
        { id: 'a', output: 'a' },
        { id: 'a', output: 'a' }, // dup
        { id: 'b', output: 'b' },
        { id: 'a', output: 'å' }, // override
      ]),[
        { id: 'b', output: 'b' },
        { id: 'a', output: 'å' },
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
  describe('allUsedKeyIdsInFlick', () => {
    it('should handle a null case', () => {
      assert.sameDeepMembers(Array.from(allUsedKeyIdsInFlick(
        undefined
      )),
        []);
    });
    it('should handle a simple case', () => {
      assert.sameDeepMembers(Array.from(allUsedKeyIdsInFlick(
        {
          id: 'something',
          flickSegment: [
            { keyId: 'aaa', directions: 'nnw se w up' },
          ]
        }
      )),
        'aaa'.split(' '));
    });
    it('should handle a redundant case', () => {
      assert.sameDeepMembers(Array.from(allUsedKeyIdsInFlick(
        {
          id: 'something',
          flickSegment: [
            { keyId: 'aaa', directions: 'nnw se w up' },
            { keyId: 'bbb', directions: 'e ne e' },
            { keyId: 'ccc', directions: 'turn left here, go about half a mile past where old man henry’s place used to be, hang a right, can’t miss it' },
            { keyId: 'aaa', directions: 'w e n s' },
          ]
        }
      )),
        ['aaa','bbb','ccc']); // aaa is in there twice
    });
  });
  describe('allUsedKeyIdsInKey', () => {
    it('should handle a simple case', () => {
      assert.sameDeepMembers(Array.from(allUsedKeyIdsInKey(
        {}
      ).keys()),
      []);
    });
    it('should handle a straightforward case', () => {
      assert.sameDeepMembers(Array.from(allUsedKeyIdsInKey(
        {
          flickId: 'ignore me',
          multiTapKeyIds: 'tap1 tap2 tap3',
          longPressDefaultKeyId: 'longPress0',
          longPressKeyIds: 'longPress1 longPress2 longPress0'
        }
      ).keys()),
      ['tap1','tap2','tap3','longPress0', 'longPress1','longPress2']);
    });
    it('should handle a duplicate case', () => {
      const auk = allUsedKeyIdsInKey(
        {
          flickId: 'ignore me',
          multiTapKeyIds: 'a b c',
          longPressDefaultKeyId: 'd',
          longPressKeyIds: 'c d e'
        }
      );
      assert.sameDeepMembers(Array.from(auk.keys()),
      ['a','b','c','d','e']);
      assert.sameDeepMembers(Array.from(auk.entries()), [
        ['a',['multiTapKeyIds']],
        ['b',['multiTapKeyIds']],
        ['c',['longPressKeyIds','multiTapKeyIds']],
        ['d',['longPressDefaultKeyId','longPressKeyIds']],
        ['e',['longPressKeyIds']],
      ]);
    });
  });
  describe('translateLayerAttrToModifier', () => {
    it('should map from layer info to modifier number', () => {
      assert.sameDeepMembers(translateLayerAttrToModifier({
        id: 'base',
      }), [constants.keys_mod_none]);
      assert.sameDeepMembers(translateLayerAttrToModifier({
        id: 'base',
        modifiers: '',
      }), [constants.keys_mod_none]);
      assert.sameDeepMembers(translateLayerAttrToModifier({
        id: 'base',
        modifiers: 'none',
      }), [constants.keys_mod_none]);
      assert.sameDeepMembers(translateLayerAttrToModifier({
        id: 'shift',
        modifiers: 'shift',
      }), [constants.keys_mod_shift]);
      assert.sameDeepMembers(translateLayerAttrToModifier({
        id: 'shift',
        modifiers: 'shift',
      }), [constants.keys_mod_shift]);
      assert.sameDeepMembers(translateLayerAttrToModifier({
        id: 'shiftOrCtrl',
        modifiers: 'shift,ctrlL',
      }), [constants.keys_mod_shift,constants.keys_mod_ctrlL]);
      assert.sameDeepMembers(translateLayerAttrToModifier({
        id: 'altshift',
        modifiers: 'alt shift',
      }), [constants.keys_mod_alt | constants.keys_mod_shift]);
    });
    it('should round trip each possible modifier', () => {
      for(let str of constants.keys_mod_map.keys()) {
        const layer : LDMLKeyboard.LKLayer = {
          id: str,
          modifiers: `${str}`,
        };
        assert.sameDeepMembers(translateLayerAttrToModifier(layer),
          [constants.keys_mod_map.get(str)], str);
      }
    });
    it('should round trip each possible modifier with altL', () => {
      for(let str of constants.keys_mod_map.keys()) {
        const layer : LDMLKeyboard.LKLayer = {
          id: str,
          modifiers: `${str} altL`,
        };
        assert.sameDeepMembers(translateLayerAttrToModifier(layer),
          [constants.keys_mod_map.get(str) | constants.keys_mod_altL], str);
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
  describe('verifyValidAndUnique()', () => {
    it('should pass a valid set', () => {
      assert.ok(verifyValidAndUnique(
        ['a', 'b', 'c'],
        dups => assert.fail(`Dups: ${dups.join(',')}`)));
      assert.ok(verifyValidAndUnique(
        [],
        dups => assert.fail(`Dups: ${dups.join(',')}`)));
      assert.ok(verifyValidAndUnique(
        ['a', 'b', 'c'],
        dups => assert.fail(`Dups: ${dups.join(',')}`),
        new Set(['a', 'b', 'c', 'd']),
        invalids => assert.fail(`Invalid: ${invalids.join(',')}`)));
    });
    describe('should fail invalid valid sets', () => {
      it('should fail a dup set', () => {
        let dups = null;
        assert.notOk(verifyValidAndUnique(
          ['a', 'b', 'c', 'b'], d => (dups = d)));
        assert.deepEqual(dups, ['b']);
      });
      it('should fail a highly duplicated set', () => {
        let dups = null;
        assert.notOk(verifyValidAndUnique(
          ['a', 'b', 'c', 'b', 'c', 'c', 'b'], d => (dups = d)));
        assert.deepEqual(dups, ['b', 'c']);
      });
      it('should fail another dup set', () => {
        let dups = null;
        assert.notOk(verifyValidAndUnique(
          ['a', 'b', 'c', 'b'], d => (dups = d),
          new Set(['a', 'b', 'c', 'd']),
          invalids => assert.fail(`Invalid: ${invalids.join(',')}`)));
        assert.deepEqual(dups, ['b']);
      });
      it('should fail a highly duplicated and invalid set', () => {
        let dups = null;
        let invalids = null;
        assert.notOk(verifyValidAndUnique(
          ['a', 'b', 'c', 'b', 'c', 'c', 'b', 'q'], d => (dups = d),
          new Set(['a', 'b', 'c', 'd']), i => (invalids = i)));
        assert.deepEqual(dups, ['b', 'c']);
        assert.deepEqual(invalids, ['q']);
      });
      it('should fail a highly duplicated and highly invalid set', () => {
        let dups = null;
        let invalids = null;
        assert.notOk(verifyValidAndUnique(
          ['a', 'b', 'c', 'q', 'b', 'c', 'c', 'b', 'q', 'z'], d => (dups = d),
          new Set(['a', 'b', 'c', 'd']), i => (invalids = i)));
        assert.deepEqual(dups, ['b', 'c', 'q']);
        assert.deepEqual(invalids, ['q', 'z']);
      });
      it('should fail an invalid set', () => {
        let invalids = null;
        assert.notOk(verifyValidAndUnique(
          ['a', 'b', 'c', 'q', 'z'], dups => assert.fail(`Dups: ${dups.join(',')}`),
          new Set(['a', 'b', 'c', 'd']), i => (invalids = i)));
        assert.deepEqual(invalids, ['q', 'z']);
      });
    });
  });
});
