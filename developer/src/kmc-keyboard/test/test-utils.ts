import 'mocha';
import {assert} from 'chai';
import { isValidEnumValue, calculateUniqueKeys, allUsedKeyIdsInLayers } from '../src/util/util.js';

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
});
