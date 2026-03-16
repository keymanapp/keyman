/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import { VariableStore } from 'keyman/engine/keyboard';
import { VariableStoreCookieSerializer } from 'keyman/engine/main';

describe('VariableStoreCookieSerializer', () => {
  describe('saveStore and loadStore', () => {
    const keyboardID = 'test-keyboard';
    const storeName = 'testStore';
    const cookieName = `KeymanWeb_${keyboardID}_Option_${storeName}`;

    afterEach(async () => {
      document.cookie = `${cookieName}=; max-age=0`;
    });

    it('should save and load a simple store with string values', () => {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();
      const storeData: VariableStore = {
        option1: 'value1',
        option2: 'value2',
        option3: 'value3'
      };

      // Act
      serializer.saveStore(keyboardID, storeName, storeData);
      const loaded = serializer.loadStore(keyboardID, storeName);

      // Assert
      assert.deepEqual(loaded, storeData, 'loaded store should match saved store');
    });

    it('should save and load a store with empty values', () => {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();
      const storeData: VariableStore = {
        option1: '',
        option2: 'value',
        option3: ''
      };

      // Act
      serializer.saveStore(keyboardID, storeName, storeData);
      const loaded = serializer.loadStore(keyboardID, storeName);

      // Assert
      assert.deepEqual(loaded, storeData, 'empty string values should be preserved');
    });

    it('should save and load a store with special characters', () => {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();
      const storeData: VariableStore = {
        encoded: 'value:with;special&chars=',
        emoji: '⭐'
      };

      // Act
      serializer.saveStore(keyboardID, storeName, storeData);
      const loaded = serializer.loadStore(keyboardID, storeName);

      // Assert
      assert.deepEqual(loaded, storeData, 'special characters should be properly encoded/decoded');
    });

    it('should handle multiple stores for the same keyboard independently', () => {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();
      const store1Data: VariableStore = { option: 'store1value' };
      const store2Data: VariableStore = { option: 'store2value' };

      // Act
      serializer.saveStore(keyboardID, 'store1', store1Data);
      serializer.saveStore(keyboardID, 'store2', store2Data);
      const loaded1 = serializer.loadStore(keyboardID, 'store1');
      const loaded2 = serializer.loadStore(keyboardID, 'store2');

      // Assert
      assert.deepEqual(loaded1, store1Data, 'first store should be independent');
      assert.deepEqual(loaded2, store2Data, 'second store should be independent');

      // Cleanup
      document.cookie = `KeymanWeb_${keyboardID}_Option_store1=; max-age=0`;
      document.cookie = `KeymanWeb_${keyboardID}_Option_store2=; max-age=0`;
    });

    it('should return empty object for non-existent store', () => {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();

      // Act
      const loaded = serializer.loadStore(keyboardID, 'nonexistent');

      // Assert
      assert.deepEqual(loaded, {}, 'non-existent store should return empty object');
    });
  });

  describe('findStores', () => {
    it('should return an empty array when no stores exist for a keyboardID', () => {

      // Arrange
      const serializer = new VariableStoreCookieSerializer();
      const keyboardID = 'test-keyboard';
      const expected: VariableStore[] = [];

      // Act
      const result = serializer.findStores(keyboardID);

      // Assert
      assert.deepEqual(result, expected, 'result should be an empty array');
      assert.isTrue(Array.isArray(result), 'result should be an array');
      assert.strictEqual(result.length, 0, 'result array length should be 0');
    });

    it('should return store for keyboard', () => {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();
      const storeData1: VariableStore = {
        option1: 'value1',
        option2: 'value2',
        option3: 'value3'
      };
      serializer.saveStore('test-keyboard', 'storeName', storeData1);
      const storeData2: VariableStore = {
        settingA: 'A',
        settingB: 'B'
      };
      serializer.saveStore('another-keyboard', 'anotherStore', storeData2);

      // Act
      const stores = serializer.findStores('test-keyboard');

      // Assert
      assert.deepEqual(stores, [storeData1], 'stores should match saved store');

      // Cleanup
      document.cookie = `KeymanWeb_test-keyboard_Option_storeName=; max-age=0`;
    });

  });
});
