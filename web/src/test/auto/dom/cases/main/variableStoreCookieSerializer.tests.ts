/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import { VariableStoreCookieSerializer } from 'keyman/engine/main';

describe('VariableStoreCookieSerializer', function() {
  describe('saveStore and loadStore', function() {
    const keyboardID = 'test-keyboard';
    const storeName = 'testStore';
    const cookieName = `KeymanWeb_${keyboardID}_Option_${storeName}`;

    afterEach(function() {
      document.cookie = `${cookieName}=; max-age=0`;
    });

    it('should save and load a simple store with string value', function() {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();

      // Act
      serializer.saveStore(keyboardID, storeName, 'value1');
      const loaded = serializer.loadStore(keyboardID, storeName);

      // Assert
      assert.equal(loaded, 'value1', 'loaded store should match saved store');
    });

    it('should save and load a store with empty value', function() {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();

      // Act
      serializer.saveStore(keyboardID, storeName, '');
      const loaded = serializer.loadStore(keyboardID, storeName);

      // Assert
      assert.equal(loaded, '', 'empty string values should be preserved');
    });

    it('should save and load a store with special characters', function() {
      // Arrange  `
      const serializer = new VariableStoreCookieSerializer();
      const encoded = 'value:with;special&chars=';

      // Act
      serializer.saveStore(keyboardID, storeName, encoded);
      const loaded = serializer.loadStore(keyboardID, storeName);

      // Assert
      assert.equal(loaded, encoded, 'special characters should be properly encoded/decoded');
    });

    it('should save and load a store with non-BMP characters', function() {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();
      const emoji = '⭐';

      // Act
      serializer.saveStore(keyboardID, storeName, emoji);
      const loaded = serializer.loadStore(keyboardID, storeName);

      // Assert
      assert.equal(loaded, emoji, 'special characters should be properly encoded/decoded');
    });

    it('should return undefined for non-existent store', function() {
      // Arrange
      const serializer = new VariableStoreCookieSerializer();

      // Act
      const loaded = serializer.loadStore(keyboardID, 'nonexistent');

      // Assert
      assert.isUndefined(loaded, 'non-existent store should return undefined');
    });
  });

});
