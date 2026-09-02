import { assert } from 'chai';
import { SyntheticTextStore } from 'keyman/engine/keyboard';

describe('SyntheticTextStoreTests', function() {
  describe('app|les', () => {
    const testTextStore = new SyntheticTextStore('apples', 3);

    it('Cloning with .from()', () => {
      assert.deepEqual(SyntheticTextStore.from(testTextStore), testTextStore);
      assert.notStrictEqual(SyntheticTextStore.from(testTextStore), testTextStore);
    });

    it('getText', () => {
      assert.equal(testTextStore.getText(), 'apples');
    });

    it('getTextBeforeCaret', () => {
      assert.equal(testTextStore.getTextBeforeCaret(), 'app');
    });

    it('getTextAfterCaret', () => {
      assert.equal(testTextStore.getTextAfterCaret(), 'les');
    });

    it('getSelectedText', () => {
      assert.equal(testTextStore.getSelectedText(), '');
    });

    it('isSelectionEmpty', () => {
      assert.isTrue(testTextStore.isSelectionEmpty());
    });

    it('clearSelection', () => {
      let editTextStore = SyntheticTextStore.from(testTextStore);
      editTextStore.clearSelection();

      assert.equal(editTextStore.getText(), testTextStore.getTextBeforeCaret() + testTextStore.getTextAfterCaret());
      assert.equal(editTextStore.getTextBeforeCaret(), testTextStore.getTextBeforeCaret());
      assert.equal(editTextStore.getTextAfterCaret(), testTextStore.getTextAfterCaret());
      assert.isTrue(editTextStore.isSelectionEmpty());

      let postClear = SyntheticTextStore.from(editTextStore);
      editTextStore.clearSelection(); // on same object; make sure its internal selection stuff updates correctly!
      assert.notStrictEqual(postClear, editTextStore);
      assert.deepEqual(postClear, editTextStore);
    });
  });

  describe('app|les and ba|nanas', () => {  // selection = 'les and ba'
    const testTextStore = new SyntheticTextStore('apples and bananas', 3, 13);

    it('Cloning with from()', () => {
      assert.deepEqual(SyntheticTextStore.from(testTextStore), testTextStore);
      assert.notStrictEqual(SyntheticTextStore.from(testTextStore), testTextStore);
    });

    it('getText', () => {
      assert.equal(testTextStore.getText(), 'apples and bananas');
    });

    it('getTextBeforeCaret', () => {
      assert.equal(testTextStore.getTextBeforeCaret(), 'app');
    });

    it('getTextAfterCaret', () => {
      assert.equal(testTextStore.getTextAfterCaret(), 'nanas');
    });

    it('getSelectedText', () => {
      assert.equal(testTextStore.getSelectedText(), 'les and ba');
    });

    it('isSelectionEmpty', () => {
      assert.isFalse(testTextStore.isSelectionEmpty());
    });

    it('clearSelection', () => {
      let editTextStore = SyntheticTextStore.from(testTextStore);
      editTextStore.clearSelection();

      assert.equal(editTextStore.getText(), testTextStore.getTextBeforeCaret() + testTextStore.getTextAfterCaret());
      assert.equal(editTextStore.getTextBeforeCaret(), testTextStore.getTextBeforeCaret());
      assert.equal(editTextStore.getTextAfterCaret(), testTextStore.getTextAfterCaret());
      assert.isTrue(editTextStore.isSelectionEmpty());

      let postClear = SyntheticTextStore.from(editTextStore);
      editTextStore.clearSelection(); // on same object; make sure its internal selection stuff updates correctly!
      assert.notStrictEqual(postClear, editTextStore);
      assert.deepEqual(postClear, editTextStore);
    });
  });
});