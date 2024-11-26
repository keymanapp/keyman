import { assert } from 'chai';
import { Mock } from 'keyman/engine/js-processor';

describe('Mocks', function() {
  describe('app|les', () => {
    const testMock = new Mock('apples', 3);

    it('Cloning with .from()', () => {
      assert.deepEqual(Mock.from(testMock), testMock);
      assert.notStrictEqual(Mock.from(testMock), testMock);
    });

    it('getText', () => {
      assert.equal(testMock.getText(), 'apples');
    });

    it('getTextBeforeCaret', () => {
      assert.equal(testMock.getTextBeforeCaret(), 'app');
    });

    it('getTextAfterCaret', () => {
      assert.equal(testMock.getTextAfterCaret(), 'les');
    });

    it('getSelectedText', () => {
      assert.equal(testMock.getSelectedText(), '');
    });

    it('isSelectionEmpty', () => {
      assert.isTrue(testMock.isSelectionEmpty());
    });

    it('clearSelection', () => {
      let editMock = Mock.from(testMock);
      editMock.clearSelection();

      assert.equal(editMock.getText(), testMock.getTextBeforeCaret() + testMock.getTextAfterCaret());
      assert.equal(editMock.getTextBeforeCaret(), testMock.getTextBeforeCaret());
      assert.equal(editMock.getTextAfterCaret(), testMock.getTextAfterCaret());
      assert.isTrue(editMock.isSelectionEmpty());

      let postClear = Mock.from(editMock);
      editMock.clearSelection(); // on same object; make sure its internal selection stuff updates correctly!
      assert.notStrictEqual(postClear, editMock);
      assert.deepEqual(postClear, editMock);
    });
  });

  describe('app|les and ba|nanas', () => {  // selection = 'les and ba'
    const testMock = new Mock('apples and bananas', 3, 13);

    it('Cloning with from()', () => {
      assert.deepEqual(Mock.from(testMock), testMock);
      assert.notStrictEqual(Mock.from(testMock), testMock);
    });

    it('getText', () => {
      assert.equal(testMock.getText(), 'apples and bananas');
    });

    it('getTextBeforeCaret', () => {
      assert.equal(testMock.getTextBeforeCaret(), 'app');
    });

    it('getTextAfterCaret', () => {
      assert.equal(testMock.getTextAfterCaret(), 'nanas');
    });

    it('getSelectedText', () => {
      assert.equal(testMock.getSelectedText(), 'les and ba');
    });

    it('isSelectionEmpty', () => {
      assert.isFalse(testMock.isSelectionEmpty());
    });

    it('clearSelection', () => {
      let editMock = Mock.from(testMock);
      editMock.clearSelection();

      assert.equal(editMock.getText(), testMock.getTextBeforeCaret() + testMock.getTextAfterCaret());
      assert.equal(editMock.getTextBeforeCaret(), testMock.getTextBeforeCaret());
      assert.equal(editMock.getTextAfterCaret(), testMock.getTextAfterCaret());
      assert.isTrue(editMock.isSelectionEmpty());

      let postClear = Mock.from(editMock);
      editMock.clearSelection(); // on same object; make sure its internal selection stuff updates correctly!
      assert.notStrictEqual(postClear, editMock);
      assert.deepEqual(postClear, editMock);
    });
  });
});