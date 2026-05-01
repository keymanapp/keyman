/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import { nestedInstanceOf } from 'keyman/engine/element-text-stores';

describe('nestedInstanceOf', () => {
  it('returns false for null input', () => {
    assert.strictEqual(nestedInstanceOf(null as any, 'HTMLElement'), false);
  });

  it('returns true for Window object when className is "Window"', () => {
    assert.strictEqual(nestedInstanceOf(window as any, 'Window'), true);
  });

  it('returns false for Window object when className is not "Window"', () => {
    assert.strictEqual(nestedInstanceOf(window as any, 'Document'), false);
  });

  it('returns true for Document object when className is "Document"', () => {
    assert.strictEqual(nestedInstanceOf(document as any, 'Document'), true);
  });

  it('returns false for Document object when className is not "Document"', () => {
    assert.strictEqual(nestedInstanceOf(document as any, 'HTMLElement'), false);
  });

  it('returns true for HTMLElement when className is "HTMLElement"', () => {
    const elem = document.createElement('div');
    assert.strictEqual(nestedInstanceOf(elem, 'HTMLElement'), true);
  });

  it('returns false for HTMLElement when className is "HTMLInputElement"', () => {
    const elem = document.createElement('div');
    assert.strictEqual(nestedInstanceOf(elem, 'HTMLInputElement'), false);
  });

  it('returns true for HTMLInputElement when className is "HTMLInputElement"', () => {
    const input = document.createElement('input');
    assert.strictEqual(nestedInstanceOf(input, 'HTMLInputElement'), true);
  });

  it('returns false for unknown className', () => {
    const elem = document.createElement('div');
    assert.strictEqual(nestedInstanceOf(elem, 'NonExistentClass'), false);
  });
});
