/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { assert } from 'chai';
import 'mocha';
import { isValidKeymanKeyboardId, isValidLdmlKeyboardId, isValidLexicalModelId } from '../src/valid-ids.js';

describe('valid-ids', function () {
  it('should accept a valid Keyman keyboard ID', function() {
    [
      'khmer_angkor',
      'm',
      'fancypants1_2',
      'keyboard___',
      '_100',
    ].forEach(id => assert.isTrue(isValidKeymanKeyboardId(id), `expected '${id}' to be valid`));
  });

  it('should reject invalid Keyman keyboard IDs', function() {
    [
      'khmer_angkor@example.com',
      'keyboard ',
      'Khmer',
      ' keyboard',
      '111',
      '?',
      'keyboard.version',
    ].forEach(id => assert.isFalse(isValidKeymanKeyboardId(id), `expected '${id}' to be invalid`));
  });

  it('should accept a valid LDML keyboard ID', function() {
    [
      'khmer_angkor',
      'm',
      'fancypants1_2',
      'keyboard___',
      '_100',
    ].forEach(id => assert.isTrue(isValidLdmlKeyboardId(id), `expected '${id}' to be valid`));
  });

  it('should reject invalid LDML keyboard IDs', function() {
    [
      'khmer_angkor@example.com',
      'keyboard ',
      ' keyboard',
      'Khmer',
      '111',
      '?',
      'keyboard.version',
    ].forEach(id => assert.isFalse(isValidLdmlKeyboardId(id), `expected '${id}' to be invalid`));
  });

  it('should accept a valid lexical model ID', function() {
    [
      'khmer.km.khmer_angkor',
      'nrc.en.mtnt',
    ].forEach(id => assert.isTrue(isValidLexicalModelId(id), `expected '${id}' to be valid`));
  });

  it('should reject invalid lexical model IDs', function() {
    [
      'khmer',
      'en-us',
      'khmer.km-KM.angkor',
      'Khmer.km_km.angkor',
      'model ',
      ' model',
      '111',
      '?',
      'lexical.model',
      'lexical..model',
      '.en.model',
    ].forEach(id => assert.isFalse(isValidLexicalModelId(id), `expected '${id}' to be invalid`));
  });
});
