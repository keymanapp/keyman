/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Lexer
 */

import 'mocha';
import { assert } from 'chai';
import { lexer, ScanRecogniser, Token, TokenTypes } from '../../src/ng-compiler/lexer.js'

describe("Lexer Tests", () => {
  describe("ScanRecogniser", () => {
    it("can construct a ScanRecogniser", () => {
      const sr = new ScanRecogniser(TokenTypes.TT_STORE, new RegExp("^store"),      true);
      assert.deepEqual(sr.toString(), '[TT_STORE,/^store/,true]');
    });
  });
  describe("Token", () => {
    it("can construct a Token", () => {
      const token = new Token(TokenTypes.TT_STORE, 'store');
      assert.deepEqual(token.toString(), '[TT_STORE,store]');
    });
  });
  describe("lexer", () => {
    it("can recognise a TT_STORE token", () => {
      const actual   = lexer(new String('store'));
      const expected = [new Token(TokenTypes.TT_STORE, 'store')];
      assert.deepEqual(actual, expected);
    });
  });
});
