/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-08-13
 *
 * Tests for KMC KMN Next Generation Lexer
 *
 * TokenTypes for the Lexer
 */

import 'mocha';
import { assert } from 'chai';
import { TokenType } from '../../src/ng-compiler/token-type.js';
import { Lexer } from '../../src/ng-compiler/lexer.js';

describe("TokenType Tests", () => {
  it("is sorted in alphabetical order", () => {
    const keys: string[]       = Object.keys(TokenType);
    const sortedKeys: string[] = [...keys].sort();
    assert.deepEqual(keys, sortedKeys);
  });
  it("matches types actually used in the Lexer", () => {
    const keys: string[] = Object.keys(TokenType).sort();
    const lexer = new Lexer(null);
    const lexerStr = lexer.toString();
    const match = [...lexerStr.matchAll(/(?:\[)[A-Z_]{2,}/g)];
    const lexerKeys = match.map((x) => x[0].slice(1));
    lexerKeys.push('EOF');
    lexerKeys.sort();
    assert.deepEqual(keys, lexerKeys);
  });
  it("has enum keys that exactly match their enum values", () => {
    type TokenTypeKey = keyof typeof TokenType;
    for(const key of (Object.keys(TokenType) as TokenTypeKey[])) {
      assert.equal(TokenType[key], key);
    }
  });
});
