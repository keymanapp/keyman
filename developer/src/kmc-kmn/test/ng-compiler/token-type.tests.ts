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

type TokenTypeKey = keyof typeof TokenType;

describe("TokenType Tests", () => {
  it("is sorted in alphabetical order", () => {
    const keys = Object.keys(TokenType) as TokenTypeKey[];
    const sortedKeys = [...keys].sort();
    assert.deepEqual(keys, sortedKeys);
  });
  it("matches types actually used in the Lexer", () => {
    const keys = (Object.keys(TokenType) as TokenTypeKey[]).sort();
    const lexer = new Lexer(null);
    const lexerStr = lexer.toString();
    const match = [...lexerStr.matchAll(/(\[)([A-Z_]+)(,.*?,(true|false)\])/g)];
    const lexerKeys = match.map((x) => x[2] as TokenTypeKey);
    lexerKeys.push(TokenType.EOF);
    lexerKeys.sort();
    assert.deepEqual(keys, lexerKeys);
  });
  it("has enum keys that exactly match their enum values", () => {
    for(const key of (Object.keys(TokenType) as TokenTypeKey[])) {
      assert.equal(TokenType[key], key);
    }
  });
});
