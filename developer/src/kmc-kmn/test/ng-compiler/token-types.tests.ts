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
import { TokenTypes } from '../../src/ng-compiler/token-types.js';

describe("TokenTypes Tests", () => {
  it("is sorted in alphabetical order", () => {
    const keys       = Object.keys(TokenTypes);
    const sortedKeys = [...keys].sort();
    assert.deepEqual(keys, sortedKeys);
  });
});
