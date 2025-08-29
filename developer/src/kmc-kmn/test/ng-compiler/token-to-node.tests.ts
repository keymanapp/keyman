/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-08-25
 *
 * Tests for KMC KMN Next Generation Compiler
 *
 * TokenTypes to NodeTypes mapping for TokenRule
 */

import 'mocha';
import { assert } from 'chai';
import { TokenTypes } from "../../src/ng-compiler/token-types.js";
import { TOKEN_TO_NODE } from '../../src/ng-compiler/token-to-node.js';

describe("TOKEN_TO_NODE Tests", () => {
  it("is sorted in alphabetical order", () => {
    const tokenTypes: TokenTypes[]  = TOKEN_TO_NODE.map((row) => { return row.tokenType });
    const sortedTypes: TokenTypes[] = [...tokenTypes].sort();
    assert.deepEqual(tokenTypes, sortedTypes);
  });
});