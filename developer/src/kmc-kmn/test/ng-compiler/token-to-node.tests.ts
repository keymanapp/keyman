/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-08-25
 *
 * Tests for KMC KMN Next Generation Compiler
 *
 * TokenType to NodeTypes mapping for TokenRule
 */

import 'mocha';
import { assert } from 'chai';
import { TokenType } from "../../src/ng-compiler/token-type.js";
import { TOKEN_TO_NODE } from '../../src/ng-compiler/token-to-node.js';
import { readFileSync } from 'node:fs';

describe("TOKEN_TO_NODE Tests", () => {
  it("is sorted in alphabetical order", () => {
    const tokenTypes: TokenType[]  = TOKEN_TO_NODE.map((row) => { return row.tokenType });
    const sortedTypes: TokenType[] = [...tokenTypes].sort();
    assert.deepEqual(tokenTypes, sortedTypes);
  });
  it("has matching TokenType and NodeType values", () => {
    const tokenTypeValues: string[] = TOKEN_TO_NODE.map((row) => { return row.tokenType.toString(); });
    const nodeTypeValues: string[]  = TOKEN_TO_NODE.map((row) => {
      const value = row.nodeType.toString();
      return value === "LINE" ? "NEWLINE" : value;
    });
    assert.deepEqual(tokenTypeValues, nodeTypeValues);
  });
  it("contains all the BNF tokens", () => {
    const bnfBuffer: string = readFileSync('../../src/kmc-kmn/src/ng-compiler/kmn-file.bnf').toString()
    const match = [...bnfBuffer.matchAll(/[A-Z_]{2,}/g)];
    const bnfTokens = match.map((x) => x[0]);
    bnfTokens.sort();
    const bnfTokenSet = new Set<String>(bnfTokens);
    const tokenTypes: string[] = TOKEN_TO_NODE.map((row) => { return row.tokenType.toString(); })
    // BNF tokens that do not get mapped directly to NodeTypes
    tokenTypes.push('CHEVRON', 'COMMA', 'LEFT_BR', 'LEFT_SQ', 'OLDCHARPOSMATCHING', 'PLUS', 'RANGE', 'RIGHT_BR', 'RIGHT_SQ');
    tokenTypes.sort();
    assert.deepEqual([...bnfTokenSet], tokenTypes);
  });
});
