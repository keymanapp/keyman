/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import 'mocha';
import { assert } from 'chai';
import { Rule } from '../../src/ng-compiler/recursive-descent.js';
import { Lexer, Token } from '../../src/ng-compiler/lexer.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { BitmapStoreAssignRule, BitmapStoreRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';

let root: ASTNode = null;

describe("KMN Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeTypes.TMP);
  });
  describe("BitmapStoreAssignRule Tests", () => {
    it("can construct a BitmapStoreAssignRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const bitmapStoreAssign: Rule = new BitmapStoreAssignRule(tokenBuffer);
      assert.isNotNull(bitmapStoreAssign);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const bitmapStoreAssign: Rule = new BitmapStoreAssignRule(tokenBuffer);
      assert.isTrue(bitmapStoreAssign.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.BITMAP);
      assert.equal(root.getSoleChild().getSoleChild().nodeType, NodeTypes.STRING);
    });
  });
  describe("BitmapStoreRule Tests", () => {
    it("can construct a BitmapStoreRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap)');
      const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
      assert.isNotNull(bitmapStore);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap)');
      const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
      assert.isTrue(bitmapStore.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.BITMAP);
    });
  });
});

function stringToTokenBuffer(buffer: String): TokenBuffer {
  const lexer = new Lexer(buffer);
  const tokens: Token[] = lexer.parse();
  return new TokenBuffer(tokens);
}
