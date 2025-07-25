/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * Tests for KMC KMN Next Generation Parser (Tree Construction)
 */

import 'mocha';
import { assert } from 'chai';
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { NodeTypes } from '../../src/ng-compiler/node-types.js';
import { Token } from '../../src/ng-compiler/lexer.js';
import { TokenTypes } from '../../src/ng-compiler/token-types.js';

describe("Tree Construction Tests", () => {
  describe("ASTNode Tests", () => {
    it("can construct an ASTNode", () => {
      const ast = new ASTNode(NodeTypes.TMP);
      assert.isNotNull(ast);
      assert.equal(ast.toString(), '[TMP]');
    });
    it("can construct an ASTNode (with Token)", () => {
      const token = new Token(TokenTypes.BITMAP, 'bitmap');
      const ast   = new ASTNode(NodeTypes.TMP, token);
      assert.isNotNull(ast);
      assert.equal(ast.toString(), '[TMP,[BITMAP,bitmap]]');
    });
  });
});
