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
      const root = new ASTNode(NodeTypes.TMP);
      assert.isNotNull(root);
      assert.equal(root.toString(), '[TMP]');
    });
    it("can construct an ASTNode (with Token)", () => {
      const token = new Token(TokenTypes.BITMAP, 'bitmap');
      const root  = new ASTNode(NodeTypes.TMP, token);
      assert.isNotNull(root);
      assert.equal(root.toString(), '[TMP,[BITMAP,bitmap]]');
    });
    it("can add child", () => {
      const root   = new ASTNode(NodeTypes.TMP);
      const bitmap = new ASTNode(NodeTypes.BITMAP);
      root.addChild(bitmap);
      assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
    });
    it("can add two children (addChild)", () => {
      const root      = new ASTNode(NodeTypes.TMP);
      const bitmap    = new ASTNode(NodeTypes.BITMAP);
      const copyright = new ASTNode(NodeTypes.COPYRIGHT);
      root.addChild(bitmap);
      root.addChild(copyright);
      assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
    });
    it("can add two children (addChildren)", () => {
      const root      = new ASTNode(NodeTypes.TMP);
      const bitmap    = new ASTNode(NodeTypes.BITMAP);
      const copyright = new ASTNode(NodeTypes.COPYRIGHT);
      root.addChildren([bitmap, copyright]);
      assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
    });
    it("can get descendents (no children)", () => {
      const root = new ASTNode(NodeTypes.TMP);
      assert.deepEqual(root.getDescendents(NodeTypes.BITMAP), []);
    });
    it("can get descendents (no matching children)", () => {
      const root = new ASTNode(NodeTypes.TMP);
      const bitmap    = new ASTNode(NodeTypes.BITMAP);
      const copyright = new ASTNode(NodeTypes.COPYRIGHT);
      root.addChildren([bitmap, copyright]);
      assert.deepEqual(root.getDescendents(NodeTypes.VERSION), []);
    });
    it("can get descendents (one matching child)", () => {
      const root = new ASTNode(NodeTypes.TMP);
      const bitmap    = new ASTNode(NodeTypes.BITMAP);
      const copyright = new ASTNode(NodeTypes.COPYRIGHT);
      const version   = new ASTNode(NodeTypes.VERSION);
      root.addChildren([bitmap, copyright, version]);
      assert.deepEqual(root.getDescendents(NodeTypes.VERSION), [version]);
    });
    it("can get descendents (two matching children)", () => {
      const root = new ASTNode(NodeTypes.TMP);
      const bitmap    = new ASTNode(NodeTypes.BITMAP);
      const copyright = new ASTNode(NodeTypes.COPYRIGHT);
      const version1  = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
      const version2  = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
      bitmap.addChild(version1);
      root.addChildren([bitmap, copyright, version2]);
      assert.deepEqual(root.getDescendents(NodeTypes.VERSION), [version1, version2]);
    });
  });
});
