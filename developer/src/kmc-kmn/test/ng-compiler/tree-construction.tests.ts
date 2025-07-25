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

let root: ASTNode      = null;
let bitmap: ASTNode    = null;
let copyright: ASTNode = null;
let version: ASTNode   = null;

describe("Tree Construction Tests", () => {
  describe("ASTNode Tests", () => {
    beforeEach(() => {
      root      = new ASTNode(NodeTypes.TMP);
      bitmap    = new ASTNode(NodeTypes.BITMAP);
      copyright = new ASTNode(NodeTypes.COPYRIGHT);
      version   = new ASTNode(NodeTypes.VERSION);
    });
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
    it("addChild(), can add child", () => {
      root.addChild(bitmap);
      assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
    });
    it("addChild(), can add two children", () => {
      root.addChild(bitmap);
      root.addChild(copyright);
      assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
    });
    it("addChildren(), can add two children", () => {
      root.addChildren([bitmap, copyright]);
      assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
    });
    it("addChildren(), can handle adding empty list of children", () => {
      root.addChildren([]);
      assert.equal(root.toString(), '[TMP]');
    });
    it("addNewChildWithToken(), can construct and add a child with a token", () => {
      const token = new Token(TokenTypes.BITMAP, 'bitmap');
      root.addNewChildWithToken(NodeTypes.BITMAP, token);
      assert.equal(root.toString(), '[TMP,{[BITMAP,[BITMAP,bitmap]]}]');
    });
    it("getDescendents(), can get descendents (no children)", () => {
      assert.deepEqual(root.getDescendents(NodeTypes.BITMAP), []);
    });
    it("getDescendents(), can get descendents (no matching children)", () => {
      root.addChildren([bitmap, copyright]);
      assert.deepEqual(root.getDescendents(NodeTypes.VERSION), []);
    });
    it("getDescendents(), can get descendents (one matching child)", () => {
      root.addChildren([bitmap, copyright, version]);
      assert.deepEqual(root.getDescendents(NodeTypes.VERSION), [version]);
    });
    it("getDescendents(), can get descendents (two matching descendents)", () => {
      const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
      const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
      bitmap.addChild(version1);
      root.addChildren([bitmap, copyright, version2]);
      assert.deepEqual(root.getDescendents(NodeTypes.VERSION), [version1, version2]);
    });
    it("hasChild(), can check if there is at least one child (no children)", () => {
      assert.isFalse(root.hasChild());
    });
    it("hasChild(), can check if there is at least one child (one child)", () => {
      root.addChild(bitmap);
      assert.isTrue(root.hasChild());
    });
    it("hasChild(), can check if there is at least one child (two children)", () => {
      root.addChildren([bitmap, copyright]);
      assert.isTrue(root.hasChild());
    });
    it("hasChildOfType(), can handle checking if there is at least one child of a given type (null)", () => {
      assert.isFalse(root.hasChildOfType(null));
    });
    it("hasChildOfType(), can check if there is at least one child of a given type (no children)", () => {
      assert.isFalse(root.hasChildOfType(NodeTypes.BITMAP));
    });
    it("hasChildOfType(), can check if there is at least one child of a given type (no match)", () => {
      root.addChildren([copyright, version]);
      assert.isFalse(root.hasChildOfType(NodeTypes.BITMAP));
    });
    it("hasChildOfType(), can check if there is at least one child of a given type (one match)", () => {
      root.addChildren([bitmap, copyright, version]);
      assert.isTrue(root.hasChildOfType(NodeTypes.BITMAP));
    });
    it("hasChildOfType(), can check if there is at least one child of a given type (two matching children)", () => {
      const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
      const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
      root.addChildren([bitmap, copyright, version1, version2]);
      assert.isTrue(root.hasChildOfType(NodeTypes.VERSION));
    });
    it("getText(), get token text (no token)", () => {
      assert.equal(root.getText(), '');
    });
    it("getText(), get token text", () => {
      const token = new Token(TokenTypes.BITMAP, 'bitmap');
      root = new ASTNode(NodeTypes.BITMAP, token);
      assert.equal(root.getText(), 'bitmap');
    });
    it("getTextOfType(), get token text of sole child (no child)", () => {
      assert.equal(root.getTextOfType(NodeTypes.BITMAP), '');
    });
    it("getTextOfType(), get token text of sole child (non-matching child)", () => {
      root.addChildren([copyright, version]);
      assert.equal(root.getTextOfType(NodeTypes.BITMAP), '');
    });
    it("getTextOfType(), get token text of sole child (matching child without token)", () => {
      root.addChildren([copyright, version]); // extra children
      root.addChild(bitmap);
      assert.equal(root.getTextOfType(NodeTypes.BITMAP), '');
    });
    it("getTextOfType(), get token text of sole child (matching child with token)", () => {
      root.addChildren([copyright, version]); // extra children
      const token = new Token(TokenTypes.BITMAP, 'bitmap');
      root.addNewChildWithToken(NodeTypes.BITMAP, token);
      assert.equal(root.getTextOfType(NodeTypes.BITMAP), 'bitmap');
    });
    it("getTextOfType(), get token text of sole child (two matching children)", () => {
      const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
      const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
      root.addChildren([bitmap, copyright, version1, version2]); // including extra children
      assert.equal(root.getTextOfType(NodeTypes.VERSION), '');
    });
    it("getSoleChild(), get sole child (no child)", () => {
      assert.isNull(root.getSoleChild());
      assert.equal(root.toString(), '[TMP]');
    });
    it("getSoleChild(), get sole child (one child)", () => {
      root.addChild(bitmap);
      assert.equal(root.getSoleChild(), bitmap);
      assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
    });
    it("getSoleChild(), get sole child (two children)", () => {
      root.addChildren([bitmap, copyright]);
      assert.isNull(root.getSoleChild());
      assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
    });
    it("getSoleChildOfType(), get sole child of type (no child)", () => {
      assert.isNull(root.getSoleChildOfType(NodeTypes.BITMAP));
      assert.equal(root.toString(), '[TMP]');
    });
    it("getSoleChildOfType(), get sole child of type (no matching child)", () => {
      root.addChildren([copyright, version]); // extra children
      assert.isNull(root.getSoleChildOfType(NodeTypes.BITMAP));
      assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
    });
    it("getSoleChildOfType(), get sole child of type (one matching child)", () => {
      root.addChildren([bitmap, copyright, version]); // extra children
      assert.equal(root.getSoleChildOfType(NodeTypes.VERSION), version);
      assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION]}]');
    });
    it("getSoleChildOfType(), get sole child of type (two matching children)", () => {
      const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
      const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
      root.addChildren([bitmap, copyright, version1, version2]); // including extra children
      assert.isNull(root.getSoleChildOfType(NodeTypes.VERSION));
      assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION,[VERSION,1]],[VERSION,[VERSION,2]]}]');
    });
  });
});
