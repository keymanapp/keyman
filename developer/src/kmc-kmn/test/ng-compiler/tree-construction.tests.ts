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
import { NodeType } from '../../src/ng-compiler/node-type.js';
import { Token } from '../../src/ng-compiler/lexer.js';
import { TokenType } from '../../src/ng-compiler/token-type.js';

let root: ASTNode      = null;
let bitmap: ASTNode    = null;
let copyright: ASTNode = null;
let version: ASTNode   = null;

function initVariables(): void {
  root      = new ASTNode(NodeType.TMP);
  bitmap    = new ASTNode(NodeType.BITMAP);
  copyright = new ASTNode(NodeType.COPYRIGHT);
  version   = new ASTNode(NodeType.VERSION);
}

describe("Tree Construction Tests", () => {
  describe("ASTNode Tests", () => {
    describe("ASTNode.constructor()", () => {
      it("can construct an ASTNode", () => {
        const root = new ASTNode(NodeType.TMP);
        assert.isNotNull(root);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can construct an ASTNode (with Token)", () => {
        const token = new Token(TokenType.BITMAP, 'bitmap');
        const root  = new ASTNode(NodeType.TMP, token);
        assert.isNotNull(root);
        assert.equal(root.toString(), '[TMP,[BITMAP,bitmap]]');
      });
    });
    describe("ASTNode.addChild()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can add child", () => {
        root.addChild(bitmap);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
      it("can add two children", () => {
        root.addChild(bitmap);
        root.addChild(copyright);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
      it("can handle adding a null (no children)", () => {
        root.addChild(null);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle adding a null (one child)", () => {
        root.addChild(bitmap);
        root.addChild(null);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
    });
    describe("ASTNode.addChildren()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can add one child", () => {
        root.addChildren([bitmap]);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
      it("can add two children", () => {
        root.addChildren([bitmap, copyright]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
      it("can handle adding empty list of children (no children)", () => {
        root.addChildren([]);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle adding a null (no children)", () => {
        root.addChildren(null);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle adding empty list of children (one child)", () => {
        root.addChild(bitmap);
        root.addChildren([]);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
      it("can handle adding a null (one child)", () => {
        root.addChild(bitmap);
        root.addChildren(null);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
    });
    describe("ASTNode.addNewChildWithToken()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can construct and add a child with a token", () => {
        const token = new Token(TokenType.BITMAP, 'bitmap');
        root.addNewChildWithToken(NodeType.BITMAP, token);
        assert.equal(root.toString(), '[TMP,{[BITMAP,[BITMAP,bitmap]]}]');
      });
      it("can handle adding a child with a null token", () => {
        root.addNewChildWithToken(NodeType.BITMAP, null);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
      it("can handle adding a child with no token", () => {
        root.addNewChildWithToken(NodeType.BITMAP);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
    });
    describe("ASTNode.getDescendents()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a null type", () => {
        root.addChildren([copyright, version]);
        assert.deepEqual(root.getDescendents(null), []);
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.getDescendents(NodeType.BITMAP), []);
      });
      it("can handle no matching children", () => {
        root.addChildren([bitmap, copyright]);
        assert.deepEqual(root.getDescendents(NodeType.VERSION), []);
      });
      it("can get descendents (one matching child)", () => {
        root.addChildren([bitmap, copyright, version]);
        assert.deepEqual(root.getDescendents(NodeType.VERSION), [version]);
      });
      it("can get descendents (two matching descendents)", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        bitmap.addChild(version1);
        root.addChildren([bitmap, copyright, version2]);
        assert.deepEqual(root.getDescendents(NodeType.VERSION), [version1, version2]);
      });
      it("can get descendents (three matching descendents)", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        const version3 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '3'));
        bitmap.addChild(version1);
        root.addChildren([bitmap, version2, copyright, version3]);
        assert.deepEqual(root.getDescendents(NodeType.VERSION), [version1, version2, version3]);
      });
    });
    describe("ASTNode.hasChild()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle if there are no children", () => {
        assert.isFalse(root.hasChild());
      });
      it("can check if there is at least one child (one child)", () => {
        root.addChild(bitmap);
        assert.isTrue(root.hasChild());
      });
      it("can check if there is at least one child (two children)", () => {
        root.addChildren([bitmap, copyright]);
        assert.isTrue(root.hasChild());
      });
    });
    describe("ASTNode.hasChildOfType()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a null type", () => {
        root.addChildren([copyright, version]);
        assert.isFalse(root.hasChildOfType(null));
      });
      it("can handle if there are no children", () => {
        assert.isFalse(root.hasChildOfType(NodeType.BITMAP));
      });
      it("can handle if there are non-matching children", () => {
        root.addChildren([copyright, version]);
        assert.isFalse(root.hasChildOfType(NodeType.BITMAP));
      });
      it("can check if there is at least one child of a given type (one match)", () => {
        root.addChildren([bitmap, copyright, version]);
        assert.isTrue(root.hasChildOfType(NodeType.BITMAP));
      });
      it("can check if there is at least one child of a given type (two matching children)", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]);
        assert.isTrue(root.hasChildOfType(NodeType.VERSION));
      });
    });
    describe("ASTNode.hasSoleChildOfType()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a null type", () => {
        root.addChildren([copyright, version]);
        assert.isFalse(root.hasSoleChildOfType(null));
      });
      it("can handle if there are no children", () => {
        assert.isFalse(root.hasSoleChildOfType(NodeType.BITMAP));
      });
      it("can handle if there are non-matching children", () => {
        root.addChildren([copyright, version]);
        assert.isFalse(root.hasSoleChildOfType(NodeType.BITMAP));
      });
      it("can check if there is one child of a given type", () => {
        root.addChildren([bitmap, copyright, version]);
        assert.isTrue(root.hasSoleChildOfType(NodeType.BITMAP));
      });
      it("can handle if there are two children of a given type", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]);
        assert.isFalse(root.hasSoleChildOfType(NodeType.VERSION));
      });
    });
    describe("ASTNode.getText()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle when there is no token", () => {
        assert.equal(root.getText(), '');
      });
      it("can get token text", () => {
        const token = new Token(TokenType.BITMAP, 'bitmap');
        root = new ASTNode(NodeType.BITMAP, token);
        assert.equal(root.getText(), 'bitmap');
      });
    });
    describe("ASTNode.getTextOfType()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a null type", () => {
        root.addChildren([copyright, version]);
        assert.equal(root.getTextOfType(null), '');
      });
      it("can handle when there are no children", () => {
        assert.equal(root.getTextOfType(NodeType.BITMAP), '');
      });
      it("can handle when there are only non-matching children", () => {
        root.addChildren([copyright, version]);
        assert.equal(root.getTextOfType(NodeType.BITMAP), '');
      });
      it("can handle when there is a sole matching child without a token", () => {
        root.addChildren([copyright, version]); // extra children
        root.addChild(bitmap);
        assert.equal(root.getTextOfType(NodeType.BITMAP), '');
      });
      it("can get token text of sole matching child with a token", () => {
        root.addChildren([copyright, version]); // extra children
        const token = new Token(TokenType.BITMAP, 'bitmap');
        root.addNewChildWithToken(NodeType.BITMAP, token);
        assert.equal(root.getTextOfType(NodeType.BITMAP), 'bitmap');
      });
      it("can handle when there are two matching children", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]); // including extra children
        assert.equal(root.getTextOfType(NodeType.VERSION), '');
      });
    });
    describe("ASTNode.getSoleChild()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle when there are no children", () => {
        assert.isNull(root.getSoleChild());
        assert.equal(root.toString(), '[TMP]');
      });
      it("can get sole child when there is one child", () => {
        root.addChild(bitmap);
        assert.equal(root.getSoleChild(), bitmap);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
      it("can handle when there are two children", () => {
        root.addChildren([bitmap, copyright]);
        assert.isNull(root.getSoleChild());
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
    });
    describe("ASTNode.getSoleChildOfType()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a null type", () => {
        root.addChildren([copyright, version]);
        assert.isNull(root.getSoleChildOfType(null));
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can handle when there are no children", () => {
        assert.isNull(root.getSoleChildOfType(NodeType.BITMAP));
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle when there is no matching child", () => {
        root.addChildren([copyright, version]); // extra children
        assert.isNull(root.getSoleChildOfType(NodeType.BITMAP));
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can get sole child of type when there is one matching child", () => {
        root.addChildren([bitmap, copyright, version]); // extra children
        assert.equal(root.getSoleChildOfType(NodeType.VERSION), version);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION]}]');
      });
      it("can handle when there are two matching children", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]); // including extra children
        assert.isNull(root.getSoleChildOfType(NodeType.VERSION));
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION,[VERSION,1]],[VERSION,[VERSION,2]]}]');
      });
    });
    describe("ASTNode.getChildren()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.getChildren(), []);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can get children when there is only one child", () => {
        root.addChild(bitmap);
        assert.deepEqual(root.getChildren(), [bitmap]);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
      it("can get children when there are two", () => {
        root.addChildren([bitmap, copyright]);
        assert.deepEqual(root.getChildren(), [bitmap, copyright]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
    });
    describe("ASTNode.getChildrenOfType()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a null type", () => {
        root.addChildren([copyright, version]);
        assert.deepEqual(root.getChildrenOfType(null), []);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.getChildrenOfType(NodeType.BITMAP), []);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle when there is no matching child", () => {
        root.addChildren([copyright, version]); // extra children
        assert.deepEqual(root.getChildrenOfType(NodeType.BITMAP), []);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can get children of type when there is one matching child", () => {
        root.addChildren([bitmap, copyright, version]); // extra children
        assert.deepEqual(root.getChildrenOfType(NodeType.VERSION), [version]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION]}]');
      });
      it("can get children of type when there are two matching children", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]); // including extra children
        assert.deepEqual(root.getChildrenOfType(NodeType.VERSION), [version1, version2]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION,[VERSION,1]],[VERSION,[VERSION,2]]}]');
      });
    });
    describe("ASTNode.removeChildren()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.removeChildren(), []);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can remove children when there is only one child", () => {
        root.addChild(bitmap);
        assert.deepEqual(root.removeChildren(), [bitmap]);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can remove children when there are two", () => {
        root.addChildren([bitmap, copyright]);
        assert.deepEqual(root.removeChildren(), [bitmap, copyright]);
        assert.equal(root.toString(), '[TMP]');
      });
    });
    describe("ASTNode.removeFirstChild()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.removeFirstChild(), null);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can remove the first child when there is only one child", () => {
        root.addChild(bitmap);
        assert.deepEqual(root.removeFirstChild(), bitmap);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can remove the first child when there are two", () => {
        root.addChildren([bitmap, copyright]);
        assert.deepEqual(root.removeFirstChild(), bitmap);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT]}]');
      });
      it("can remove the first child when there are three", () => {
        root.addChildren([bitmap, copyright, version]);
        assert.deepEqual(root.removeFirstChild(), bitmap);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
    });
    describe("ASTNode.removeSoleChildOfType()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a null type", () => {
        root.addChildren([copyright, version]);
        assert.deepEqual(root.removeSoleChildOfType(null), null);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can handle when there are no children", () => {
        assert.isNull(root.removeSoleChildOfType(NodeType.BITMAP));
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle when there is no matching child", () => {
        root.addChildren([copyright, version]); // extra children
        assert.isNull(root.removeSoleChildOfType(NodeType.BITMAP));
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can get sole child of type when there is one matching child", () => {
        root.addChildren([bitmap, version, copyright]); // extra children
        assert.equal(root.removeSoleChildOfType(NodeType.VERSION), version);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
      it("can handle when there are two matching children", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        root.addChildren([bitmap, version1, copyright, version2]); // including extra children
        assert.isNull(root.removeSoleChildOfType(NodeType.VERSION));
        assert.equal(root.toString(), '[TMP,{[BITMAP],[VERSION,[VERSION,1]],[COPYRIGHT],[VERSION,[VERSION,2]]}]');
      });
    });
    describe("ASTNode.removeChildrenOfType()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a null type", () => {
        root.addChildren([copyright, version]);
        assert.deepEqual(root.removeChildrenOfType(null), []);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.removeChildrenOfType(NodeType.BITMAP), []);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle when there is no matching child", () => {
        root.addChildren([copyright, version]); // extra children
        assert.deepEqual(root.removeChildrenOfType(NodeType.BITMAP), []);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can get children of type when there is one matching child", () => {
        root.addChildren([bitmap, version, copyright]); // extra children
        assert.deepEqual(root.removeChildrenOfType(NodeType.VERSION), [version]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
      it("can get children of type when there are two matching children", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        root.addChildren([bitmap, version1, copyright, version2]); // including extra children
        assert.deepEqual(root.removeChildrenOfType(NodeType.VERSION), [version1, version2]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
    });
    describe("ASTNode.removeChildrenOfTypes()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle null type array", () => {
        root.addChildren([copyright, version]); // extra children
        assert.deepEqual(root.removeChildrenOfTypes(null), []);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can handle empty type array", () => {
        root.addChildren([copyright, version]); // extra children
        assert.deepEqual(root.removeChildrenOfTypes([]), []);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.removeChildrenOfTypes([NodeType.BITMAP]), []);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle when there is no matching child", () => {
        root.addChildren([copyright, version]); // extra children
        assert.deepEqual(root.removeChildrenOfTypes([NodeType.BITMAP]), []);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can get children of type when there is one matching child", () => {
        root.addChildren([bitmap, version, copyright]); // extra children
        assert.deepEqual(root.removeChildrenOfTypes([NodeType.VERSION]), [version]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
      it("can get children of type when there are two matching children (of one type)", () => {
        const version1 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2 = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        root.addChildren([bitmap, version1, copyright, version2]); // including extra children
        assert.deepEqual(root.removeChildrenOfTypes([NodeType.VERSION]), [version1, version2]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
      it("can get children of types when there are two matching children (of different type)", () => {
        root.addChildren([bitmap, version, copyright]); // including extra children
        assert.deepEqual(root.removeChildrenOfTypes([NodeType.VERSION,NodeType.COPYRIGHT]), [version, copyright]);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
      it("can get children of types when there are two matching children of each of two different type", () => {
        const version1   = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '1'));
        const version2   = new ASTNode(NodeType.VERSION, new Token(TokenType.VERSION, '2'));
        const copyright1 = new ASTNode(NodeType.COPYRIGHT, new Token(TokenType.COPYRIGHT, '1'));
        const copyright2 = new ASTNode(NodeType.COPYRIGHT, new Token(TokenType.COPYRIGHT, '2'));
        root.addChildren([version1, copyright1, bitmap, copyright2, version2]); // including extra children
        assert.deepEqual(root.removeChildrenOfTypes([NodeType.VERSION,NodeType.COPYRIGHT]), [version1, copyright1, copyright2, version2]);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
    });
    describe("ASTNode.removeBlocks()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle null parentType", () => {
        const group1      = new ASTNode(NodeType.GROUP, new Token(TokenType.GROUP, '1'));
        const production1 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '1'));
        root.addChildren([group1, production1]);
        assert.deepEqual(root.removeBlocks(null, NodeType.PRODUCTION), []);
        assert.equal(root.toString(), '[TMP,{[GROUP,[GROUP,1]],[PRODUCTION,[MATCH,1]]}]');
      });
      it("can handle null childType", () => {
        const group1      = new ASTNode(NodeType.GROUP, new Token(TokenType.GROUP, '1'));
        const production1 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '1'));
        root.addChildren([group1, production1]);
        assert.deepEqual(root.removeBlocks(NodeType.GROUP, null), []);
        assert.equal(root.toString(), '[TMP,{[GROUP,[GROUP,1]],[PRODUCTION,[MATCH,1]]}]');
      });
      it("can handle both parentType and childType null", () => {
        const group1      = new ASTNode(NodeType.GROUP, new Token(TokenType.GROUP, '1'));
        const production1 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '1'));
        root.addChildren([group1, production1]);
        assert.deepEqual(root.removeBlocks(null, null), []);
        assert.equal(root.toString(), '[TMP,{[GROUP,[GROUP,1]],[PRODUCTION,[MATCH,1]]}]');
      });
      it("can handle a single parent with one childType", () => {
        const group1      = new ASTNode(NodeType.GROUP, new Token(TokenType.GROUP, '1'));
        const production1 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '1'));
        root.addChildren([bitmap, group1, copyright, production1, version]);
        assert.deepEqual(root.removeBlocks(NodeType.GROUP, NodeType.PRODUCTION), [group1]);
        assert.equal(group1.toString(), '[GROUP,[GROUP,1],{[PRODUCTION,[MATCH,1]]}]');
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION]}]');
      });
      it("can handle a two parents with one childType", () => {
        const group1      = new ASTNode(NodeType.GROUP, new Token(TokenType.GROUP, '1'));
        const production1 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '1'));
        const group2      = new ASTNode(NodeType.GROUP, new Token(TokenType.GROUP, '2'));
        const production2 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '2'));
        root.addChildren([bitmap, group1, production1, copyright, group2, version, production2]);
        assert.deepEqual(root.removeBlocks(NodeType.GROUP, NodeType.PRODUCTION), [group1, group2]);
        assert.equal(group1.toString(), '[GROUP,[GROUP,1],{[PRODUCTION,[MATCH,1]]}]');
        assert.equal(group2.toString(), '[GROUP,[GROUP,2],{[PRODUCTION,[MATCH,2]]}]');
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION]}]');
      });
      it("can handle a two parents with several childType nodes", () => {
        const group1      = new ASTNode(NodeType.GROUP, new Token(TokenType.GROUP, '1'));
        const production1 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '1'));
        const production2 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '2'));
        const production3 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '3'));
        const group2      = new ASTNode(NodeType.GROUP, new Token(TokenType.GROUP, '2'));
        const production4 = new ASTNode(NodeType.PRODUCTION, new Token(TokenType.MATCH, '4'));
        root.addChildren([group1, bitmap, production1, production2, copyright, production3, version, group2, production4]);
        assert.deepEqual(root.removeBlocks(NodeType.GROUP, NodeType.PRODUCTION), [group1, group2]);
        assert.equal(group1.toString(), '[GROUP,[GROUP,1],{[PRODUCTION,[MATCH,1]],[PRODUCTION,[MATCH,2]],[PRODUCTION,[MATCH,3]]}]');
        assert.equal(group2.toString(), '[GROUP,[GROUP,2],{[PRODUCTION,[MATCH,4]]}]');
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION]}]');
      });
    });
    describe("ASTNode.toString()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a simple node", () => {
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle a node with a token", () => {
        const token = new Token(TokenType.BITMAP, 'bitmap');
        const root  = new ASTNode(NodeType.TMP, token);
        assert.equal(root.toString(), '[TMP,[BITMAP,bitmap]]');
      });
      it("can handle a node with a single child", () => {
        root.addChild(bitmap);
        assert.equal(root.toString(), '[TMP,{[BITMAP]}]');
      });
      it("can handle a node with two children", () => {
        root.addChildren([bitmap, copyright]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
    });
    describe("ASTNode.toText()", () => {
      beforeEach(() => {
        initVariables();
      });
      it("can handle a node without a SOURCE_CODE child (no children)", () => {
        assert.equal(root.toText(), '');
      });
      it("can handle a node without a SOURCE_CODE child (two children)", () => {
        root.addChildren([bitmap, copyright]);
        assert.equal(root.toText(), '');
      });
      it("can extract source code", () => {
        const line1  = new ASTNode(NodeType.LINE, new Token(TokenType.NEWLINE, '\n', 1, 1, 'line1\n'));
        const line2  = new ASTNode(NodeType.LINE, new Token(TokenType.NEWLINE, '\n', 2, 1, 'line2\n'));
        const line3  = new ASTNode(NodeType.LINE, new Token(TokenType.NEWLINE, '\n', 3, 1, 'line3\n'));
        const source = new ASTNode(NodeType.SOURCE_CODE);
        source.addChildren([line1, line2, line3]);
        root.addChild(source);
        assert.equal(root.toText(), 'line1\nline2\nline3\n');
      });
    });
  });
});
