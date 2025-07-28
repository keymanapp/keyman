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

function init_variables(): void {
  root      = new ASTNode(NodeTypes.TMP);
  bitmap    = new ASTNode(NodeTypes.BITMAP);
  copyright = new ASTNode(NodeTypes.COPYRIGHT);
  version   = new ASTNode(NodeTypes.VERSION);
}

describe("Tree Construction Tests", () => {
  describe("ASTNode Tests", () => {
    describe("ASTNode.constructor()", () => {
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
    });
    describe("ASTNode.addChild()", () => {
      beforeEach(() => {
        init_variables();
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
    });
    describe("ASTNode.addChildren()", () => {
      beforeEach(() => {
        init_variables();
      });
      it("can add two children", () => {
        root.addChildren([bitmap, copyright]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT]}]');
      });
      it("can handle adding empty list of children", () => {
        root.addChildren([]);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle adding a null", () => {
        root.addChildren(null);
        assert.equal(root.toString(), '[TMP]');
      });
    });
    describe("ASTNode.addNewChildWithToken()", () => {
      beforeEach(() => {
        init_variables();
      });
      it("can construct and add a child with a token", () => {
        const token = new Token(TokenTypes.BITMAP, 'bitmap');
        root.addNewChildWithToken(NodeTypes.BITMAP, token);
        assert.equal(root.toString(), '[TMP,{[BITMAP,[BITMAP,bitmap]]}]');
      });
    });
    describe("ASTNode.getDescendents()", () => {
      beforeEach(() => {
        init_variables();
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.getDescendents(NodeTypes.BITMAP), []);
      });
      it("can handle no matching children", () => {
        root.addChildren([bitmap, copyright]);
        assert.deepEqual(root.getDescendents(NodeTypes.VERSION), []);
      });
      it("can get descendents (one matching child)", () => {
        root.addChildren([bitmap, copyright, version]);
        assert.deepEqual(root.getDescendents(NodeTypes.VERSION), [version]);
      });
      it("can get descendents (two matching descendents)", () => {
        const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
        const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
        bitmap.addChild(version1);
        root.addChildren([bitmap, copyright, version2]);
        assert.deepEqual(root.getDescendents(NodeTypes.VERSION), [version1, version2]);
      });
    });
    describe("ASTNode.hasChild()", () => {
      beforeEach(() => {
        init_variables();
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
        init_variables();
      });
      it("can handle a null type", () => {
        assert.isFalse(root.hasChildOfType(null));
      });
      it("can handle if there are no children", () => {
        assert.isFalse(root.hasChildOfType(NodeTypes.BITMAP));
      });
      it("can handle if there are non-matching children", () => {
        root.addChildren([copyright, version]);
        assert.isFalse(root.hasChildOfType(NodeTypes.BITMAP));
      });
      it("can check if there is at least one child of a given type (one match)", () => {
        root.addChildren([bitmap, copyright, version]);
        assert.isTrue(root.hasChildOfType(NodeTypes.BITMAP));
      });
      it("can check if there is at least one child of a given type (two matching children)", () => {
        const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
        const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]);
        assert.isTrue(root.hasChildOfType(NodeTypes.VERSION));
      });
    });
    describe("ASTNode.hasSoloChildOfType()", () => {
      beforeEach(() => {
        init_variables();
      });
      it("can handle a null type", () => {
        assert.isFalse(root.hasSoloChildOfType(null));
      });
      it("can handle if there are no children", () => {
        assert.isFalse(root.hasSoloChildOfType(NodeTypes.BITMAP));
      });
      it("can handle if there are non-matching children", () => {
        root.addChildren([copyright, version]);
        assert.isFalse(root.hasSoloChildOfType(NodeTypes.BITMAP));
      });
      it("can check if there is one child of a given type", () => {
        root.addChildren([bitmap, copyright, version]);
        assert.isTrue(root.hasSoloChildOfType(NodeTypes.BITMAP));
      });
      it("can handle if there are two children of a given type", () => {
        const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
        const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]);
        assert.isFalse(root.hasSoloChildOfType(NodeTypes.VERSION));
      });
    });
    describe("ASTNode.getText()", () => {
      beforeEach(() => {
        init_variables();
      });
      it("can handle when there is no token", () => {
        assert.equal(root.getText(), '');
      });
      it("can get token text", () => {
        const token = new Token(TokenTypes.BITMAP, 'bitmap');
        root = new ASTNode(NodeTypes.BITMAP, token);
        assert.equal(root.getText(), 'bitmap');
      });
    });
    describe("ASTNode.getTextOfType()", () => {
      beforeEach(() => {
        init_variables();
      });
      it("can handle when there are no children", () => {
        assert.equal(root.getTextOfType(NodeTypes.BITMAP), '');
      });
      it("can handle when there are only non-matching children", () => {
        root.addChildren([copyright, version]);
        assert.equal(root.getTextOfType(NodeTypes.BITMAP), '');
      });
      it("can handle when there is a sole matching child without a token", () => {
        root.addChildren([copyright, version]); // extra children
        root.addChild(bitmap);
        assert.equal(root.getTextOfType(NodeTypes.BITMAP), '');
      });
      it("can get token text of sole matching child with a token", () => {
        root.addChildren([copyright, version]); // extra children
        const token = new Token(TokenTypes.BITMAP, 'bitmap');
        root.addNewChildWithToken(NodeTypes.BITMAP, token);
        assert.equal(root.getTextOfType(NodeTypes.BITMAP), 'bitmap');
      });
      it("can handle when there are two matching children", () => {
        const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
        const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]); // including extra children
        assert.equal(root.getTextOfType(NodeTypes.VERSION), '');
      });
    });
    describe("ASTNode.getSoleChild()", () => {
      beforeEach(() => {
        init_variables();
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
        init_variables();
      });
      it("can handle when there are no children", () => {
        assert.isNull(root.getSoleChildOfType(NodeTypes.BITMAP));
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle when there is no matching child", () => {
        root.addChildren([copyright, version]); // extra children
        assert.isNull(root.getSoleChildOfType(NodeTypes.BITMAP));
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can get sole child of type when there is one matching child", () => {
        root.addChildren([bitmap, copyright, version]); // extra children
        assert.equal(root.getSoleChildOfType(NodeTypes.VERSION), version);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION]}]');
      });
      it("can handle when there are two matching children", () => {
        const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
        const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]); // including extra children
        assert.isNull(root.getSoleChildOfType(NodeTypes.VERSION));
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION,[VERSION,1]],[VERSION,[VERSION,2]]}]');
      });
    });
    describe("ASTNode.getChildren()", () => {
      beforeEach(() => {
        init_variables();
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
        init_variables();
      });
      it("can handle when there are no children", () => {
        assert.deepEqual(root.getChildrenOfType(NodeTypes.BITMAP), []);
        assert.equal(root.toString(), '[TMP]');
      });
      it("can handle when there is no matching child", () => {
        root.addChildren([copyright, version]); // extra children
        assert.deepEqual(root.getChildrenOfType(NodeTypes.BITMAP), []);
        assert.equal(root.toString(), '[TMP,{[COPYRIGHT],[VERSION]}]');
      });
      it("can get children of type when there is one matching child", () => {
        root.addChildren([bitmap, copyright, version]); // extra children
        assert.deepEqual(root.getChildrenOfType(NodeTypes.VERSION), [version]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION]}]');
      });
      it("can get children of type when there are two matching children", () => {
        const version1 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '1'));
        const version2 = new ASTNode(NodeTypes.VERSION, new Token(TokenTypes.VERSION, '2'));
        root.addChildren([bitmap, copyright, version1, version2]); // including extra children
        assert.deepEqual(root.getChildrenOfType(NodeTypes.VERSION), [version1, version2]);
        assert.equal(root.toString(), '[TMP,{[BITMAP],[COPYRIGHT],[VERSION,[VERSION,1]],[VERSION,[VERSION,2]]}]');
      });
    });
    describe("ASTNode.removeChildren()", () => {
      beforeEach(() => {
        init_variables();
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
      it("can removet children when there are two", () => {
        root.addChildren([bitmap, copyright]);
        assert.deepEqual(root.removeChildren(), [bitmap, copyright]);
        assert.equal(root.toString(), '[TMP]');
      });
    });
  });
});
