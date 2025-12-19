/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-12-05
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/ASTRebuild)
 */

import 'mocha';
import { assert } from 'chai';
import { NodeType } from '../../src/ng-compiler/node-type.js';
import { ChangeNode, FirstNode, GivenNode, NewNode, NewNodeOrTree, StackedPair } from '../../src/ng-compiler/ast-rebuild.js';
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { Token } from '../../src/ng-compiler/lexer.js';
import { TokenType } from '../../src/ng-compiler/token-type.js';

describe("ASTRebuild Tests", () => {
  describe("GivenNode", () => {
    it("can construct a GivenNode", () => {
      const givenNode = new GivenNode(NodeType.GROUP);
      assert.isNotNull(givenNode);
      assert.equal(givenNode['nodeType'], NodeType.GROUP);
    });
    it("can rebuild the AST", () => {
      const root: ASTNode = new ASTNode();
      root.addChildren([
        new ASTNode(NodeType.GROUP),
        new ASTNode(NodeType.PRODUCTION),
        new ASTNode(NodeType.PRODUCTION),
      ]);
      const givenNode = new GivenNode(NodeType.GROUP);
      givenNode.apply(root);
      const parent = root.getSoleChildOfType(NodeType.GROUP);
      assert.isNotNull(parent);
      assert.equal(parent.numberOfChildren(), 2);
      assert.equal(parent.numberOfChildrenOfType(NodeType.PRODUCTION), 2);
    });
  });
  describe("StackedPair", () => {
    it("can construct a StackedPair", () => {
      const stackedPair = new StackedPair(NodeType.STORE, NodeType.STORENAME);
      assert.isNotNull(stackedPair);
      assert.equal(stackedPair['parentType'], NodeType.STORE);
      assert.equal(stackedPair['childType'],  NodeType.STORENAME);
    });
    it("can rebuild the AST", () => {
      const root: ASTNode = new ASTNode();
      root.addChildren([
        new ASTNode(NodeType.STORE),
        new ASTNode(NodeType.STORENAME)
      ]);
      const stackedPair = new StackedPair(NodeType.STORE, NodeType.STORENAME);
      stackedPair.apply(root);
      const parent = root.getSoleChildOfType(NodeType.STORE);
      assert.isNotNull(parent);
      assert.isNotNull(parent.getSoleChildOfType(NodeType.STORENAME));
    });
  });
  describe("NewNode", () => {
    it("can construct a NewNode", () => {
      const newNode = new NewNode(NodeType.GROUP);
      assert.isNotNull(newNode);
      assert.equal(newNode['nodeType'], NodeType.GROUP);
    });
    it("can rebuild the AST", () => {
      const root: ASTNode = new ASTNode();
      root.addChildren([
        new ASTNode(NodeType.PRODUCTION),
        new ASTNode(NodeType.PRODUCTION),
      ]);
      const newNode = new NewNode(NodeType.GROUP);
      newNode.apply(root);
      const parent = root.getSoleChildOfType(NodeType.GROUP);
      assert.isNotNull(parent);
      assert.equal(parent.numberOfChildren(), 2);
      assert.equal(parent.numberOfChildrenOfType(NodeType.PRODUCTION), 2);
    });
  });
  describe("NewNodeOrTree", () => {
    it("can construct a NewNodeOrTree", () => {
      const newNodeOrTree = new NewNodeOrTree(NodeType.STORE);
      assert.isNotNull(newNodeOrTree);
      assert.equal(newNodeOrTree['nodeType'], NodeType.STORE);
    });
    it("can rebuild the AST (one child)", () => {
      const root: ASTNode = new ASTNode();
      const token: Token  = new Token(TokenType.OCTAL, '1');
      root.addNewChildWithToken(NodeType.OCTAL, token);
      const newNodeOrTree = new NewNodeOrTree(NodeType.STORENAME);
      newNodeOrTree.apply(root);
      const storeName = root.getSoleChildOfType(NodeType.STORENAME);
      assert.isNotNull(storeName);
      assert.equal(storeName.getText(), '1');
      assert.isFalse(storeName.hasChildren());
    });
    it("can rebuild the AST (two children)", () => {
      const root: ASTNode = new ASTNode();
      const tokenOne: Token  = new Token(TokenType.OCTAL, '1');
      const tokenTwo: Token  = new Token(TokenType.OCTAL, '2');
      root.addNewChildWithToken(NodeType.OCTAL, tokenOne);
      root.addNewChildWithToken(NodeType.OCTAL, tokenTwo);
      const newNodeOrTree = new NewNodeOrTree(NodeType.STORENAME);
      newNodeOrTree.apply(root);
      const storeName = root.getSoleChildOfType(NodeType.STORENAME);
      assert.isNotNull(storeName);
      assert.equal(storeName.numberOfChildren(), 2);
      assert.equal(storeName.numberOfChildrenOfType(NodeType.OCTAL), 2);
      const children = storeName.getChildren();
      assert.equal(children[0].getText(), '1');
      assert.equal(children[1].getText(), '2');
    });
  });
  describe("FirstNode", () => {
    it("can construct a FirstNode", () => {
      const firstNode = new FirstNode();
      assert.isNotNull(firstNode);
    });
    it("can rebuild the AST", () => {
      const root: ASTNode = new ASTNode();
      root.addChildren([
        new ASTNode(NodeType.GROUP),
        new ASTNode(NodeType.PRODUCTION),
        new ASTNode(NodeType.PRODUCTION),
      ]);
      const firstNode = new FirstNode();
      firstNode.apply(root);
      const parent = root.getSoleChildOfType(NodeType.GROUP);
      assert.isNotNull(parent);
      assert.equal(parent.numberOfChildren(), 2);
      assert.equal(parent.numberOfChildrenOfType(NodeType.PRODUCTION), 2);
    });
  });
  describe("ChangeNode", () => {
    it("can construct a ChangeNode", () => {
      const changeNode = new ChangeNode(NodeType.OFFSET);
      assert.isNotNull(changeNode);
      assert.equal(changeNode['nodeType'], NodeType.OFFSET);
    });
    it("can rebuild the AST", () => {
      const root: ASTNode = new ASTNode();
      const token = new Token(TokenType.OCTAL, '1');
      root.addChild(new ASTNode(NodeType.OCTAL, token));
      const changeNode = new ChangeNode(NodeType.OFFSET);
      changeNode.apply(root);
      const offsetNode = root.getSoleChildOfType(NodeType.OFFSET);
      assert.isNotNull(offsetNode)
      assert.deepEqual(offsetNode.token, token);
    });
  });
});
