/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-12-05
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/ASTStrategy)
 */

import 'mocha';
import { assert } from 'chai';
import { NodeType } from '../../src/ng-compiler/node-type.js';
import { FirstNode, GivenNode, NewNode, NewNodeOrTree, StackedPair } from '../../src/ng-compiler/ast-strategy.js';

describe("ASTStrategy Tests", () => {
  describe("GivenNode", () => {
    it("can construct a GivenNode", () => {
      const givenNode = new GivenNode(NodeType.STORE);
      assert.isNotNull(givenNode);
      assert.equal(givenNode['nodeType'], NodeType.STORE);
    });
  });
  describe("StackedPair", () => {
    it("can construct a StackedPair", () => {
      const stackedPair = new StackedPair(NodeType.STORE, NodeType.STORENAME);
      assert.isNotNull(stackedPair);
      assert.equal(stackedPair['parentType'], NodeType.STORE);
      assert.equal(stackedPair['childType'],  NodeType.STORENAME);
    });
  });
  describe("NewNode", () => {
    it("can construct a NewNode", () => {
      const newNode = new NewNode(NodeType.STORE);
      assert.isNotNull(newNode);
      assert.equal(newNode['nodeType'], NodeType.STORE);
    });
  });
  describe("NewNodeOrTree", () => {
    it("can construct a NewNodeOrTree", () => {
      const newNodeOrTree = new NewNodeOrTree(NodeType.STORE);
      assert.isNotNull(newNodeOrTree);
      assert.equal(newNodeOrTree['nodeType'], NodeType.STORE);
    });
  });
  describe("FirstNode", () => {
    it("can construct a FirstNode", () => {
      const firstNode = new FirstNode();
      assert.isNotNull(firstNode);

    });
  });
});
