/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-07-02
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Statement Analyser)
 *
 * Statement Rule Tests
 */

import 'mocha';
import { assert } from 'chai';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';
import { stringToTokenBuffer } from './kmn-analyser.tests.js';
import { Rule } from '../../src/ng-compiler/recursive-descent.js';
import { AnyStatementRule, BaselayoutStatementRule, CallStatementRule, DeadKeyStatementRule, LayerStatementRule, NotAnyStatementRule, PlatformStatementRule, SaveStatementRule } from '../../src/ng-compiler/statement-analyser.js';

let root: ASTNode = null;

describe("KMN Statement Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeTypes.TMP);
  });
  describe("AnyStatementRule Tests", () => {
    it("can construct an AnyStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const anyStatement: Rule = new AnyStatementRule();
      assert.isNotNull(anyStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(digit)');
      const anyStatement: Rule = new AnyStatementRule();
      assert.isTrue(anyStatement.parse(root));
      const anyNode = root.getSoleChildOfType(NodeTypes.ANY);
      assert.isNotNull(anyNode);
      assert.isNotNull(anyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("CallStatementRule Tests", () => {
    it("can construct an CallStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const callStatement: Rule = new CallStatementRule();
      assert.isNotNull(callStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('call(callDefinitionStore)');
      const callStatement: Rule = new CallStatementRule();
      assert.isTrue(callStatement.parse(root));
      const callNode = root.getSoleChildOfType(NodeTypes.CALL);
      assert.isNotNull(callNode);
      assert.isNotNull(callNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("DeadKeyStatementRule Tests", () => {
    it("can construct an DeadKeyStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const deadKeyStatement: Rule = new DeadKeyStatementRule();
      assert.isNotNull(deadKeyStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('dk(storeName)');
      const deadKeyStatement: Rule = new DeadKeyStatementRule();
      assert.isTrue(deadKeyStatement.parse(root));
      const deadKeyNode = root.getSoleChildOfType(NodeTypes.DEADKEY);
      assert.isNotNull(deadKeyNode);
      assert.isNotNull(deadKeyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("NotAnyStatementRule Tests", () => {
    it("can construct an NotAnyStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const notAnyStatement: Rule = new NotAnyStatementRule();
      assert.isNotNull(notAnyStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('notany(digit)');
      const notAnyStatement: Rule = new NotAnyStatementRule();
      assert.isTrue(notAnyStatement.parse(root));
      const notAnyNode = root.getSoleChildOfType(NodeTypes.NOTANY);
      assert.isNotNull(notAnyNode);
      assert.isNotNull(notAnyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("SaveStatementRule Tests", () => {
    it("can construct an SaveStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const saveStatement: Rule = new SaveStatementRule();
      assert.isNotNull(saveStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('save(storeName)');
      const saveStatement: Rule = new SaveStatementRule();
      assert.isTrue(saveStatement.parse(root));
      const saveNode = root.getSoleChildOfType(NodeTypes.SAVE);
      assert.isNotNull(saveNode);
      assert.isNotNull(saveNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("BaselayoutStatementRule Tests", () => {
    it("can construct an BaselayoutStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const baselayoutStatement: Rule = new BaselayoutStatementRule();
      assert.isNotNull(baselayoutStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('baselayout("en-US")');
      const baselayoutStatement: Rule = new BaselayoutStatementRule();
      assert.isTrue(baselayoutStatement.parse(root));
      const baselayoutNode = root.getSoleChildOfType(NodeTypes.BASELAYOUT_SHORTCUT);
      assert.isNotNull(baselayoutNode);
      const stringNode = baselayoutNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"en-US"');
    });
  });
  describe("LayerStatementRule Tests", () => {
    it("can construct an LayerStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isNotNull(layerStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer("shift")');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (spacve before name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer( "shift")');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (space after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer("shift" )');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (space before and after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer( "shift" )');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
  });
  describe("PlatformStatementRule Tests", () => {
    it("can construct an PlatformStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const platformStatement: Rule = new PlatformStatementRule();
      assert.isNotNull(platformStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch")');
      const platformStatement: Rule = new PlatformStatementRule();
      assert.isTrue(platformStatement.parse(root));
      const platformNode = root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT);
      assert.isNotNull(platformNode);
      const stringNode = platformNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"touch"');
    });
  });
});