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
import { NodeType } from "../../src/ng-compiler/node-type.js";
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { stringToTokenBuffer } from './kmn-analyzer.tests.js';
import { Rule, TokenRule } from '../../src/ng-compiler/recursive-descent.js';
import { AnyStatementRule, BaselayoutStatementRule, CallStatementRule, ComparisonRule, ContextStatementRule } from '../../src/ng-compiler/statement-analyzer.js';
import { DeadkeyStatementRule, IfLikeStatementRule, IfNormalStoreStatementRule, IfStatementRule, IfSystemStoreStatementRule } from '../../src/ng-compiler/statement-analyzer.js';
import { IndexStatementRule, LayerStatementRule, NotanyStatementRule, OutsStatementRule, PlatformStatementRule } from '../../src/ng-compiler/statement-analyzer.js';
import { SaveStatementRule, SystemStoreNameForIfRule } from '../../src/ng-compiler/statement-analyzer.js';
import { TokenType } from '../../src/ng-compiler/token-type.js';

let root: ASTNode = null;

describe("KMN Statement Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeType.TMP);
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
      const anyNode = root.getSoleChildOfType(NodeType.ANY);
      assert.isNotNull(anyNode);
      assert.isNotNull(anyNode.getSoleChildOfType(NodeType.STORENAME));
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
      const callNode = root.getSoleChildOfType(NodeType.CALL);
      assert.isNotNull(callNode);
      assert.isNotNull(callNode.getSoleChildOfType(NodeType.STORENAME));
    });
  });
  describe("DeadkeyStatementRule Tests", () => {
    it("can construct an DeadkeyStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const deadKeyStatement: Rule = new DeadkeyStatementRule();
      assert.isNotNull(deadKeyStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('dk(storeName)');
      const deadKeyStatement: Rule = new DeadkeyStatementRule();
      assert.isTrue(deadKeyStatement.parse(root));
      const deadKeyNode = root.getSoleChildOfType(NodeType.DEADKEY);
      assert.isNotNull(deadKeyNode);
      assert.isNotNull(deadKeyNode.getSoleChildOfType(NodeType.STORENAME));
    });
  });
  describe("NotanyStatementRule Tests", () => {
    it("can construct an NotanyStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const notAnyStatement: Rule = new NotanyStatementRule();
      assert.isNotNull(notAnyStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('notany(digit)');
      const notAnyStatement: Rule = new NotanyStatementRule();
      assert.isTrue(notAnyStatement.parse(root));
      const notAnyNode = root.getSoleChildOfType(NodeType.NOTANY);
      assert.isNotNull(notAnyNode);
      assert.isNotNull(notAnyNode.getSoleChildOfType(NodeType.STORENAME));
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
      const saveNode = root.getSoleChildOfType(NodeType.SAVE);
      assert.isNotNull(saveNode);
      assert.isNotNull(saveNode.getSoleChildOfType(NodeType.STORENAME));
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
      const baselayoutNode = root.getSoleChildOfType(NodeType.BASELAYOUT_SHORTCUT);
      assert.isNotNull(baselayoutNode);
      const stringNode = baselayoutNode.getSoleChildOfType(NodeType.STRING)
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
      const layerNode = root.getSoleChildOfType(NodeType.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeType.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (spacve before name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer( "shift")');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeType.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeType.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (space after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer("shift" )');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeType.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeType.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (space before and after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer( "shift" )');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeType.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeType.STRING)
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
      const platformNode = root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT);
      assert.isNotNull(platformNode);
      const stringNode = platformNode.getSoleChildOfType(NodeType.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"touch"');
    });
  });
  describe("IfLikeStatementRule Tests", () => {
    it("can construct a IfLikeStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isNotNull(ifLikeStatement);
    });
    it("can parse correctly (if)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = "1")');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isTrue(ifLikeStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STRING))
    });
    it("can parse correctly (platform)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch")');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isTrue(ifLikeStatement.parse(root));
      const platformNode = root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT);
      assert.isNotNull(platformNode);
      const stringNode = platformNode.getSoleChildOfType(NodeType.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"touch"');
    });
    it("can parse correctly (baselayout)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('baselayout("en-US")');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isTrue(ifLikeStatement.parse(root));
      const baselayoutNode = root.getSoleChildOfType(NodeType.BASELAYOUT_SHORTCUT);
      assert.isNotNull(baselayoutNode);
      const stringNode = baselayoutNode.getSoleChildOfType(NodeType.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"en-US"');
    });
  });
  describe("IfStatementRule Tests", () => {
    it("can construct a IfStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifStatement: Rule = new IfStatementRule();
      assert.isNotNull(ifStatement);
    });
    it("can parse correctly (store, string)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = "1")');
      const ifStatement: Rule = new IfStatementRule();
      assert.isTrue(ifStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STRING))
    });
    it("can parse correctly (system store, string)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap = "filename")');
      const ifStatement: Rule = new IfStatementRule();
      assert.isTrue(ifStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STRING))
    });
  });
  describe("IfNormalStoreStatementRule Tests", () => {
    it("can construct a IfNormalStoreStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifNormalStoreStatement: Rule = new IfNormalStoreStatementRule();
      assert.isNotNull(ifNormalStoreStatement);
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = "1")');
      const ifNormalStoreStatement: Rule = new IfNormalStoreStatementRule();
      assert.isTrue(ifNormalStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STRING))
    });
    it("can parse correctly (not equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number != "1")');
      const ifNormalStoreStatement: Rule = new IfNormalStoreStatementRule();
      assert.isTrue(ifNormalStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.NOT_EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STRING))
    });
    it("can parse correctly (equal, two u_chars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = U+1780 U+1781)');
      const ifNormalStoreStatement: Rule = new IfNormalStoreStatementRule();
      assert.isTrue(ifNormalStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STORENAME))
      assert.equal(ifNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
  });
  describe("IfSystemStoreStatementRule Tests", () => {
    it("can construct a IfSystemStoreStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifSystemStoreStatement: Rule = new IfSystemStoreStatementRule();
      assert.isNotNull(ifSystemStoreStatement);
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap = "filename")');
      const ifSystemStoreStatement: Rule = new IfSystemStoreStatementRule();
      assert.isTrue(ifSystemStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STRING))
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap != "filename")');
      const ifSystemStoreStatement: Rule = new IfSystemStoreStatementRule();
      assert.isTrue(ifSystemStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.NOT_EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.STRING))
    });
    it("can parse correctly (equal, two u_chars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap = U+1780 U+1781)');
      const ifSystemStoreStatement: Rule = new IfSystemStoreStatementRule();
      assert.isTrue(ifSystemStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeType.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeType.BITMAP))
      assert.equal(ifNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
  });
  describe("SystemStoreNameForIfRule Tests", () => {
    it("can construct a SystemStoreNameForIfRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isNotNull(systemStoreNameForIf);
    });
    it("can parse correctly (string system store name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&bitmap');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
    });
    it("can parse correctly (baselayout)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&baselayout');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BASELAYOUT));
    });
    it("can parse correctly (layer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&layer');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.LAYER));
    });
    it("can parse correctly (newLayer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&newLayer');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NEWLAYER));
    });
    it("can parse correctly (oldLayer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&oldLayer');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.OLDLAYER));
    });
    it("can parse correctly (platform)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&platform');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM));
    });
  });
  describe("ComparisonRule Tests", () => {
    it("can construct a ComparisonRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const comparison: Rule = new ComparisonRule();
      assert.isNotNull(comparison);
    });
    it("can parse correctly (=)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('=');
      const comparison: Rule = new ComparisonRule();
      assert.isTrue(comparison.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.EQUAL));
    });
    it("can parse correctly (!=)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('!=');
      const comparison: Rule = new ComparisonRule();
      assert.isTrue(comparison.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NOT_EQUAL));
    });
  });
  describe("ContextStatementRule Tests", () => {
    it("can construct a ContextStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const contextStatement: Rule = new ContextStatementRule();
      assert.isNotNull(contextStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('context(1)');
      const contextStatement: Rule = new ContextStatementRule();
      assert.isTrue(contextStatement.parse(root));
      const contextNode = root.getSoleChildOfType(NodeType.CONTEXT);
      assert.isNotNull(contextNode);
      assert.equal(contextNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("rejects a context without brackets", () => {
      Rule.tokenBuffer = stringToTokenBuffer('context');
      const contextStatement: Rule = new ContextStatementRule();
      assert.isFalse(contextStatement.parse(root));
    });
  });
  describe("IndexStatementRule Tests", () => {
    it("can construct a IndexStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isNotNull(indexStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit,1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (offset > 7)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit,8)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '8');
    });
    it("can parse correctly (space after open)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index( digit,1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (space before close)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit,1 )');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (space after open and before close)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index( digit,1 )');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (space before comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit ,1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (space after comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit, 1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (space before and after comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit , 1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
  });
  describe("CommaRule Tests", () => {
    it("can construct a CommaRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const comma: Rule = new TokenRule(TokenType.COMMA);
      assert.isNotNull(comma);
    });
    it("can parse correctly (comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(',');
      const comma: Rule = new TokenRule(TokenType.COMMA);
      assert.isTrue(comma.parse(root));
    });
    it("can parse correctly (comma, space before)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' ,');
      const comma: Rule = new TokenRule(TokenType.COMMA);
      assert.isTrue(comma.parse(root));
    });
    it("can parse correctly (comma, space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(', ');
      const comma: Rule = new TokenRule(TokenType.COMMA);
      assert.isTrue(comma.parse(root));
    });
    it("can parse correctly (comma, space before and after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' , ');
      const comma: Rule = new TokenRule(TokenType.COMMA);
      assert.isTrue(comma.parse(root));
    });
  });
  describe("OutsStatementRule Tests", () => {
    it("can construct an OutsStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isNotNull(outsStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs(digit)');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isTrue(outsStatement.parse(root));
      const outsNode = root.getSoleChildOfType(NodeType.OUTS);
      assert.isNotNull(outsNode);
      assert.isNotNull(outsNode.getSoleChildOfType(NodeType.STORENAME));
    });
    it("can parse correctly (space before name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs( digit)');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isTrue(outsStatement.parse(root));
      const outsNode = root.getSoleChildOfType(NodeType.OUTS);
      assert.isNotNull(outsNode);
      assert.equal(outsNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
    });
    it("can parse correctly (space after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs(digit )');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isTrue(outsStatement.parse(root));
      const outsNode = root.getSoleChildOfType(NodeType.OUTS);
      assert.isNotNull(outsNode);
      assert.equal(outsNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
    });
    it("can parse correctly (space before and after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs( digit )');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isTrue(outsStatement.parse(root));
      const outsNode = root.getSoleChildOfType(NodeType.OUTS);
      assert.isNotNull(outsNode);
      assert.equal(outsNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'digit');
    });
    it("can parse correctly (system store)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs(&keyboardversion)');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isTrue(outsStatement.parse(root));
      const outsNode = root.getSoleChildOfType(NodeType.OUTS);
      assert.isNotNull(outsNode);
      assert.isNotNull(outsNode.getSoleChildOfType(NodeType.KEYBOARDVERSION));
    });
  });
});