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
import { BitmapStoreAssignRule, BitmapStoreRule, CommentEndRule, ContinuationEndRule, CopyrightStoreAssignRule } from '../../src/ng-compiler/kmn-analyser.js';
import { CopyrightStoreRule, IncludecodesStoreAssignRule, IncludecodesStoreRule, SystemStoreAssignRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';

let root: ASTNode = null;

describe("KMN Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeTypes.TMP);
  });
  describe("CommentEndRule Tests", () => {
    it("can construct a CommentEndRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied');
      const commentEnd: Rule = new CommentEndRule(tokenBuffer);
      assert.isNotNull(commentEnd);
    });
    it("can parse correctly (no space before)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied');
      const commentEnd: Rule = new CommentEndRule(tokenBuffer);
      assert.isTrue(commentEnd.parse(root));
    });
    it("can parse correctly (space before)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' c This tells Keyman which keys should have casing behavior applied');
      const commentEnd: Rule = new CommentEndRule(tokenBuffer);
      assert.isTrue(commentEnd.parse(root));
    });
  });
  describe("ContinuationEndRule Tests", () => {
    it("can construct a ContinuationEndRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\\\n');
      const continuationEnd: Rule = new ContinuationEndRule(tokenBuffer);
      assert.isNotNull(continuationEnd);
    });
    it("can parse correctly (no space after)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\\\n');
      const continuationEnd: Rule = new ContinuationEndRule(tokenBuffer);
      assert.isTrue(continuationEnd.parse(root));
    });
    it("can parse correctly (space after)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\\ \n');
      const continuationEnd: Rule = new ContinuationEndRule(tokenBuffer);
      assert.isTrue(continuationEnd.parse(root));
    });
  });
  describe("SystemStoreAssignRule Tests", () => {
    it("can construct a SystemStoreAssignRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule(tokenBuffer);
      assert.isNotNull(systemStoreAssign);
    });
    it("can parse bitmap store correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule(tokenBuffer);
      assert.isTrue(systemStoreAssign.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.BITMAP);
      assert.equal(root.getSoleChild().getSoleChild().nodeType, NodeTypes.STRING);
    });
    it("can parse copyright store correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&copyright) "message"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule(tokenBuffer);
      assert.isTrue(systemStoreAssign.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.COPYRIGHT);
      assert.equal(root.getSoleChild().getSoleChild().nodeType, NodeTypes.STRING);
    });
    it("can parse includecodes store correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&includecodes) "filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule(tokenBuffer);
      assert.isTrue(systemStoreAssign.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.INCLUDECODES);
      assert.equal(root.getSoleChild().getSoleChild().nodeType, NodeTypes.STRING);
    });
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
    it("can parse correctly (whitespace after left bracket)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store( &bitmap)');
      const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
      assert.isTrue(bitmapStore.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.BITMAP);
    });
    it("can parse correctly (whitespace before right bracket)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap )');
      const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
      assert.isTrue(bitmapStore.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.BITMAP);
    });
    it("can parse correctly (whitespace after left and before right brackets)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store( &bitmap )');
      const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
      assert.isTrue(bitmapStore.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.BITMAP);
    });
  });
  describe("CopyrightStoreAssignRule Tests", () => {
    it("can construct a CopyrightStoreAssignRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&copyright) "message"');
      const copyrightStoreAssign: Rule = new CopyrightStoreAssignRule(tokenBuffer);
      assert.isNotNull(copyrightStoreAssign);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&copyright) "message"');
      const copyrightStoreAssign: Rule = new CopyrightStoreAssignRule(tokenBuffer);
      assert.isTrue(copyrightStoreAssign.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.COPYRIGHT);
      assert.equal(root.getSoleChild().getSoleChild().nodeType, NodeTypes.STRING);
    });
  });
  describe("CopyrightStoreRule Tests", () => {
    it("can construct a CopyrightStoreRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&copyright)');
      const copyrightStore: Rule = new CopyrightStoreRule(tokenBuffer);
      assert.isNotNull(copyrightStore);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&copyright)');
      const copyrightStore: Rule = new CopyrightStoreRule(tokenBuffer);
      assert.isTrue(copyrightStore.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.COPYRIGHT);
    });
  });
  describe("IncludecodesStoreAssignRule Tests", () => {
    it("can construct a IncludecodesStoreAssignRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&includecodes) "filename"');
      const includecodesStoreAssign: Rule = new IncludecodesStoreAssignRule(tokenBuffer);
      assert.isNotNull(includecodesStoreAssign);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&includecodes) "filename"');
      const includecodesStoreAssign: Rule = new IncludecodesStoreAssignRule(tokenBuffer);
      assert.isTrue(includecodesStoreAssign.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.INCLUDECODES);
      assert.equal(root.getSoleChild().getSoleChild().nodeType, NodeTypes.STRING);
    });
  });
  describe("IncludecodesStoreRule Tests", () => {
    it("can construct a IncludecodesStoreRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&includecodes)');
      const includecodesStore: Rule = new IncludecodesStoreRule(tokenBuffer);
      assert.isNotNull(includecodesStore);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&includecodes)');
      const includecodesStore: Rule = new IncludecodesStoreRule(tokenBuffer);
      assert.isTrue(includecodesStore.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.INCLUDECODES);
    });
  });
});

function stringToTokenBuffer(buffer: String): TokenBuffer {
  const lexer = new Lexer(buffer);
  const tokens: Token[] = lexer.parse();
  return new TokenBuffer(tokens);
}
