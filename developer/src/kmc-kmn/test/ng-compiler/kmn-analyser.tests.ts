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
import { BitmapStoreAssignRule, BitmapStoreRule, BlankLineRule, CommentEndRule, ContentLineRule, MultiLineRule, SoloLineRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ContinuationEndRule, ContinuationLineRule, CopyrightStoreAssignRule, CopyrightStoreRule, IncludecodesStoreAssignRule } from '../../src/ng-compiler/kmn-analyser.js';
import { IncludecodesStoreRule, SystemStoreAssignRule } from '../../src/ng-compiler/kmn-analyser.js';import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';

let root: ASTNode = null;

describe("KMN Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeTypes.TMP);
  });
  describe("MultiLineRule Tests", () => {
    it("can construct a MultiLineRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const lineBlock: Rule = new MultiLineRule(tokenBuffer);
      assert.isNotNull(lineBlock);
    });
    it("can parse correctly", () => {
      const line1 = 'store(&bitmap) "filename" \\\n'
      const line2 = 'store(&copyright) "message" \\\n'
      const line3 = 'store(&includecodes) "filename" c a comment\n'
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(`${line1}${line2}${line3}`);
      const lineBlock: Rule = new MultiLineRule(tokenBuffer);
      assert.isTrue(lineBlock.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[1].nodeType, NodeTypes.COPYRIGHT);
      assert.equal(children[2].nodeType, NodeTypes.INCLUDECODES);
      assert.equal(children[3].nodeType, NodeTypes.LINE);
      assert.equal(children[3].token.lineNum, 1);
      assert.equal(children[4].nodeType, NodeTypes.LINE);
      assert.equal(children[4].token.lineNum, 2);
      assert.equal(children[5].nodeType, NodeTypes.LINE);
      assert.equal(children[5].token.lineNum, 3);
    });
  });
  describe("SoloLineRule Tests", () => {
    it("can construct a SoloLineRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const soloLine: Rule = new SoloLineRule(tokenBuffer);
      assert.isNotNull(soloLine);
    });
    it("can parse correctly (contentLine)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const soloLine: Rule = new SoloLineRule(tokenBuffer);
      assert.isTrue(soloLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (blankLine, comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied\n');
      const soloLine: Rule = new SoloLineRule(tokenBuffer);
      assert.isTrue(soloLine.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
  });
  describe("ContentLineRule Tests", () => {
    it("can construct a ContentLineRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const contentLine: Rule = new ContentLineRule(tokenBuffer);
      assert.isNotNull(contentLine);
    });
    it("can parse correctly (no space before, no comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const contentLine: Rule = new ContentLineRule(tokenBuffer);
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before, no comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename"\n');
      const contentLine: Rule = new ContentLineRule(tokenBuffer);
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (no space before, comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" c a comment\n');
      const contentLine: Rule = new ContentLineRule(tokenBuffer);
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before, comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename" c a comment\n');
      const contentLine: Rule = new ContentLineRule(tokenBuffer);
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
  });
  describe("BlankLineRule Tests", () => {
    it("can construct a BlankLineRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\n');
      const blankLine: Rule = new BlankLineRule(tokenBuffer);
      assert.isNotNull(blankLine);
    });
    it("can parse correctly (no comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\n');
      const blankLine: Rule = new BlankLineRule(tokenBuffer);
      assert.isTrue(blankLine.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
    it("can parse correctly (comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied\n');
      const blankLine: Rule = new BlankLineRule(tokenBuffer);
      assert.isTrue(blankLine.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
  })
  describe("ContinuationLineRule Tests", () => {
    it("can construct a ContinuationLineRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" \\\n');
      const continuationLine: Rule = new ContinuationLineRule(tokenBuffer);
      assert.isNotNull(continuationLine);
    });
    it("can parse correctly (no space before)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" \\\n');
      const continuationLine: Rule = new ContinuationLineRule(tokenBuffer);
      assert.isTrue(continuationLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename" \\\n');
      const continuationLine: Rule = new ContinuationLineRule(tokenBuffer);
      assert.isTrue(continuationLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
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
      assert.isFalse(root.hasChild());
    });
    it("can parse correctly (space before)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' c This tells Keyman which keys should have casing behavior applied');
      const commentEnd: Rule = new CommentEndRule(tokenBuffer);
      assert.isTrue(commentEnd.parse(root));
      assert.isFalse(root.hasChild());
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
      assert.isFalse(root.hasChild());
    });
    it("can parse correctly (space after)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\\ \n');
      const continuationEnd: Rule = new ContinuationEndRule(tokenBuffer);
      assert.isTrue(continuationEnd.parse(root));
      assert.isFalse(root.hasChild());
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
