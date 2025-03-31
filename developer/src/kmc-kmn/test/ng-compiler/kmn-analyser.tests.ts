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
import { BlankLineRule, CommentEndRule, ContentLineRule, ContinuationNewlineRule, LineRule, TextRule, VariableStoreAssignRule, VariableStoreRule, VirtualKeyRule } from '../../src/ng-compiler/kmn-analyser.js';
import { PaddingRule, StringSystemStoreNameRule, StringSystemStoreRule, StringSystemStoreAssignRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';

let root: ASTNode = null;

describe("KMN Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeTypes.TMP);
  });
  describe("LineRule Tests", () => {
    it("can construct a LineRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const line: Rule = new LineRule(tokenBuffer);
      assert.isNotNull(line);
    });
    it("can parse correctly (contentLine)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const line: Rule = new LineRule(tokenBuffer);
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (blankLine, comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied\n');
      const line: Rule = new LineRule(tokenBuffer);
      assert.isTrue(line.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
  });
  describe("ContentLineRule Tests", () => {
    it("can construct a ContentLineRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
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
    it("can parse correctly (space before comment)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' c This tells Keyman which keys should have casing behavior applied\n');
      const blankLine: Rule = new BlankLineRule(tokenBuffer);
      assert.isTrue(blankLine.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
  })
  describe("CommentEndRule Tests", () => {
    it("can construct a CommentEndRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
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
  describe("PaddingRule Tests", () => {
    it("can construct a PaddingRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const padding: Rule = new PaddingRule(tokenBuffer);
      assert.isNotNull(padding);
    });
    it("can parse correctly (whitespace)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' ');
      const padding: Rule = new PaddingRule(tokenBuffer);
      assert.isTrue(padding.parse(root));
      assert.isFalse(root.hasChild());
    });
    it("can parse correctly (ContinuationNewline)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\\ \n');
      const padding: Rule = new PaddingRule(tokenBuffer);
      assert.isTrue(padding.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
  });
  describe("ContinuationNewlineRule Tests", () => {
    it("can construct a ContinuationNewlineRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const continuationNewline: Rule = new ContinuationNewlineRule(tokenBuffer);
      assert.isNotNull(continuationNewline);
    });
    it("can parse correctly (no space after)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\\\n');
      const continuationNewline: Rule = new ContinuationNewlineRule(tokenBuffer);
      assert.isTrue(continuationNewline.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space after)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('\\ \n');
      const continuationNewline: Rule = new ContinuationNewlineRule(tokenBuffer);
      assert.isTrue(continuationNewline.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' \\\n');
      const continuationNewline: Rule = new ContinuationNewlineRule(tokenBuffer);
      assert.isTrue(continuationNewline.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before and after)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer(' \\ \n');
      const continuationNewline: Rule = new ContinuationNewlineRule(tokenBuffer);
      assert.isTrue(continuationNewline.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
  });
  describe("StringSystemStoreAssignRule Tests", () => {
    it("can construct a StringSystemStoreAssignRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule(tokenBuffer);
      assert.isNotNull(stringSystemStoreAssign);
    });
    it("can parse bitmap store correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule(tokenBuffer);
      assert.isTrue(stringSystemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse bitmap store correctly (with continuation)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap)\\\n"filename"');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule(tokenBuffer);
      assert.isTrue(stringSystemStoreAssign.parse(root));
      const bitmapNode = root.getSoleChildOfType(NodeTypes.BITMAP);
      assert.isNotNull(bitmapNode);
      assert.isNotNull(bitmapNode.getSoleChildOfType(NodeTypes.STRING));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LINE));
    });
    it("can parse copyright store correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&copyright) "message"');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule(tokenBuffer);
      assert.isTrue(stringSystemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.COPYRIGHT));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse includecodes store correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&includecodes) "filename"');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule(tokenBuffer);
      assert.isTrue(stringSystemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.INCLUDECODES));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
  });
  describe("StringSystemStoreRule Tests", () => {
    it("can construct a StringSystemStoreRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const stringSystemStore: Rule = new StringSystemStoreRule(tokenBuffer);
      assert.isNotNull(stringSystemStore);
    });
    it("can parse correctly (bitmap)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap)');
      const stringSystemStore: Rule = new StringSystemStoreRule(tokenBuffer);
      assert.isTrue(stringSystemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace after left bracket)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store( &bitmap)');
      const stringSystemStore: Rule = new StringSystemStoreRule(tokenBuffer);
      assert.isTrue(stringSystemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace before right bracket)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap )');
      const stringSystemStore: Rule = new StringSystemStoreRule(tokenBuffer);
      assert.isTrue(stringSystemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace after left and before right brackets)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store( &bitmap )');
      const stringSystemStore: Rule = new StringSystemStoreRule(tokenBuffer);
      assert.isTrue(stringSystemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
  });
  describe("StringSystemStoreNameRule Tests", () => {
    it("can construct a StringSystemStoreNameRule", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isNotNull(stringSystemStoreName);
    });
    it("can parse correctly (bitmap)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('bitmap');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (copyright)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('copyright');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.COPYRIGHT));
    });
    it("can parse correctly (includecodes)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('includecodes');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.INCLUDECODES));
    });
  });
  describe("VariableStoreAssignRule Tests", () => {
    it("can construct a VariableStoreAssignRule", () => {
      const tokenBuffer: TokenBuffer  = stringToTokenBuffer('');
      const variableStoreAssign: Rule = new VariableStoreAssignRule(tokenBuffer);
      assert.isNotNull(variableStoreAssign);
    });
    it("can parse correctly (single u_char)", () => {
      const tokenBuffer: TokenBuffer  = stringToTokenBuffer('store(c_out) U+1780');
      const variableStoreAssign: Rule = new VariableStoreAssignRule(tokenBuffer);
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (two u_char)", () => {
      const tokenBuffer: TokenBuffer  = stringToTokenBuffer('store(c_out) U+1780 U+1781');
      const variableStoreAssign: Rule = new VariableStoreAssignRule(tokenBuffer);
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation)", () => {
      const tokenBuffer: TokenBuffer  = stringToTokenBuffer('store(c_out) U+1780\\\nU+1781');
      const variableStoreAssign: Rule = new VariableStoreAssignRule(tokenBuffer);
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LINE));
    });
  });
  describe("TextRule Tests", () => {
    it("can construct a TextRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const text: Rule               = new TextRule(tokenBuffer);
      assert.isNotNull(text);
    });
    it("can parse correctly (string)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('"a"');
      const text: Rule               = new TextRule(tokenBuffer);
      assert.isTrue(text.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (u_char)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('U+1780');
      const text: Rule               = new TextRule(tokenBuffer);
      assert.isTrue(text.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.U_CHAR));
    });
  });
  describe("VirtualKeyRule Tests", () => {
    it("can construct a VirtualKeyRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const virtualKey: Rule         = new VirtualKeyRule(tokenBuffer);
      assert.isNotNull(virtualKey);
    });
    it("can parse correctly (simple virtual key)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[K_K]');
      const virtualKey: Rule         = new VirtualKeyRule(tokenBuffer);
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
    });
    it("can parse correctly (single shiftcode virtual key)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[SHIFT K_K]');
      const virtualKey: Rule         = new VirtualKeyRule(tokenBuffer);
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.SHIFT_CODE));
    });
    it("can parse correctly (two shiftcode virtual key)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[SHIFT CTRL K_K]');
      const virtualKey: Rule         = new VirtualKeyRule(tokenBuffer);
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
      const shiftCodes = virtualKeyNode.getChildrenOfType(NodeTypes.SHIFT_CODE);
      assert.equal(shiftCodes.length, 2);
      assert.equal(shiftCodes[0].token.text, 'SHIFT');
      assert.equal(shiftCodes[1].token.text, 'CTRL');
    });
    it("can parse correctly (simple virtual key, space before)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[ K_K]');
      const virtualKey: Rule         = new VirtualKeyRule(tokenBuffer);
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
    });
    it("can parse correctly (simple virtual key, space after)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[K_K ]');
      const virtualKey: Rule         = new VirtualKeyRule(tokenBuffer);
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
    });
    it("can parse correctly (simple virtual key, space before and after)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[ K_K ]');
      const virtualKey: Rule         = new VirtualKeyRule(tokenBuffer);
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
    });
  });
  describe("VariableStoreRule Tests", () => {
    it("can construct a VariableStoreRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const variableStore: Rule      = new VariableStoreRule(tokenBuffer);
      assert.isNotNull(variableStore);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(digit)');
      const variableStore: Rule      = new VariableStoreRule(tokenBuffer);
      assert.isTrue(variableStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.STORE));
    });
  });
});

function stringToTokenBuffer(buffer: String): TokenBuffer {
  const lexer = new Lexer(buffer);
  const tokens: Token[] = lexer.parse();
  return new TokenBuffer(tokens);
}
