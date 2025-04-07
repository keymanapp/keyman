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
import { AnyStatementRule, BaselayoutStatementRule, BeepStatementRule, BeginStatementRule, BlankLineRule } from '../../src/ng-compiler/kmn-analyser.js';
import { CasedkeysStoreAssignRule, CasedkeysStoreRule, CommentEndRule, ContentLineRule, ContentRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ContinuationNewlineRule, EntryPointRule, HotkeyStoreAssignRule, HotkeyStoreRule, SimpleTextRule } from '../../src/ng-compiler/kmn-analyser.js';
import { LineRule, PaddingRule, StringSystemStoreNameRule, StringSystemStoreAssignRule, StringSystemStoreRule } from '../../src/ng-compiler/kmn-analyser.js';
import { TextRangeRule, TextRule, VariableStoreAssignRule, VariableStoreRule, VirtualKeyRule } from '../../src/ng-compiler/kmn-analyser.js';
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
  describe("ContentRule Tests", () => {
    it("can construct a ContentRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const content: Rule = new ContentRule(tokenBuffer);
      assert.isNotNull(content);
    });
    it("can parse correctly (string system store assign)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const content: Rule = new ContentRule(tokenBuffer);
      assert.isTrue(content.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (casedkeys store assign)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]');
      const content: Rule            = new ContentRule(tokenBuffer);
      assert.isTrue(content.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      assert.isNotNull(casedkeysNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (hotkey store assign)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&hotkey) [K_A]');
      const content: Rule            = new ContentRule(tokenBuffer);
      assert.isTrue(content.parse(root));
      const hotkeyNode = root.getSoleChildOfType(NodeTypes.HOTKEY)
      assert.isNotNull(hotkeyNode);
      assert.isNotNull(hotkeyNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (variable store assign)", () => {
      const tokenBuffer: TokenBuffer  = stringToTokenBuffer('store(c_out) U+1780');
      const content: Rule = new ContentRule(tokenBuffer);
      assert.isTrue(content.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.U_CHAR));
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
      assert.isNotNull(bitmapNode.getSoleChildOfType(NodeTypes.LINE));
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
    it("can parse correctly (displaymap)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('displaymap');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.DISPLAYMAP));
    });
    it("can parse correctly (ethnologuecode)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('ethnologuecode');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.ETHNOLOGUECODE));
    });
    it("can parse correctly (includecodes)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('includecodes');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.INCLUDECODES));
    });
    it("can parse correctly (keyboardversion)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('keyboardversion');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYBOARDVERSION));
    });
    it("can parse correctly (kmw_embedcss)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('kmw_embedcss');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_EMBEDCSS));
    });
    it("can parse correctly (kmw_embedjs)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('kmw_embedjs');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_EMBEDJS));
    });
    it("can parse correctly (kmw_helpfile)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('kmw_helpfile');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_HELPFILE));
    });
    it("can parse correctly (kmw_helptext)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('kmw_helptext');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_HELPTEXT));
    });
    it("can parse correctly (kmw_rtl)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('kmw_rtl');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_RTL));
    });
    it("can parse correctly (language)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('language');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LANGUAGE));
    });
    it("can parse correctly (layoutfile)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('layoutfile');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYOUTFILE));
    });
    it("can parse correctly (message)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('message');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.MESSAGE));
    });
    it("can parse correctly (mnemoniclayout)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('mnemoniclayout');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.MNEMONICLAYOUT));
    });
    it("can parse correctly (name)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('name');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NAME));
    });
    it("can parse correctly (targets)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('targets');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.TARGETS));
    });
    it("can parse correctly (version)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('version');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VERSION));
    });
    it("can parse correctly (visualkeyboard)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('visualkeyboard');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VISUALKEYBOARD));
    });
    it("can parse correctly (windowslanguages)", () => {
      const tokenBuffer: TokenBuffer    = stringToTokenBuffer('windowslanguages');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.WINDOWSLANGUAGES));
    });
  });
  describe("CasedkeysStoreAssignRule Tests", () => {
    it("can construct a CasedkeysStoreAssignRule", () => {
      const tokenBuffer: TokenBuffer   = stringToTokenBuffer('');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule(tokenBuffer);
      assert.isNotNull(casedkeysStoreAssign);
    });
    it("can parse correctly (single virtual key)", () => {
      const tokenBuffer: TokenBuffer   = stringToTokenBuffer('store(&casedkeys) [K_A]');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule(tokenBuffer);
      assert.isTrue(casedkeysStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      assert.isNotNull(casedkeysNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (two virtual keys)", () => {
      const tokenBuffer: TokenBuffer   = stringToTokenBuffer('store(&casedkeys) [K_A] [K_B]');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule(tokenBuffer);
      assert.isTrue(casedkeysStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const children = casedkeysNode.getChildrenOfType(NodeTypes.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].getSoleChild().getText(), 'K_B');
    });
    it("can parse correctly (virtual key range)", () => {
      const tokenBuffer: TokenBuffer   = stringToTokenBuffer('store(&casedkeys) [K_A]..[K_C]');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule(tokenBuffer);
      assert.isTrue(casedkeysStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const rangeNode = casedkeysNode.getSoleChildOfType(NodeTypes.RANGE);
      const children  = rangeNode.getChildrenOfType(NodeTypes.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].getSoleChild().getText(), 'K_C');
    });
    it("can parse correctly (character range)", () => {
      const tokenBuffer: TokenBuffer   = stringToTokenBuffer('store(&casedkeys) "a".."c"');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule(tokenBuffer);
      assert.isTrue(casedkeysStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const rangeNode = casedkeysNode.getSoleChildOfType(NodeTypes.RANGE);
      const children  = rangeNode.getChildrenOfType(NodeTypes.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].getText(), '"c"');
    });
  });
  describe("CasedkeysStoreRule Tests", () => {
    it("can construct a CasedkeysStoreRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const casedkeysStore: Rule     = new CasedkeysStoreRule(tokenBuffer);
      assert.isNotNull(casedkeysStore);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&casedkeys)');
      const casedkeysStore: Rule     = new CasedkeysStoreRule(tokenBuffer);
      assert.isTrue(casedkeysStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.CASEDKEYS));
    });
  });
  describe("HotkeyStoreAssignRule Tests", () => {
    it("can construct a HotkeyStoreAssignRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const hotkeyStoreAssign: Rule  = new HotkeyStoreAssignRule(tokenBuffer);
      assert.isNotNull(hotkeyStoreAssign);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&hotkey) [K_A]');
      const hotkeyStoreAssign: Rule  = new HotkeyStoreAssignRule(tokenBuffer);
      assert.isTrue(hotkeyStoreAssign.parse(root));
      const hotkeyNode = root.getSoleChildOfType(NodeTypes.HOTKEY)
      assert.isNotNull(hotkeyNode);
      assert.isNotNull(hotkeyNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
  });
  describe("HotkeyStoreRule Tests", () => {
    it("can construct a HotkeyStoreRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const hotkeyStore: Rule        = new HotkeyStoreRule(tokenBuffer);
      assert.isNotNull(hotkeyStore);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('store(&hotkey)');
      const hotkeyStore: Rule        = new HotkeyStoreRule(tokenBuffer);
      assert.isTrue(hotkeyStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.HOTKEY));
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
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (mixed text)", () => {
      const tokenBuffer: TokenBuffer  = stringToTokenBuffer('store(c_out) " " U+1781 [K_K]');
      const variableStoreAssign: Rule = new VariableStoreAssignRule(tokenBuffer);
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.STRING));
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.U_CHAR));
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
  });
  describe("TextRule Tests", () => {
    it("can construct a TextRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const text: Rule               = new TextRule(tokenBuffer);
      assert.isNotNull(text);
    });
    it("can parse correctly (simple text)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('"a"');
      const text: Rule               = new TextRule(tokenBuffer);
      assert.isTrue(text.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (text range)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('"a".."c"');
      const text: Rule               = new TextRule(tokenBuffer);
      assert.isTrue(text.parse(root));
      const rangeNode = root.getSoleChildOfType(NodeTypes.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeTypes.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
  });
  describe("SimpleTextRule Tests", () => {
    it("can construct a SimpleTextRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const simpleText: Rule         = new SimpleTextRule(tokenBuffer);
      assert.isNotNull(simpleText);
    });
    it("can parse correctly (string)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('"a"');
      const simpleText: Rule         = new SimpleTextRule(tokenBuffer);
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (u_char)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('U+1780');
      const simpleText: Rule         = new SimpleTextRule(tokenBuffer);
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (virtualKey)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[K_K]');
      const simpleText: Rule         = new SimpleTextRule(tokenBuffer);
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
  });
  describe("TextRangeRule Tests", () => {
    it("can construct a TextRangeRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const textRange: Rule          = new TextRangeRule(tokenBuffer);
      assert.isNotNull(textRange);
    });
    it("can parse correctly (string range)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('"a".."c"');
      const textRange: Rule          = new TextRangeRule(tokenBuffer);
      assert.isTrue(textRange.parse(root));
      const rangeNode = root.getSoleChildOfType(NodeTypes.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeTypes.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (string range, space before range)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('"a" .."c"');
      const textRange: Rule          = new TextRangeRule(tokenBuffer);
      assert.isTrue(textRange.parse(root));
      const rangeNode = root.getSoleChildOfType(NodeTypes.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeTypes.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (string range, space after range)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('"a".. "c"');
      const textRange: Rule          = new TextRangeRule(tokenBuffer);
      assert.isTrue(textRange.parse(root));
      const rangeNode = root.getSoleChildOfType(NodeTypes.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeTypes.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (string range, space before and after range)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('"a" .. "c"');
      const textRange: Rule          = new TextRangeRule(tokenBuffer);
      assert.isTrue(textRange.parse(root));
      const rangeNode = root.getSoleChildOfType(NodeTypes.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeTypes.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (virtual key range)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[K_A]..[K_C]');
      const textRange: Rule          = new TextRangeRule(tokenBuffer);
      assert.isTrue(textRange.parse(root));
      const rangeNode = root.getSoleChildOfType(NodeTypes.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].nodeType, NodeTypes.VIRTUAL_KEY);
      assert.equal(children[1].getSoleChild().getText(), 'K_C');
    });
    it("can parse correctly (multiple virtual key range)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('[K_A]..[K_C]..[K_E]');
      const textRange: Rule          = new TextRangeRule(tokenBuffer);
      assert.isTrue(textRange.parse(root));
      const rangeNode = root.getSoleChildOfType(NodeTypes.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].nodeType, NodeTypes.VIRTUAL_KEY);
      assert.equal(children[1].getSoleChild().getText(), 'K_C');
      assert.equal(children[2].nodeType, NodeTypes.VIRTUAL_KEY);
      assert.equal(children[2].getSoleChild().getText(), 'K_E');
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
  describe("AnyStatementRule Tests", () => {
    it("can construct an AnyStatementRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const anyStatement: Rule       = new AnyStatementRule(tokenBuffer);
      assert.isNotNull(anyStatement);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('any(digit)');
      const anyStatement: Rule       = new AnyStatementRule(tokenBuffer);
      assert.isTrue(anyStatement.parse(root));
      const anyNode = root.getSoleChildOfType(NodeTypes.ANY);
      assert.isNotNull(anyNode);
      assert.isNotNull(anyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("BaselayoutStatementRule Tests", () => {
    it("can construct an BaselayoutStatementRule", () => {
      const tokenBuffer: TokenBuffer  = stringToTokenBuffer('');
      const baselayoutStatement: Rule = new BaselayoutStatementRule(tokenBuffer);
      assert.isNotNull(baselayoutStatement);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer  = stringToTokenBuffer('baselayout("en-US")');
      const baselayoutStatement: Rule = new BaselayoutStatementRule(tokenBuffer);
      assert.isTrue(baselayoutStatement.parse(root));
      const baselayoutNode = root.getSoleChildOfType(NodeTypes.BASELAYOUT);
      assert.isNotNull(baselayoutNode);
      const stringNode = baselayoutNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"en-US"');
    });
  });
  describe("BeepStatementRule Tests", () => {
    it("can construct an BeepStatementRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const beepStatement: Rule      = new BeepStatementRule(tokenBuffer);
      assert.isNotNull(beepStatement);
    });
    it("can parse correctly", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('beep');
      const beepStatement: Rule      = new BeepStatementRule(tokenBuffer);
      assert.isTrue(beepStatement.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BEEP));
    });
  });
  describe("EntryPointRule Tests", () => {
    it("can construct an EntryPointRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const entryPoint: Rule         = new EntryPointRule(tokenBuffer);
      assert.isNotNull(entryPoint);
    });
    it("can parse correctly (unicode)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('unicode');
      const entryPoint: Rule         = new EntryPointRule(tokenBuffer);
      assert.isTrue(entryPoint.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.UNICODE));
    });
    it("can parse correctly (newcontext)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('newcontext');
      const entryPoint: Rule         = new EntryPointRule(tokenBuffer);
      assert.isTrue(entryPoint.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NEWCONTEXT));
    });
    it("can parse correctly (postkeystroke)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('postkeystroke');
      const entryPoint: Rule         = new EntryPointRule(tokenBuffer);
      assert.isTrue(entryPoint.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.POSTKEYSTROKE));
    });
    it("can parse correctly (ansi)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('ansi');
      const entryPoint: Rule         = new EntryPointRule(tokenBuffer);
      assert.isTrue(entryPoint.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.ANSI));
    });
  });
  describe("BeginStatementRule Tests", () => {
    it("can construct an BeginStatementRule", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('');
      const beginStatement: Rule     = new BeginStatementRule(tokenBuffer);
      assert.isNotNull(beginStatement);
    });
    it("can parse correctly (entrypoint)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('begin unicode');
      const beginStatement: Rule     = new BeginStatementRule(tokenBuffer);
      assert.isTrue(beginStatement.parse(root));
      const beginNode = root.getSoleChildOfType(NodeTypes.BEGIN);
      assert.isNotNull(beginNode);
      assert.isNotNull(beginNode.getSoleChildOfType(NodeTypes.UNICODE));
    });
    it("can parse correctly (no entrypoint)", () => {
      const tokenBuffer: TokenBuffer = stringToTokenBuffer('begin');
      const beginStatement: Rule     = new BeginStatementRule(tokenBuffer);
      assert.isTrue(beginStatement.parse(root));
      const beginNode = root.getSoleChildOfType(NodeTypes.BEGIN);
      assert.isNotNull(beginNode);
      assert.isFalse(beginNode.hasChild());
    });
  });
});

function stringToTokenBuffer(buffer: String): TokenBuffer {
  const lexer = new Lexer(buffer);
  const tokens: Token[] = lexer.parse();
  return new TokenBuffer(tokens);
}
