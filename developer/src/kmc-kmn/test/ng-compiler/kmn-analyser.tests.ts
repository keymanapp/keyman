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
import { AnyStatementRule, BaselayoutStatementRule, BeepStatementRule, BeginBlockRule, BeginStatementRule } from '../../src/ng-compiler/kmn-analyser.js';
import { BlankLineRule, BracketedStoreNameRule, CasedkeysStoreAssignRule, CasedkeysStoreRule, ContentLineRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ContentRule, ContinuationNewlineRule, EntryPointRule, GroupBlockRule, GroupQualifierRule } from '../../src/ng-compiler/kmn-analyser.js';
import { GroupStatementRule, HotkeyStoreAssignRule, HotkeyStoreRule, KmnTreeRule, LineRule } from '../../src/ng-compiler/kmn-analyser.js';
import { OutsStatementRule, PaddingRule, SimpleTextRule, StringSystemStoreNameRule, StringSystemStoreAssignRule } from '../../src/ng-compiler/kmn-analyser.js';
import { StringSystemStoreRule, TextRangeRule, TextRule, UseStatementRule, UsingKeysRule } from '../../src/ng-compiler/kmn-analyser.js';
import { VariableStoreAssignRule, VariableStoreRule, VirtualKeyRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';
import { readFileSync } from 'fs';

let root: ASTNode = null;

describe("KMN Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeTypes.TMP);
  });
  describe("LineRule Tests", () => {
    it("can construct a LineRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const line: Rule = new LineRule();
      assert.isNotNull(line);
    });
    it("can parse correctly (contentLine)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (blankLine, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
  });
  describe("ContentLineRule Tests", () => {
    it("can construct a ContentLineRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const contentLine: Rule = new ContentLineRule();
      assert.isNotNull(contentLine);
    });
    it("can parse correctly (no space before, no comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const contentLine: Rule = new ContentLineRule();
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (no space before, no comment, space before newline)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" \n');
      const contentLine: Rule = new ContentLineRule();
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before, no comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename"\n');
      const contentLine: Rule = new ContentLineRule();
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (no space before, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" c a comment\n');
      const contentLine: Rule = new ContentLineRule();
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename" c a comment\n');
      const contentLine: Rule = new ContentLineRule();
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
  });
  describe("BlankLineRule Tests", () => {
    it("can construct a BlankLineRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('\n');
      const blankLine: Rule = new BlankLineRule();
      assert.isNotNull(blankLine);
    });
    it("can parse correctly (no comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('\n');
      const blankLine: Rule = new BlankLineRule();
      assert.isTrue(blankLine.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
    it("can parse correctly (no comment, space before newline)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' \n');
      const blankLine: Rule = new BlankLineRule();
      assert.isTrue(blankLine.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
    it("can parse correctly (comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied\n');
      const blankLine: Rule = new BlankLineRule();
      assert.isTrue(blankLine.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
    it("can parse correctly (space before comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' c This tells Keyman which keys should have casing behavior applied\n');
      const blankLine: Rule = new BlankLineRule();
      assert.isTrue(blankLine.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
  })
  describe("PaddingRule Tests", () => {
    it("can construct a PaddingRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const padding: Rule = new PaddingRule();
      assert.isNotNull(padding);
    });
    it("can parse correctly (whitespace)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' ');
      const padding: Rule = new PaddingRule();
      assert.isTrue(padding.parse(root));
      assert.isFalse(root.hasChild());
    });
    it("can parse correctly (ContinuationNewline)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('\\ \n');
      const padding: Rule = new PaddingRule();
      assert.isTrue(padding.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
  });
  describe("ContinuationNewlineRule Tests", () => {
    it("can construct a ContinuationNewlineRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const continuationNewline: Rule = new ContinuationNewlineRule();
      assert.isNotNull(continuationNewline);
    });
    it("can parse correctly (no space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('\\\n');
      const continuationNewline: Rule = new ContinuationNewlineRule();
      assert.isTrue(continuationNewline.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('\\ \n');
      const continuationNewline: Rule = new ContinuationNewlineRule();
      assert.isTrue(continuationNewline.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' \\\n');
      const continuationNewline: Rule = new ContinuationNewlineRule();
      assert.isTrue(continuationNewline.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before and after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' \\ \n');
      const continuationNewline: Rule = new ContinuationNewlineRule();
      assert.isTrue(continuationNewline.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
    });
  });
  describe("ContentRule Tests", () => {
    it("can construct a ContentRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const content: Rule = new ContentRule();
      assert.isNotNull(content);
    });
    it("can parse correctly (string system store assign)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (casedkeys store assign)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      assert.isNotNull(casedkeysNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (hotkey store assign)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&hotkey) [K_A]');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(root));
      const hotkeyNode = root.getSoleChildOfType(NodeTypes.HOTKEY)
      assert.isNotNull(hotkeyNode);
      assert.isNotNull(hotkeyNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (variable store assign)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
  });
  describe("StringSystemStoreAssignRule Tests", () => {
    it("can construct a StringSystemStoreAssignRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule();
      assert.isNotNull(stringSystemStoreAssign);
    });
    it("can parse bitmap store correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule();
      assert.isTrue(stringSystemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse bitmap store correctly (with continuation)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap)\\\n"filename"');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule();
      assert.isTrue(stringSystemStoreAssign.parse(root));
      const bitmapNode = root.getSoleChildOfType(NodeTypes.BITMAP);
      assert.isNotNull(bitmapNode);
      assert.isNotNull(bitmapNode.getSoleChildOfType(NodeTypes.STRING));
      assert.isNotNull(bitmapNode.getSoleChildOfType(NodeTypes.LINE));
    });
    it("can parse copyright store correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&copyright) "message"');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule();
      assert.isTrue(stringSystemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.COPYRIGHT));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse includecodes store correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&includecodes) "filename"');
      const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule();
      assert.isTrue(stringSystemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.INCLUDECODES));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
  });
  describe("StringSystemStoreRule Tests", () => {
    it("can construct a StringSystemStoreRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const stringSystemStore: Rule = new StringSystemStoreRule();
      assert.isNotNull(stringSystemStore);
    });
    it("can parse correctly (bitmap)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap)');
      const stringSystemStore: Rule = new StringSystemStoreRule();
      assert.isTrue(stringSystemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace after left bracket)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store( &bitmap)');
      const stringSystemStore: Rule = new StringSystemStoreRule();
      assert.isTrue(stringSystemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace before right bracket)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap )');
      const stringSystemStore: Rule = new StringSystemStoreRule();
      assert.isTrue(stringSystemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace after left and before right brackets)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store( &bitmap )');
      const stringSystemStore: Rule = new StringSystemStoreRule();
      assert.isTrue(stringSystemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
  });
  describe("StringSystemStoreNameRule Tests", () => {
    it("can construct a StringSystemStoreNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isNotNull(stringSystemStoreName);
    });
    it("can parse correctly (bitmap)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('bitmap');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (copyright)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('copyright');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.COPYRIGHT));
    });
    it("can parse correctly (displaymap)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('displaymap');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.DISPLAYMAP));
    });
    it("can parse correctly (ethnologuecode)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('ethnologuecode');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.ETHNOLOGUECODE));
    });
    it("can parse correctly (includecodes)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('includecodes');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.INCLUDECODES));
    });
    it("can parse correctly (keyboardversion)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('keyboardversion');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYBOARDVERSION));
    });
    it("can parse correctly (kmw_embedcss)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('kmw_embedcss');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_EMBEDCSS));
    });
    it("can parse correctly (kmw_embedjs)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('kmw_embedjs');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_EMBEDJS));
    });
    it("can parse correctly (kmw_helpfile)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('kmw_helpfile');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_HELPFILE));
    });
    it("can parse correctly (kmw_helptext)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('kmw_helptext');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_HELPTEXT));
    });
    it("can parse correctly (kmw_rtl)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('kmw_rtl');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMW_RTL));
    });
    it("can parse correctly (language)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('language');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LANGUAGE));
    });
    it("can parse correctly (layoutfile)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layoutfile');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYOUTFILE));
    });
    it("can parse correctly (message)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('message');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.MESSAGE));
    });
    it("can parse correctly (mnemoniclayout)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('mnemoniclayout');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.MNEMONICLAYOUT));
    });
    it("can parse correctly (name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('name');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NAME));
    });
    it("can parse correctly (targets)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('targets');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.TARGETS));
    });
    it("can parse correctly (version)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('version');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VERSION));
    });
    it("can parse correctly (visualkeyboard)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('visualkeyboard');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VISUALKEYBOARD));
    });
    it("can parse correctly (windowslanguages)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('windowslanguages');
      const stringSystemStoreName: Rule = new StringSystemStoreNameRule();
      assert.isTrue(stringSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.WINDOWSLANGUAGES));
    });
  });
  describe("CasedkeysStoreAssignRule Tests", () => {
    it("can construct a CasedkeysStoreAssignRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule();
      assert.isNotNull(casedkeysStoreAssign);
    });
    it("can parse correctly (single virtual key)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule();
      assert.isTrue(casedkeysStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      assert.isNotNull(casedkeysNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (two virtual keys)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A] [K_B]');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule();
      assert.isTrue(casedkeysStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const children = casedkeysNode.getChildrenOfType(NodeTypes.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].getSoleChild().getText(), 'K_B');
    });
    it("can parse correctly (virtual key range)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]..[K_C]');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule();
      assert.isTrue(casedkeysStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const rangeNode = casedkeysNode.getSoleChildOfType(NodeTypes.RANGE);
      const children  = rangeNode.getChildrenOfType(NodeTypes.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].getSoleChild().getText(), 'K_C');
    });
    it("can parse correctly (character range)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) "a".."c"');
      const casedkeysStoreAssign: Rule = new CasedkeysStoreAssignRule();
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
      Rule.tokenBuffer = stringToTokenBuffer('');
      const casedkeysStore: Rule = new CasedkeysStoreRule();
      assert.isNotNull(casedkeysStore);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys)');
      const casedkeysStore: Rule = new CasedkeysStoreRule();
      assert.isTrue(casedkeysStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.CASEDKEYS));
    });
  });
  describe("HotkeyStoreAssignRule Tests", () => {
    it("can construct a HotkeyStoreAssignRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const hotkeyStoreAssign: Rule  = new HotkeyStoreAssignRule();
      assert.isNotNull(hotkeyStoreAssign);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&hotkey) [K_A]');
      const hotkeyStoreAssign: Rule = new HotkeyStoreAssignRule();
      assert.isTrue(hotkeyStoreAssign.parse(root));
      const hotkeyNode = root.getSoleChildOfType(NodeTypes.HOTKEY)
      assert.isNotNull(hotkeyNode);
      assert.isNotNull(hotkeyNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
  });
  describe("HotkeyStoreRule Tests", () => {
    it("can construct a HotkeyStoreRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const hotkeyStore: Rule = new HotkeyStoreRule();
      assert.isNotNull(hotkeyStore);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&hotkey)');
      const hotkeyStore: Rule = new HotkeyStoreRule();
      assert.isTrue(hotkeyStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.HOTKEY));
    });
  });
  describe("VariableStoreAssignRule Tests", () => {
    it("can construct a VariableStoreAssignRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isNotNull(variableStoreAssign);
    });
    it("can parse correctly (single u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (single outs)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(ShiftOutAll)  outs(ShiftOutSingle)');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.OUTS));
    });
    it("can parse correctly (two u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 U+1781');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\nU+1781');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (mixed text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) " " U+1781 [K_K] outs(ShiftOutSingle)');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.STRING));
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.U_CHAR));
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.OUTS));
    });
  });
  describe("TextRule Tests", () => {
    it("can construct a TextRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const text: Rule = new TextRule();
      assert.isNotNull(text);
    });
    it("can parse correctly (text range)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('"a".."c"');
      const text: Rule = new TextRule();
      assert.isTrue(text.parse(root));
      const rangeNode = root.getSoleChildOfType(NodeTypes.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeTypes.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeTypes.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (simple text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('"a"');
      const text: Rule = new TextRule();
      assert.isTrue(text.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (outs statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs(digit)');
      const text: Rule = new TextRule();
      assert.isTrue(text.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.OUTS));
    });
  });
  describe("SimpleTextRule Tests", () => {
    it("can construct a SimpleTextRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const simpleText: Rule = new SimpleTextRule();
      assert.isNotNull(simpleText);
    });
    it("can parse correctly (string)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('"a"');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+1780');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (virtualKey)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[K_K]');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
  });
  describe("TextRangeRule Tests", () => {
    it("can construct a TextRangeRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const textRange: Rule = new TextRangeRule();
      assert.isNotNull(textRange);
    });
    it("can parse correctly (string range)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('"a".."c"');
      const textRange: Rule = new TextRangeRule();
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
      Rule.tokenBuffer = stringToTokenBuffer('"a" .."c"');
      const textRange: Rule = new TextRangeRule();
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
      Rule.tokenBuffer = stringToTokenBuffer('"a".. "c"');
      const textRange: Rule = new TextRangeRule();
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
      Rule.tokenBuffer = stringToTokenBuffer('"a" .. "c"');
      const textRange: Rule = new TextRangeRule();
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
      Rule.tokenBuffer = stringToTokenBuffer('[K_A]..[K_C]');
      const textRange: Rule = new TextRangeRule();
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
      Rule.tokenBuffer = stringToTokenBuffer('[K_A]..[K_C]..[K_E]');
      const textRange: Rule = new TextRangeRule();
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
      Rule.tokenBuffer = stringToTokenBuffer('');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isNotNull(virtualKey);
    });
    it("can parse correctly (simple virtual key)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
    });
    it("can parse correctly (single shiftcode virtual key)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[SHIFT K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.SHIFT_CODE));
    });
    it("can parse correctly (two shiftcode virtual key)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[SHIFT CTRL K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
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
      Rule.tokenBuffer = stringToTokenBuffer('[ K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
    });
    it("can parse correctly (simple virtual key, space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[K_K ]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
    });
    it("can parse correctly (simple virtual key, space before and after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[ K_K ]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
    });
  });
  describe("VariableStoreRule Tests", () => {
    it("can construct a VariableStoreRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const variableStore: Rule = new VariableStoreRule();
      assert.isNotNull(variableStore);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(digit)');
      const variableStore: Rule = new VariableStoreRule();
      assert.isTrue(variableStore.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE);
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("BracketedStoreNameRule Tests", () => {
    it("can construct a BracketedStoreNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const bracketedStoreName: Rule = new BracketedStoreNameRule();
      assert.isNotNull(bracketedStoreName);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('(digit)');
      const bracketedStoreName: Rule = new BracketedStoreNameRule();
      assert.isTrue(bracketedStoreName.parse(root));
      const storeNameNode: ASTNode = root.getSoleChildOfType(NodeTypes.STORENAME);
      assert.isNotNull(storeNameNode);
      assert.equal(storeNameNode.getText(), 'digit');
    });
    it("can parse correctly (space before name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('( digit)');
      const bracketedStoreName: Rule = new BracketedStoreNameRule();
      assert.isTrue(bracketedStoreName.parse(root));
      const storeNameNode: ASTNode = root.getSoleChildOfType(NodeTypes.STORENAME);
      assert.isNotNull(storeNameNode);
      assert.equal(storeNameNode.getText(), 'digit');
    });
    it("can parse correctly (space after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('(digit )');
      const bracketedStoreName: Rule = new BracketedStoreNameRule();
      assert.isTrue(bracketedStoreName.parse(root));
      const storeNameNode: ASTNode = root.getSoleChildOfType(NodeTypes.STORENAME);
      assert.isNotNull(storeNameNode);
      assert.equal(storeNameNode.getText(), 'digit');
    });
    it("can parse correctly (space before and after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('( digit )');
      const bracketedStoreName: Rule = new BracketedStoreNameRule();
      assert.isTrue(bracketedStoreName.parse(root));
      const storeNameNode: ASTNode = root.getSoleChildOfType(NodeTypes.STORENAME);
      assert.isNotNull(storeNameNode);
      assert.equal(storeNameNode.getText(), 'digit');
    });
  });
  describe("BeginBlockRule Tests", () => {
    it("can construct an BeginBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const beginBlock: Rule = new BeginBlockRule();
      assert.isNotNull(beginBlock);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('begin unicode > use(main)');
      const beginBlock: Rule = new BeginBlockRule();
      assert.isTrue(beginBlock.parse(root));
      const beginNode = root.getSoleChildOfType(NodeTypes.BEGIN);
      assert.isNotNull(beginNode);
      assert.isNotNull(beginNode.getSoleChildOfType(NodeTypes.USE));
    });
  });
  describe("BeginStatementRule Tests", () => {
    it("can construct an BeginStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const beginStatement: Rule = new BeginStatementRule();
      assert.isNotNull(beginStatement);
    });
    it("can parse correctly (entrypoint)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('begin unicode');
      const beginStatement: Rule = new BeginStatementRule();
      assert.isTrue(beginStatement.parse(root));
      const beginNode = root.getSoleChildOfType(NodeTypes.BEGIN);
      assert.isNotNull(beginNode);
      assert.isNotNull(beginNode.getSoleChildOfType(NodeTypes.UNICODE));
    });
    it("can parse correctly (no entrypoint)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('begin');
      const beginStatement: Rule = new BeginStatementRule();
      assert.isTrue(beginStatement.parse(root));
      const beginNode = root.getSoleChildOfType(NodeTypes.BEGIN);
      assert.isNotNull(beginNode);
      assert.isFalse(beginNode.hasChild());
    });
  });
  describe("EntryPointRule Tests", () => {
    it("can construct an EntryPointRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const entryPoint: Rule = new EntryPointRule();
      assert.isNotNull(entryPoint);
    });
    it("can parse correctly (unicode)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('unicode');
      const entryPoint: Rule = new EntryPointRule();
      assert.isTrue(entryPoint.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.UNICODE));
    });
    it("can parse correctly (newcontext)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('newcontext');
      const entryPoint: Rule = new EntryPointRule();
      assert.isTrue(entryPoint.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NEWCONTEXT));
    });
    it("can parse correctly (postkeystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('postkeystroke');
      const entryPoint: Rule = new EntryPointRule();
      assert.isTrue(entryPoint.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.POSTKEYSTROKE));
    });
    it("can parse correctly (ansi)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('ansi');
      const entryPoint: Rule = new EntryPointRule();
      assert.isTrue(entryPoint.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.ANSI));
    });
  });
  describe("UseStatementRule Tests", () => {
    it("can construct an UseStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const useStatement: Rule = new UseStatementRule();
      assert.isNotNull(useStatement);
    });
    it("can parse correctly (parameter)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use(main)');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(root));
      const useNode = root.getSoleChildOfType(NodeTypes.USE);
      assert.isNotNull(useNode);
      assert.isNotNull(useNode.getSoleChildOfType(NodeTypes.GROUPNAME));
    });
    it("can parse correctly (permitted keyword)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use(postkeystroke)');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(root));
      const useNode = root.getSoleChildOfType(NodeTypes.USE);
      assert.isNotNull(useNode);
      assert.isNotNull(useNode.getSoleChildOfType(NodeTypes.GROUPNAME));
    });
  });
  describe("GroupBlockRule Tests", () => {
    it("can construct an GroupBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const groupBlock: Rule = new GroupBlockRule();
      assert.isNotNull(groupBlock);
    });
    it("can parse correctly (no qualifier)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(main)');
      const groupBlock: Rule = new GroupBlockRule();
      assert.isTrue(groupBlock.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNull(groupNode.getSoleChildOfType(NodeTypes.USING_KEYS));
      assert.isNull(groupNode.getSoleChildOfType(NodeTypes.READONLY));
    });
    it("can parse correctly (using keys)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(main) using keys');
      const groupBlock: Rule = new GroupBlockRule();
      assert.isTrue(groupBlock.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.USING_KEYS));
      assert.isNull(groupNode.getSoleChildOfType(NodeTypes.READONLY));
    });
    it("can parse correctly (readonly)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(main) readonly');
      const groupBlock: Rule = new GroupBlockRule();
      assert.isTrue(groupBlock.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNull(groupNode.getSoleChildOfType(NodeTypes.USING_KEYS));
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.READONLY));
    });
  });
  describe("GroupStatementRule Tests", () => {
    it("can construct an GroupStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isNotNull(groupStatement);
    });
    it("can parse correctly (parameter)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(main)');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.GROUPNAME));
    });
    it("can parse correctly (permitted keyword)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(newcontext)');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.GROUPNAME));
    });
  });
  describe("GroupQualifierRule Tests", () => {
    it("can construct an GroupQualifierRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const groupQualifier: Rule = new GroupQualifierRule();
      assert.isNotNull(groupQualifier);
    });
    it("can parse correctly (using keys)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('using keys');
      const groupQualifier: Rule = new GroupQualifierRule();
      assert.isTrue(groupQualifier.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.USING_KEYS));
    });
    it("can parse correctly (readonly)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('readonly');
      const groupQualifier: Rule = new GroupQualifierRule();
      assert.isTrue(groupQualifier.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.READONLY));
    });
  });
  describe("UsingKeysRule Tests", () => {
    it("can construct an UsingKeysRuleRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const usingKeys: Rule = new UsingKeysRule();
      assert.isNotNull(usingKeys);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('using keys');
      const usingKeys: Rule = new UsingKeysRule();
      assert.isTrue(usingKeys.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.USING_KEYS));
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
      const outsNode = root.getSoleChildOfType(NodeTypes.OUTS);
      assert.isNotNull(outsNode);
      assert.isNotNull(outsNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
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
      const baselayoutNode = root.getSoleChildOfType(NodeTypes.BASELAYOUT);
      assert.isNotNull(baselayoutNode);
      const stringNode = baselayoutNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"en-US"');
    });
  });
  describe("BeepStatementRule Tests", () => {
    it("can construct an BeepStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const beepStatement: Rule = new BeepStatementRule();
      assert.isNotNull(beepStatement);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('beep');
      const beepStatement: Rule = new BeepStatementRule();
      assert.isTrue(beepStatement.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BEEP));
    });
  });
  describe("Analyser Tests", () => {
    it("can parse (part of) Khmer Angkor correctly", () => {
      const buffer: String = new String(readFileSync('test/fixtures/keyboards/khmer_angkor.kmn'));
      const lexer = new Lexer(buffer);
      const tokens: Token[] = lexer.parse();
      const subset: Token[] = tokens.filter((token) => token.lineNum <= 33);
      Rule.tokenBuffer = new TokenBuffer(subset);
      const kmnTreeRule: Rule = new KmnTreeRule();
      assert.isTrue(kmnTreeRule.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VERSION));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NAME));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.COPYRIGHT));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.MESSAGE));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.TARGETS));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.DISPLAYMAP));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYOUTFILE));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYBOARDVERSION));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VISUALKEYBOARD));
      const beginNodes = root.getChildrenOfType(NodeTypes.BEGIN);
      assert.equal(beginNodes.length, 2);
      assert.equal(beginNodes[0].getDescendents(NodeTypes.GROUPNAME)[0].getText(), 'main');
      assert.equal(beginNodes[1].getDescendents(NodeTypes.GROUPNAME)[0].getText(), 'PostKeystroke');
      const storeNodes = root.getChildrenOfType(NodeTypes.STORE);
      assert.equal(storeNodes.length, 8);
      assert.equal(storeNodes[0].getDescendents(NodeTypes.STORENAME)[0].getText(), 'ShiftOutSingle');
      assert.equal(storeNodes[1].getDescendents(NodeTypes.STORENAME)[0].getText(), 'vCombo1');
      assert.equal(storeNodes[2].getDescendents(NodeTypes.STORENAME)[0].getText(), 'vCombo2');
      assert.equal(storeNodes[3].getDescendents(NodeTypes.STORENAME)[0].getText(), 'vCombo3');
      assert.equal(storeNodes[4].getDescendents(NodeTypes.STORENAME)[0].getText(), 'ShiftOutAll');
      assert.equal(storeNodes[5].getDescendents(NodeTypes.STORENAME)[0].getText(), 'digit');
      assert.equal(storeNodes[6].getDescendents(NodeTypes.STORENAME)[0].getText(), 'number');
      assert.equal(storeNodes[7].getDescendents(NodeTypes.STORENAME)[0].getText(), 'whitespace');
      const groupNodes = root.getChildrenOfType(NodeTypes.GROUP);
      assert.equal(groupNodes.length, 1);
      assert.equal(groupNodes[0].getDescendents(NodeTypes.GROUPNAME)[0].getText(), 'NewContext');
      assert.equal(groupNodes[0].getDescendents(NodeTypes.READONLY).length, 1);
      //assert.equal(root.toString(), '');
    });
  });
});

function stringToTokenBuffer(buffer: String): TokenBuffer {
  const lexer = new Lexer(buffer);
  const tokens: Token[] = lexer.parse();
  return new TokenBuffer(tokens);
}
