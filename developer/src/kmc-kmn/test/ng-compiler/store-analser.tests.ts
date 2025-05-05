/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 * 
 * System and Variable Store Rules
 */

import 'mocha';
import { assert } from 'chai';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';
import { Rule } from '../../src/ng-compiler/recursive-descent.js';
import { stringToTokenBuffer } from './kmn-analyser.tests.js';
import { BracketedStoreNameRule, CasedkeysStoreAssignRule, CasedkeysStoreRule, HotkeyStoreAssignRule, HotkeyStoreRule, PermittedKeywordRule, StringSystemStoreAssignRule, StringSystemStoreNameRule, StringSystemStoreRule, SystemStoreNameRule, VariableStoreAssignRule, VariableStoreRule } from '../../src/ng-compiler/store-analyser.js';

let root: ASTNode = null;

describe("KMN Store Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeTypes.TMP);
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
    it("can parse correctly (two u_char with continuation, space before)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\\nU+1781');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (two u_char with continuation, space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\n U+1781');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (two u_char with continuation, spaces before and after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\\n U+1781');
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (two u_char with continuation, spaces before, after and between)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\ \n U+1781');
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
    it("can parse correctly (multi line assignment)", () => {
      const line1 = 'store(c_key)           [K_K] [K_X] [SHIFT K_K] [SHIFT K_X] [K_G] \\\n';
      const line2 = '                       [K_C] [K_Q] [SHIFT K_C] [SHIFT K_Q] [SHIFT K_J] \\\n';
      const line3 = '                       [K_D] [K_Z] [SHIFT K_D] [SHIFT K_Z] [SHIFT K_N] \\\n';
      const line4 = '                       [K_T] [K_F] [SHIFT K_T] [SHIFT K_F] [K_N] \\\n';
      const line5 = '                       [K_B] [K_P] [SHIFT K_B] [SHIFT K_P] [K_M] \\\n';
      const line6 = '                       [K_Y] [K_R] [K_L] [K_V] [K_S] [K_H] [SHIFT K_L] [SHIFT K_G] \\\n';
      const line7 = '                       [RALT K_K] [RALT K_B]';
      const str = `${line1}${line2}${line3}${line4}${line5}${line6}${line7}`;
      Rule.tokenBuffer = stringToTokenBuffer(str);
      const variableStoreAssign: Rule = new VariableStoreAssignRule();
      assert.isTrue(variableStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      const virtualKeyNodes = storeNode.getChildrenOfType(NodeTypes.VIRTUAL_KEY);
      assert.equal(virtualKeyNodes.length, 35);
      const lineNodes = storeNode.getChildrenOfType(NodeTypes.LINE);
      assert.equal(lineNodes.length, 6);
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
  describe("SystemStoreNameRule Tests", () => {
    it("can construct a SystemStoreNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isNotNull(systemStoreName);
    });
    it("can parse correctly (bitmap)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('bitmap');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isTrue(systemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (casedkeys)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('casedkeys');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isTrue(systemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.CASEDKEYS));
    });
    it("can parse correctly (hotkey)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('hotkey');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isTrue(systemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.HOTKEY));
    });
    it("can parse correctly (layer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isTrue(systemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYER));
    });
    it("can parse correctly (newLayer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('newLayer');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isTrue(systemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NEWLAYER));
    });
    it("can parse correctly (oldLayer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('oldLayer');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isTrue(systemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.OLDLAYER));
    });
  });
  describe("PermittedKeywordRule Tests", () => {
    it("can construct a PermittedKeywordRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const permittedKeyword: Rule = new PermittedKeywordRule();
      assert.isNotNull(permittedKeyword);
    });
    it("can parse correctly (newcontext)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('newcontext');
      const permittedKeyword: Rule = new PermittedKeywordRule();
      assert.isTrue(permittedKeyword.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NEWCONTEXT));
    });
    it("can parse correctly (postkeystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('postkeystroke');
      const permittedKeyword: Rule = new PermittedKeywordRule();
      assert.isTrue(permittedKeyword.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.POSTKEYSTROKE));
    });
  });
});