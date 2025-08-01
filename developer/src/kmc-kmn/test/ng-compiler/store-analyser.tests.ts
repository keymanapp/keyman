/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 *
 * System and Normal Store Rule Tests
 */

import 'mocha';
import { assert } from 'chai';
import { NodeTypes } from "../../src/ng-compiler/node-types.js";
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { Rule } from '../../src/ng-compiler/recursive-descent.js';
import { stringToTokenBuffer } from './kmn-analyser.tests.js';
import { CapsAlwaysOffRule, CapsOnOnlyRule, HeaderAssignRule, HeaderNameRule, NormalStoreNameRule, ResetStoreRule, ShiftFreesCapsRule, StoreNameRule, SystemStoreNameForSetRule } from '../../src/ng-compiler/store-analyser.js';
import { SetSystemStoreRule, SetNormalStoreRule, SystemStoreAssignRule, SystemStoreNameRule } from '../../src/ng-compiler/store-analyser.js';
import { SystemStoreRule, NormalStoreAssignRule, NormalStoreRule } from '../../src/ng-compiler/store-analyser.js';

let root: ASTNode = null;

describe("KMN Store Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeTypes.TMP);
  });
  describe("SystemStoreAssignRule Tests", () => {
    it("can construct a SystemStoreAssignRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isNotNull(systemStoreAssign);
    });
    it("can parse bitmap store correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse bitmap store correctly (with continuation)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap)\\\n"filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      const bitmapNode = root.getSoleChildOfType(NodeTypes.BITMAP);
      assert.isNotNull(bitmapNode);
      assert.isNotNull(bitmapNode.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse copyright store correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&copyright) "message"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.COPYRIGHT));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse includecodes store correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&includecodes) "filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.INCLUDECODES));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (casedkeys single virtual key)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      assert.isNotNull(casedkeysNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (casedkeys two virtual keys)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A] [K_B]');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const children = casedkeysNode.getChildrenOfType(NodeTypes.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].getSoleChild().getText(), 'K_B');
    });
    it("can parse correctly (casedkeys virtual key range)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]..[K_C]');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const rangeNode = casedkeysNode.getSoleChildOfType(NodeTypes.RANGE);
      const children  = rangeNode.getChildrenOfType(NodeTypes.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].getSoleChild().getText(), 'K_C');
    });
    it("can parse correctly (casedkeys character range)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&casedkeys) "a".."c"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      const casedkeysNode = root.getSoleChildOfType(NodeTypes.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const rangeNode = casedkeysNode.getSoleChildOfType(NodeTypes.RANGE);
      const children  = rangeNode.getChildrenOfType(NodeTypes.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (hotkey)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&hotkey) [K_A]');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      const hotkeyNode = root.getSoleChildOfType(NodeTypes.HOTKEY)
      assert.isNotNull(hotkeyNode);
      assert.isNotNull(hotkeyNode.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (no text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap)');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
  });
  describe("SystemStoreRule Tests", () => {
    it("can construct a SystemStoreRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const systemStore: Rule = new SystemStoreRule();
      assert.isNotNull(systemStore);
    });
    it("can parse correctly (bitmap)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap)');
      const systemStore: Rule = new SystemStoreRule();
      assert.isTrue(systemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace after left bracket)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store( &bitmap)');
      const systemStore: Rule = new SystemStoreRule();
      assert.isTrue(systemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace before right bracket)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap )');
      const systemStore: Rule = new SystemStoreRule();
      assert.isTrue(systemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace after left and before right brackets)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store( &bitmap )');
      const systemStore: Rule = new SystemStoreRule();
      assert.isTrue(systemStore.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
  });
  describe("SystemStoreNameRule Tests", () => {
    it("can construct a SystemStoreNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isNotNull(systemStoreName);
    });
    it("can parse correctly", () => {
      [
        {code: 'bitmap',           nodeType: NodeTypes.BITMAP},
        {code: 'casedkeys',        nodeType: NodeTypes.CASEDKEYS},
        {code: 'copyright',        nodeType: NodeTypes.COPYRIGHT},
        {code: 'displaymap',       nodeType: NodeTypes.DISPLAYMAP},
        {code: 'ethnologuecode',   nodeType: NodeTypes.ETHNOLOGUECODE},
        {code: 'hotkey',           nodeType: NodeTypes.HOTKEY},
        {code: 'includecodes',     nodeType: NodeTypes.INCLUDECODES},
        {code: 'keyboardversion',  nodeType: NodeTypes.KEYBOARDVERSION},
        {code: 'kmw_embedcss',     nodeType: NodeTypes.KMW_EMBEDCSS},
        {code: 'kmw_embedjs',      nodeType: NodeTypes.KMW_EMBEDJS},
        {code: 'kmw_helpfile',     nodeType: NodeTypes.KMW_HELPFILE},
        {code: 'kmw_helptext',     nodeType: NodeTypes.KMW_HELPTEXT},
        {code: 'kmw_rtl',          nodeType: NodeTypes.KMW_RTL},
        {code: 'language',         nodeType: NodeTypes.LANGUAGE},
        {code: 'layoutfile',       nodeType: NodeTypes.LAYOUTFILE},
        {code: 'message',          nodeType: NodeTypes.MESSAGE},
        {code: 'mnemoniclayout',   nodeType: NodeTypes.MNEMONICLAYOUT},
        {code: 'name',             nodeType: NodeTypes.NAME},
        {code: 'targets',          nodeType: NodeTypes.TARGETS},
        {code: 'version',          nodeType: NodeTypes.VERSION},
        {code: 'visualkeyboard',   nodeType: NodeTypes.VISUALKEYBOARD},
        {code: 'windowslanguages', nodeType: NodeTypes.WINDOWSLANGUAGES},
        {code: 'capsalwaysoff',    nodeType: NodeTypes.CAPSALWAYSOFF},
        {code: 'capsononly',       nodeType: NodeTypes.CAPSONONLY},
        {code: 'shiftfreescaps',   nodeType: NodeTypes.SHIFTFREESCAPS},
      ].forEach((testCase) => {
        Rule.tokenBuffer = stringToTokenBuffer(`&${testCase.code}`);
        const systemStoreName: Rule = new SystemStoreNameRule();
        root = new ASTNode(NodeTypes.TMP);
        assert.isTrue(systemStoreName.parse(root));
        assert.isNotNull(root.getSoleChildOfType(testCase.nodeType));
      });
    });
  });
  describe("NormalStoreAssignRule Tests", () => {
    it("can construct a NormalStoreAssignRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isNotNull(normalStoreAssign);
    });
    it("can parse correctly (single u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (single outs)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(ShiftOutAll)  outs(ShiftOutSingle)');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.OUTS));
    });
    it("can parse correctly (two u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 U+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\nU+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation, space before)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\\nU+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation, space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\n U+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation, spaces before and after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\\n U+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation, spaces before, after and between)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\ \n U+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (mixed text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) " " U+1781 [K_K] outs(ShiftOutSingle)');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
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
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      const virtualKeyNodes = storeNode.getChildrenOfType(NodeTypes.VIRTUAL_KEY);
      assert.equal(virtualKeyNodes.length, 35);
    });
    it("can parse correctly (no text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_key)');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.STORE));
    });
  });
  describe("NormalStoreRule Tests", () => {
    it("can construct a NormalStoreRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const normalStore: Rule = new NormalStoreRule();
      assert.isNotNull(normalStore);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(digit)');
      const normalStore: Rule = new NormalStoreRule();
      assert.isTrue(normalStore.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE);
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("NormalStoreNameRule Tests", () => {
    it("can construct a NormalStoreNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isNotNull(normalStoreName);
    });
    it("can parse correctly (parameter)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('main');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isTrue(normalStoreName.parse(root));
      assert.equal(root.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'main');
    });
    it("can parse correctly (octal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('1');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isTrue(normalStoreName.parse(root));
      assert.equal(root.getSoleChildOfType(NodeTypes.STORENAME).getText(), '1');
    });
    it("can parse correctly (permitted keyword)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('newcontext');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isTrue(normalStoreName.parse(root));
      assert.equal(root.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'newcontext');
    });
    it("can parse correctly (multi word name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('Unicode Area');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isTrue(normalStoreName.parse(root));
      const normalStoreNameNode = root.getSoleChildOfType(NodeTypes.STORENAME);
      assert.isNotNull(normalStoreNameNode);
      assert.equal(normalStoreNameNode.getSoleChildOfType(NodeTypes.UNICODE).getText(), 'Unicode');
      assert.equal(normalStoreNameNode.getSoleChildOfType(NodeTypes.PARAMETER).getText(), 'Area');
    });
  });
  describe("StoreNameRule Tests", () => {
    it("can construct a StoreNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const storeName: Rule = new StoreNameRule();
      assert.isNotNull(storeName);
    });
  });
  describe("SetNormalStoreRule Tests", () => {
    it("can construct a SetNormalStoreRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const setNormalStore: Rule = new SetNormalStoreRule();
      assert.isNotNull(setNormalStore);
    });
    it("can parse correctly (string)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('set(storeName = "value")');
      const setNormalStore: Rule = new SetNormalStoreRule();
      assert.isTrue(setNormalStore.parse(root));
      const setNode = root.getSoleChildOfType(NodeTypes.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.STORENAME));
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (two u_chars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('set(storeName = U+1780 U+1781)');
      const setNormalStore: Rule = new SetNormalStoreRule();
      assert.isTrue(setNormalStore.parse(root));
      const setNode = root.getSoleChildOfType(NodeTypes.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.STORENAME));
      assert.equal(setNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
  });
  describe("SetSystemStoreRule Tests", () => {
    it("can construct a SetSystemStoreRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const SetSystemStore: Rule = new SetSystemStoreRule();
      assert.isNotNull(SetSystemStore);
    });
    it("can parse correctly (layer, string)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('set(&layer = "value")');
      const SetSystemStore: Rule = new SetSystemStoreRule();
      assert.isTrue(SetSystemStore.parse(root));
      const setNode = root.getSoleChildOfType(NodeTypes.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.LAYER));
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (layer, two u_chars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('set(&layer = U+1780 U+1781)');
      const SetSystemStore: Rule = new SetSystemStoreRule();
      assert.isTrue(SetSystemStore.parse(root));
      const setNode = root.getSoleChildOfType(NodeTypes.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.LAYER));
      assert.equal(setNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
    });
    it("can parse correctly (bitmap)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('set(&bitmap = "value")');
      const SetSystemStore: Rule = new SetSystemStoreRule();
      assert.isTrue(SetSystemStore.parse(root));
      const setNode = root.getSoleChildOfType(NodeTypes.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.BITMAP));
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.STRING));
    });
  });
  describe("SystemStoreNameForSetRule Tests", () => {
    it("can construct a SSystemStoreNameForSetRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const systemStoreNameForSet: Rule = new SystemStoreNameForSetRule();
      assert.isNotNull(systemStoreNameForSet);
    });
    it("can parse correctly (string system store name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&bitmap');
      const systemStoreNameForSet: Rule = new SystemStoreNameForSetRule();
      assert.isTrue(systemStoreNameForSet.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (layer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&layer');
      const systemStoreNameForSet: Rule = new SystemStoreNameForSetRule();
      assert.isTrue(systemStoreNameForSet.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYER));
    });
  });
  describe("ResetStoreRule Tests", () => {
    it("can construct a ResetStoreRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const resetStore: Rule = new ResetStoreRule();
      assert.isNotNull(resetStore);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('reset(digit)');
      const resetStore: Rule = new ResetStoreRule();
      assert.isTrue(resetStore.parse(root));
      const resetNode = root.getSoleChildOfType(NodeTypes.RESET);
      assert.isNotNull(resetNode);
      assert.isNotNull(resetNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
  });
  describe("CapsAlwaysOffRule Tests", () => {
    it("can construct a CapsAlwaysOffRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const capsAlwaysOff: Rule = new CapsAlwaysOffRule();
      assert.isNotNull(capsAlwaysOff);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('caps always off');
      const capsAlwaysOff: Rule = new CapsAlwaysOffRule();
      assert.isTrue(capsAlwaysOff.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.CAPSALWAYSOFF));
    });
  });
  describe("CapsOnOnlyRule Tests", () => {
    it("can construct a CapsOnOnlyRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const capsOnOnly: Rule = new CapsOnOnlyRule();
      assert.isNotNull(capsOnOnly);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('caps on only');
      const capsOnOnly: Rule = new CapsOnOnlyRule();
      assert.isTrue(capsOnOnly.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.CAPSONONLY));
    });
  });
  describe("ShiftFreesCapsRule Tests", () => {
    it("can construct a ShiftFreesCapsRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const shiftFreesCaps: Rule = new ShiftFreesCapsRule();
      assert.isNotNull(shiftFreesCaps);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('shift frees caps');
      const shiftFreesCaps: Rule = new ShiftFreesCapsRule();
      assert.isTrue(shiftFreesCaps.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.SHIFTFREESCAPS));
    });
  });
  describe("HeaderAssignRule Tests", () => {
    it("can construct a HeaderAssignRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const systemStoreName: Rule = new HeaderAssignRule();
      assert.isNotNull(systemStoreName);
    });
    it("can parse correctly (value is string)", () => {
      [
        {code: 'bitmap',    nodeType: NodeTypes.BITMAP_HEADER},
        {code: 'copyright', nodeType: NodeTypes.COPYRIGHT_HEADER},
        {code: 'hotkey',    nodeType: NodeTypes.HOTKEY_HEADER},
        {code: 'language',  nodeType: NodeTypes.LANGUAGE_HEADER},
        {code: 'layout',    nodeType: NodeTypes.LAYOUT_HEADER},
        {code: 'message',   nodeType: NodeTypes.MESSAGE_HEADER},
        {code: 'name',      nodeType: NodeTypes.NAME_HEADER},
        {code: 'version',   nodeType: NodeTypes.VERSION_HEADER},
      ].forEach((testCase) => {
        Rule.tokenBuffer = stringToTokenBuffer(`${testCase.code} "value"`);
        const headerName: Rule = new HeaderAssignRule();
        root = new ASTNode(NodeTypes.TMP);
        assert.isTrue(headerName.parse(root));
        const headerNode = root.getSoleChildOfType(testCase.nodeType);
        assert.equal(headerNode.getSoleChildOfType(NodeTypes.STRING).getText(), '"value"');
      });
    });
    it("can parse correctly (version with value as float)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('VERSION 10.0');
      const headerName: Rule = new HeaderAssignRule();
      assert.isTrue(headerName.parse(root));
      const versionNode = root.getSoleChildOfType(NodeTypes.VERSION_HEADER);
      assert.equal(versionNode.getSoleChildOfType(NodeTypes.PARAMETER).getText(), '10.0')
    });
    it.skip("can parse correctly (no value)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('VERSION');
      const headerName: Rule = new HeaderAssignRule();
      assert.isTrue(headerName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VERSION_HEADER));
    });
  });
  describe("HeaderNameRule Tests", () => {
    it("can construct a HeaderNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const systemStoreName: Rule = new HeaderNameRule();
      assert.isNotNull(systemStoreName);
    });
    it("can parse correctly", () => {
      [
        {code: 'bitmap',    nodeType: NodeTypes.BITMAP_HEADER},
        {code: 'copyright', nodeType: NodeTypes.COPYRIGHT_HEADER},
        {code: 'hotkey',    nodeType: NodeTypes.HOTKEY_HEADER},
        {code: 'language',  nodeType: NodeTypes.LANGUAGE_HEADER},
        {code: 'layout',    nodeType: NodeTypes.LAYOUT_HEADER},
        {code: 'message',   nodeType: NodeTypes.MESSAGE_HEADER},
        {code: 'name',      nodeType: NodeTypes.NAME_HEADER},
        {code: 'version',   nodeType: NodeTypes.VERSION_HEADER},
      ].forEach((testCase) => {
        Rule.tokenBuffer = stringToTokenBuffer(`${testCase.code} `);
        const headerName: Rule = new HeaderNameRule();
        root = new ASTNode(NodeTypes.TMP);
        assert.isTrue(headerName.parse(root));
        assert.isNotNull(root.getSoleChildOfType(testCase.nodeType));
      });
    });
  });
});
