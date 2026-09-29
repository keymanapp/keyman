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
import { NodeType } from "../../src/ng-compiler/node-type.js";
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { Rule } from '../../src/ng-compiler/recursive-descent.js';
import { stringToTokenBuffer } from './kmn-analyzer.tests.js';
import { CapsAlwaysOffRule, CapsOnOnlyRule, DeadkeyNameRule, HeaderAssignRule, HeaderNameRule, NormalStoreNameRule } from '../../src/ng-compiler/store-analyzer.js';
import { NormalStoreAssignRule, NormalStoreRule, ResetStoreRule, SetNormalStoreRule, SetSystemStoreRule } from '../../src/ng-compiler/store-analyzer.js';
import { ShiftFreesCapsRule, StoreNameRule, SystemStoreAssignRule, SystemStoreNameForSetRule } from '../../src/ng-compiler/store-analyzer.js';
import { SystemStoreNameRule, SystemStoreRule } from '../../src/ng-compiler/store-analyzer.js';
import { TokenBuffer } from './token-buffer.js';

let tokenBuffer: TokenBuffer = null;
let root: ASTNode = null;

describe("KMN Store Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode();
  });
  describe("SystemStoreAssignRule Tests", () => {
    it("can construct a SystemStoreAssignRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isNotNull(systemStoreAssign);
    });
    it("can parse bitmap store correctly", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeType.STRING));
    });
    it("can parse bitmap store correctly (with continuation)", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap)\\\n"filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      const bitmapNode = root.getSoleChildOfType(NodeType.BITMAP);
      assert.isNotNull(bitmapNode);
      assert.isNotNull(bitmapNode.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse copyright store correctly", () => {
      tokenBuffer = stringToTokenBuffer('store(&copyright) "message"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.COPYRIGHT));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeType.STRING));
    });
    it("can parse includecodes store correctly", () => {
      tokenBuffer = stringToTokenBuffer('store(&includecodes) "filename"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.INCLUDECODES));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (casedkeys single virtual key)", () => {
      tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      const casedkeysNode = root.getSoleChildOfType(NodeType.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      assert.isNotNull(casedkeysNode.getSoleChildOfType(NodeType.VIRTUAL_KEY));
    });
    it("can parse correctly (casedkeys two virtual keys)", () => {
      tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A] [K_B]');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      const casedkeysNode = root.getSoleChildOfType(NodeType.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const children = casedkeysNode.getChildrenOfType(NodeType.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].getSoleChild().getText(), 'K_B');
    });
    it("can parse correctly (casedkeys virtual key range)", () => {
      tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]..[K_C]');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      const casedkeysNode = root.getSoleChildOfType(NodeType.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const rangeNode = casedkeysNode.getSoleChildOfType(NodeType.RANGE);
      const children  = rangeNode.getChildrenOfType(NodeType.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].getSoleChild().getText(), 'K_C');
    });
    it("can parse correctly (casedkeys character range)", () => {
      tokenBuffer = stringToTokenBuffer('store(&casedkeys) "a".."c"');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      const casedkeysNode = root.getSoleChildOfType(NodeType.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      const rangeNode = casedkeysNode.getSoleChildOfType(NodeType.RANGE);
      const children  = rangeNode.getChildrenOfType(NodeType.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (hotkey)", () => {
      tokenBuffer = stringToTokenBuffer('store(&hotkey) [K_A]');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      const hotkeyNode = root.getSoleChildOfType(NodeType.HOTKEY)
      assert.isNotNull(hotkeyNode);
      assert.isNotNull(hotkeyNode.getSoleChildOfType(NodeType.VIRTUAL_KEY));
    });
    it("can parse correctly (no text)", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap)');
      const systemStoreAssign: Rule = new SystemStoreAssignRule();
      assert.isTrue(systemStoreAssign.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
    });
  });
  describe("SystemStoreRule Tests", () => {
    it("can construct a SystemStoreRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const systemStore: Rule = new SystemStoreRule();
      assert.isNotNull(systemStore);
    });
    it("can parse correctly (bitmap)", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap)');
      const systemStore: Rule = new SystemStoreRule();
      assert.isTrue(systemStore.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace after left bracket)", () => {
      tokenBuffer = stringToTokenBuffer('store( &bitmap)');
      const systemStore: Rule = new SystemStoreRule();
      assert.isTrue(systemStore.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace before right bracket)", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap )');
      const systemStore: Rule = new SystemStoreRule();
      assert.isTrue(systemStore.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
    });
    it("can parse correctly (bitmap, whitespace after left and before right brackets)", () => {
      tokenBuffer = stringToTokenBuffer('store( &bitmap )');
      const systemStore: Rule = new SystemStoreRule();
      assert.isTrue(systemStore.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
    });
  });
  describe("SystemStoreNameRule Tests", () => {
    it("can construct a SystemStoreNameRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const systemStoreName: Rule = new SystemStoreNameRule();
      assert.isNotNull(systemStoreName);
    });
    it("can parse correctly", () => {
      [
        {code: 'bitmap',           nodeType: NodeType.BITMAP},
        {code: 'casedkeys',        nodeType: NodeType.CASEDKEYS},
        {code: 'copyright',        nodeType: NodeType.COPYRIGHT},
        {code: 'displaymap',       nodeType: NodeType.DISPLAYMAP},
        {code: 'ethnologuecode',   nodeType: NodeType.ETHNOLOGUECODE},
        {code: 'hotkey',           nodeType: NodeType.HOTKEY},
        {code: 'includecodes',     nodeType: NodeType.INCLUDECODES},
        {code: 'keyboardversion',  nodeType: NodeType.KEYBOARDVERSION},
        {code: 'kmw_embedcss',     nodeType: NodeType.KMW_EMBEDCSS},
        {code: 'kmw_embedjs',      nodeType: NodeType.KMW_EMBEDJS},
        {code: 'kmw_helpfile',     nodeType: NodeType.KMW_HELPFILE},
        {code: 'kmw_helptext',     nodeType: NodeType.KMW_HELPTEXT},
        {code: 'kmw_rtl',          nodeType: NodeType.KMW_RTL},
        {code: 'language',         nodeType: NodeType.LANGUAGE},
        {code: 'layoutfile',       nodeType: NodeType.LAYOUTFILE},
        {code: 'message',          nodeType: NodeType.MESSAGE},
        {code: 'mnemoniclayout',   nodeType: NodeType.MNEMONICLAYOUT},
        {code: 'name',             nodeType: NodeType.NAME},
        {code: 'targets',          nodeType: NodeType.TARGETS},
        {code: 'version',          nodeType: NodeType.VERSION},
        {code: 'visualkeyboard',   nodeType: NodeType.VISUALKEYBOARD},
        {code: 'windowslanguages', nodeType: NodeType.WINDOWSLANGUAGES},
        {code: 'capsalwaysoff',    nodeType: NodeType.CAPSALWAYSOFF},
        {code: 'capsononly',       nodeType: NodeType.CAPSONONLY},
        {code: 'shiftfreescaps',   nodeType: NodeType.SHIFTFREESCAPS},
      ].forEach((testCase) => {
        tokenBuffer = stringToTokenBuffer(`&${testCase.code}`);
        const systemStoreName: Rule = new SystemStoreNameRule();
        root = new ASTNode();
        assert.isTrue(systemStoreName.parse(tokenBuffer, root));
        assert.isNotNull(root.getSoleChildOfType(testCase.nodeType));
      });
    });
  });
  describe("NormalStoreAssignRule Tests", () => {
    it("can construct a NormalStoreAssignRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isNotNull(normalStoreAssign);
    });
    it("can parse correctly (single u_char)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeType.U_CHAR));
    });
    it("can parse correctly (single outs)", () => {
      tokenBuffer = stringToTokenBuffer('store(ShiftOutAll)  outs(ShiftOutSingle)');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeType.OUTS));
    });
    it("can parse correctly (two u_char)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 U+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\nU+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation, space before)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\\nU+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation, space after)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\n U+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation, spaces before and after)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\\n U+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
    it("can parse correctly (two u_char with continuation, spaces before, after and between)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 \\ \n U+1781');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.equal(storeNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
    it("can parse correctly (mixed text)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) " " U+1781 [K_K] outs(ShiftOutSingle)');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeType.STRING));
      assert.isNotNull(storeNode.getSoleChildOfType(NodeType.U_CHAR));
      assert.isNotNull(storeNode.getSoleChildOfType(NodeType.VIRTUAL_KEY));
      assert.isNotNull(storeNode.getSoleChildOfType(NodeType.OUTS));
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
      tokenBuffer = stringToTokenBuffer(str);
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      const virtualKeyNodes = storeNode.getChildrenOfType(NodeType.VIRTUAL_KEY);
      assert.equal(virtualKeyNodes.length, 35);
    });
    it("can parse correctly (no text)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_key)');
      const normalStoreAssign: Rule = new NormalStoreAssignRule();
      assert.isTrue(normalStoreAssign.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.STORE));
    });
  });
  describe("NormalStoreRule Tests", () => {
    it("can construct a NormalStoreRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const normalStore: Rule = new NormalStoreRule();
      assert.isNotNull(normalStore);
    });
    it("can parse correctly", () => {
      tokenBuffer = stringToTokenBuffer('store(digit)');
      const normalStore: Rule = new NormalStoreRule();
      assert.isTrue(normalStore.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE);
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeType.STORENAME));
    });
  });
  describe("NormalStoreNameRule Tests", () => {
    it("can construct a NormalStoreNameRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isNotNull(normalStoreName);
    });
    it("can parse correctly (parameter)", () => {
      tokenBuffer = stringToTokenBuffer('main');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isTrue(normalStoreName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.STORENAME).getText(), 'main');
    });
    it("can parse correctly (octal)", () => {
      tokenBuffer = stringToTokenBuffer('1');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isTrue(normalStoreName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.STORENAME).getText(), '1');
    });
    it("can parse correctly (permitted keyword)", () => {
      tokenBuffer = stringToTokenBuffer('newcontext');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isTrue(normalStoreName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.STORENAME).getText(), 'newcontext');
    });
    it("can parse correctly (multi word name)", () => {
      tokenBuffer = stringToTokenBuffer('Unicode Area');
      const normalStoreName: Rule = new NormalStoreNameRule();
      assert.isTrue(normalStoreName.parse(tokenBuffer, root));
      const normalStoreNameNode = root.getSoleChildOfType(NodeType.STORENAME);
      assert.isNotNull(normalStoreNameNode);
      assert.equal(normalStoreNameNode.getSoleChildOfType(NodeType.UNICODE).getText(), 'Unicode');
      assert.equal(normalStoreNameNode.getSoleChildOfType(NodeType.PARAMETER).getText(), 'Area');
    });
  });
  describe("DeadkeyNameRule Tests", () => {
    it("can construct a DeadkeyNameRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const deadkeyName: Rule = new DeadkeyNameRule();
      assert.isNotNull(deadkeyName);
    });
    it("can parse correctly (parameter)", () => {
      tokenBuffer = stringToTokenBuffer('main');
      const deadkeyName: Rule = new DeadkeyNameRule();
      assert.isTrue(deadkeyName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.DEADKEYNAME).getText(), 'main');
    });
    it("can parse correctly (octal)", () => {
      tokenBuffer = stringToTokenBuffer('1');
      const deadkeyName: Rule = new DeadkeyNameRule();
      assert.isTrue(deadkeyName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.DEADKEYNAME).getText(), '1');
    });
    it("can parse correctly (permitted keyword)", () => {
      tokenBuffer = stringToTokenBuffer('newcontext');
      const deadkeyName: Rule = new DeadkeyNameRule();
      assert.isTrue(deadkeyName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.DEADKEYNAME).getText(), 'newcontext');
    });
    it("can parse correctly (multi word name)", () => {
      tokenBuffer = stringToTokenBuffer('Unicode Area');
      const deadkeyName: Rule = new DeadkeyNameRule();
      assert.isTrue(deadkeyName.parse(tokenBuffer, root));
      const deadkeyNameNode = root.getSoleChildOfType(NodeType.DEADKEYNAME);
      assert.isNotNull(deadkeyNameNode);
      assert.equal(deadkeyNameNode.getSoleChildOfType(NodeType.UNICODE).getText(), 'Unicode');
      assert.equal(deadkeyNameNode.getSoleChildOfType(NodeType.PARAMETER).getText(), 'Area');
    });
  });
  describe("StoreNameRule Tests", () => {
    it("can construct a StoreNameRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const storeName: Rule = new StoreNameRule();
      assert.isNotNull(storeName);
    });
  });
  describe("SetNormalStoreRule Tests", () => {
    it("can construct a SetNormalStoreRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const setNormalStore: Rule = new SetNormalStoreRule();
      assert.isNotNull(setNormalStore);
    });
    it("can parse correctly (string)", () => {
      tokenBuffer = stringToTokenBuffer('set(storeName = "value")');
      const setNormalStore: Rule = new SetNormalStoreRule();
      assert.isTrue(setNormalStore.parse(tokenBuffer, root));
      const setNode = root.getSoleChildOfType(NodeType.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.STORENAME));
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (two u_chars)", () => {
      tokenBuffer = stringToTokenBuffer('set(storeName = U+1780 U+1781)');
      const setNormalStore: Rule = new SetNormalStoreRule();
      assert.isTrue(setNormalStore.parse(tokenBuffer, root));
      const setNode = root.getSoleChildOfType(NodeType.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.STORENAME));
      assert.equal(setNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
  });
  describe("SetSystemStoreRule Tests", () => {
    it("can construct a SetSystemStoreRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const SetSystemStore: Rule = new SetSystemStoreRule();
      assert.isNotNull(SetSystemStore);
    });
    it("can parse correctly (layer, string)", () => {
      tokenBuffer = stringToTokenBuffer('set(&layer = "value")');
      const SetSystemStore: Rule = new SetSystemStoreRule();
      assert.isTrue(SetSystemStore.parse(tokenBuffer, root));
      const setNode = root.getSoleChildOfType(NodeType.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.LAYER));
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (layer, two u_chars)", () => {
      tokenBuffer = stringToTokenBuffer('set(&layer = U+1780 U+1781)');
      const SetSystemStore: Rule = new SetSystemStoreRule();
      assert.isTrue(SetSystemStore.parse(tokenBuffer, root));
      const setNode = root.getSoleChildOfType(NodeType.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.LAYER));
      assert.equal(setNode.getChildrenOfType(NodeType.U_CHAR).length, 2);
    });
    it("can parse correctly (bitmap)", () => {
      tokenBuffer = stringToTokenBuffer('set(&bitmap = "value")');
      const SetSystemStore: Rule = new SetSystemStoreRule();
      assert.isTrue(SetSystemStore.parse(tokenBuffer, root));
      const setNode = root.getSoleChildOfType(NodeType.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.BITMAP));
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.STRING));
    });
  });
  describe("SystemStoreNameForSetRule Tests", () => {
    it("can construct a SSystemStoreNameForSetRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const systemStoreNameForSet: Rule = new SystemStoreNameForSetRule();
      assert.isNotNull(systemStoreNameForSet);
    });
    it("can parse correctly (string system store name)", () => {
      tokenBuffer = stringToTokenBuffer('&bitmap');
      const systemStoreNameForSet: Rule = new SystemStoreNameForSetRule();
      assert.isTrue(systemStoreNameForSet.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
    });
    it("can parse correctly (layer)", () => {
      tokenBuffer = stringToTokenBuffer('&layer');
      const systemStoreNameForSet: Rule = new SystemStoreNameForSetRule();
      assert.isTrue(systemStoreNameForSet.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.LAYER));
    });
  });
  describe("ResetStoreRule Tests", () => {
    it("can construct a ResetStoreRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const resetStore: Rule = new ResetStoreRule();
      assert.isNotNull(resetStore);
    });
    it("can parse correctly", () => {
      tokenBuffer = stringToTokenBuffer('reset(digit)');
      const resetStore: Rule = new ResetStoreRule();
      assert.isTrue(resetStore.parse(tokenBuffer, root));
      const resetNode = root.getSoleChildOfType(NodeType.RESET);
      assert.isNotNull(resetNode);
      assert.isNotNull(resetNode.getSoleChildOfType(NodeType.STORENAME));
    });
  });
  describe("CapsAlwaysOffRule Tests", () => {
    it("can construct a CapsAlwaysOffRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const capsAlwaysOff: Rule = new CapsAlwaysOffRule();
      assert.isNotNull(capsAlwaysOff);
    });
    it("can parse correctly", () => {
      tokenBuffer = stringToTokenBuffer('caps always off');
      const capsAlwaysOff: Rule = new CapsAlwaysOffRule();
      assert.isTrue(capsAlwaysOff.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.CAPSALWAYSOFF));
    });
  });
  describe("CapsOnOnlyRule Tests", () => {
    it("can construct a CapsOnOnlyRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const capsOnOnly: Rule = new CapsOnOnlyRule();
      assert.isNotNull(capsOnOnly);
    });
    it("can parse correctly", () => {
      tokenBuffer = stringToTokenBuffer('caps on only');
      const capsOnOnly: Rule = new CapsOnOnlyRule();
      assert.isTrue(capsOnOnly.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.CAPSONONLY));
    });
  });
  describe("ShiftFreesCapsRule Tests", () => {
    it("can construct a ShiftFreesCapsRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const shiftFreesCaps: Rule = new ShiftFreesCapsRule();
      assert.isNotNull(shiftFreesCaps);
    });
    it("can parse correctly", () => {
      tokenBuffer = stringToTokenBuffer('shift frees caps');
      const shiftFreesCaps: Rule = new ShiftFreesCapsRule();
      assert.isTrue(shiftFreesCaps.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.SHIFTFREESCAPS));
    });
  });
  describe("HeaderAssignRule Tests", () => {
    it("can construct a HeaderAssignRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const systemStoreName: Rule = new HeaderAssignRule();
      assert.isNotNull(systemStoreName);
    });
    it("can parse correctly (value is string)", () => {
      [
        {code: 'bitmap',    nodeType: NodeType.BITMAP_HEADER},
        {code: 'copyright', nodeType: NodeType.COPYRIGHT_HEADER},
        {code: 'hotkey',    nodeType: NodeType.HOTKEY_HEADER},
        {code: 'language',  nodeType: NodeType.LANGUAGE_HEADER},
        {code: 'layout',    nodeType: NodeType.LAYOUT_HEADER},
        {code: 'message',   nodeType: NodeType.MESSAGE_HEADER},
        {code: 'name',      nodeType: NodeType.NAME_HEADER},
        {code: 'version',   nodeType: NodeType.VERSION_HEADER},
      ].forEach((testCase) => {
        tokenBuffer = stringToTokenBuffer(`${testCase.code} "value"`);
        const headerName: Rule = new HeaderAssignRule();
        root = new ASTNode();
        assert.isTrue(headerName.parse(tokenBuffer, root));
        const headerNode = root.getSoleChildOfType(testCase.nodeType);
        assert.equal(headerNode.getSoleChildOfType(NodeType.STRING).getText(), '"value"');
      });
    });
    it("can parse correctly (version with value as float)", () => {
      tokenBuffer = stringToTokenBuffer('VERSION 10.0');
      const headerName: Rule = new HeaderAssignRule();
      assert.isTrue(headerName.parse(tokenBuffer, root));
      const versionNode = root.getSoleChildOfType(NodeType.VERSION_HEADER);
      assert.equal(versionNode.getSoleChildOfType(NodeType.PARAMETER).getText(), '10.0')
    });
    it.skip("can parse correctly (no value)", () => {
      tokenBuffer = stringToTokenBuffer('VERSION');
      const headerName: Rule = new HeaderAssignRule();
      assert.isTrue(headerName.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.VERSION_HEADER));
    });
  });
  describe("HeaderNameRule Tests", () => {
    it("can construct a HeaderNameRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const systemStoreName: Rule = new HeaderNameRule();
      assert.isNotNull(systemStoreName);
    });
    it("can parse correctly", () => {
      [
        {code: 'bitmap',    nodeType: NodeType.BITMAP_HEADER},
        {code: 'copyright', nodeType: NodeType.COPYRIGHT_HEADER},
        {code: 'hotkey',    nodeType: NodeType.HOTKEY_HEADER},
        {code: 'language',  nodeType: NodeType.LANGUAGE_HEADER},
        {code: 'layout',    nodeType: NodeType.LAYOUT_HEADER},
        {code: 'message',   nodeType: NodeType.MESSAGE_HEADER},
        {code: 'name',      nodeType: NodeType.NAME_HEADER},
        {code: 'version',   nodeType: NodeType.VERSION_HEADER},
      ].forEach((testCase) => {
        tokenBuffer = stringToTokenBuffer(`${testCase.code} `);
        const headerName: Rule = new HeaderNameRule();
        root = new ASTNode();
        assert.isTrue(headerName.parse(tokenBuffer, root));
        assert.isNotNull(root.getSoleChildOfType(testCase.nodeType));
      });
    });
  });
});
