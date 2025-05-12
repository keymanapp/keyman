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
import { AnyStatementRule, BaselayoutStatementRule, BeginBlockRule, BeginStatementRule, BlankLineRule, CallStatementRule, DeadKeyStatementRule, InputElementRule, NotAnyStatementRule, SaveStatementRule, ShiftCodeRule } from '../../src/ng-compiler/kmn-analyser.js';
import { BracketedGroupNameRule, BracketedStringRule, ComparisonRule, ContentRule, ContentLineRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ContextInputBlockRule, ContextProductionBlockRule, ContextStatementRule, ContinuationNewlineRule } from '../../src/ng-compiler/kmn-analyser.js';
import { EntryPointRule, GroupBlockRule, GroupStatementRule, GroupQualifierRule } from '../../src/ng-compiler/kmn-analyser.js';
import { IfLikeBlockRule, IfLikeStatementRule, IfStatementRule, IfStoreStoreStatementRule, IfStoreStringStatementRule } from '../../src/ng-compiler/kmn-analyser.js';
import { IfSystemStoreNameRule, IfSystemStoreStoreStatementRule, IfSystemStoreStringStatementRule, IndexStatementRule, InputContextRule } from '../../src/ng-compiler/kmn-analyser.js';
import { KeystrokeRule, KmnTreeRule, LayerStatementRule, LineRule, OutputBlockRule } from '../../src/ng-compiler/kmn-analyser.js';
import { OutputStatementRule, OutsStatementRule, PaddingRule, PlatformStatementRule, ReadOnlyInputBlockRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ReadOnlyLhsBlockRule, ReadOnlyProductionBlockRule, RhsBlockRule, SimpleTextRule, SpacedCommaRule } from '../../src/ng-compiler/kmn-analyser.js';
import { TextRangeRule, TextRule, UseStatementRule, UsingKeysInputBlockRule, UsingKeysLhsBlockRule } from '../../src/ng-compiler/kmn-analyser.js';
import { UsingKeysProductionBlockRule, UsingKeysRule, VirtualKeyRule } from '../../src/ng-compiler/kmn-analyser.js';
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
    it("can parse correctly (variable store assign, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 c a comment\n');
      const contentLine: Rule = new ContentLineRule();
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeTypes.STORE);
      assert.equal(children[0].getSoleChildOfType(NodeTypes.STORENAME).getText(), 'c_out');
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (variable store assign, continuation, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\nU+1781 c a comment\n');
      const contentLine: Rule = new ContentLineRule();
      assert.isTrue(contentLine.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeTypes.STORE);
      assert.equal(children[0].getSoleChildOfType(NodeTypes.STORENAME).getText(), 'c_out');
      assert.equal(children[0].getChildrenOfType(NodeTypes.U_CHAR).length, 2);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (multi line variable store assignment, comment)", () => {
      const line1 = 'store(c_out)           U+1780 U+1781 U+1782 U+1783 U+1784 \\\n';
      const line2 = '    	   	   	          U+1785 U+1786 U+1787 U+1788 U+1789 \\\n';
      const line3 = '    	   	   	          U+178A U+178B U+178C U+178D U+178E \\\n';
      const line4 = '    	   	   	          U+178F U+1790 U+1791 U+1792 U+1793 \\\n';
      const line5 = '    	   	   	          U+1794 U+1795 U+1796 U+1797 U+1798 \\\n';
      const line6 = '    	   	   	          U+1799 U+179A U+179B U+179C U+179F U+17A0 U+17A1 U+17A2 \\\n';
      const line7 = '    	   	   	          U+179D U+179E c deprecated, but they are used in minority languages\n';
      const str = `${line1}${line2}${line3}${line4}${line5}${line6}${line7}`;
      Rule.tokenBuffer = stringToTokenBuffer(str);
      const contentLine: Rule = new ContentLineRule();
      assert.isTrue(contentLine.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      const uCharNodes = storeNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 35);
      const lineNodes = storeNode.getChildrenOfType(NodeTypes.LINE);
      assert.equal(lineNodes.length, 6);
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
  describe("ShiftCodeRule Tests", () => {
    it("can construct a ShiftCodeRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const shiftCode: Rule = new ShiftCodeRule();
      assert.isNotNull(shiftCode);
    });
    it("can parse correctly", () => {
      [
        'shift',
        'caps',
        'ctrl',
        'lctrl',
        'rctrl',
        'alt',
        'lalt',
        'ralt',
        'ncaps',
      ].forEach((code) => {
        Rule.tokenBuffer = stringToTokenBuffer(`${code} `);
        const shiftCode: Rule = new ShiftCodeRule();
        root = new ASTNode(NodeTypes.TMP);
        assert.isTrue(shiftCode.parse(root));
        assert.equal(root.getSoleChildOfType(NodeTypes.SHIFT_CODE).getText(), code);
      });
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
  describe("BracketedGroupNameRule Tests", () => {
    it("can construct a BracketedGroupNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const bracketedGroupName: Rule = new BracketedGroupNameRule();
      assert.isNotNull(bracketedGroupName);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('(main)');
      const bracketedGroupName: Rule = new BracketedGroupNameRule();
      assert.isTrue(bracketedGroupName.parse(root));
      const groupNameNode: ASTNode = root.getSoleChildOfType(NodeTypes.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
    });
    it("can parse correctly (space before name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('( main)');
      const bracketedGroupName: Rule = new BracketedGroupNameRule();
      assert.isTrue(bracketedGroupName.parse(root));
      const groupNameNode: ASTNode = root.getSoleChildOfType(NodeTypes.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
    });
    it("can parse correctly (space after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('(main )');
      const bracketedGroupName: Rule = new BracketedGroupNameRule();
      assert.isTrue(bracketedGroupName.parse(root));
      const groupNameNode: ASTNode = root.getSoleChildOfType(NodeTypes.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
    });
    it("can parse correctly (space before and after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('( main )');
      const bracketedStoreName: Rule = new BracketedGroupNameRule();
      assert.isTrue(bracketedStoreName.parse(root));
      const groupNameNode: ASTNode = root.getSoleChildOfType(NodeTypes.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
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
  describe("UsingKeysProductionBlockRule Tests", () => {
    it("can construct a UsingKeysProductionBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const productionBlock: Rule = new UsingKeysProductionBlockRule();
      assert.isNotNull(productionBlock);
    });
    it("can parse correctly (plus, any, index)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ any(c_key) > index(c_out,1)');
      const productionBlock: Rule = new UsingKeysProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION_USING_KEYS);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS_USING_KEYS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.INDEX));
    });
    it("can parse correctly (plus, virtual key, two uChars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ [SHIFT K_A] > U+17B6 U+17C6');
      const productionBlock: Rule = new UsingKeysProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION_USING_KEYS);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS_USING_KEYS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      const uCharNodes = rhsNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 2);
    });
  });
  describe("ReadOnlyProductionBlockRule Tests", () => {
    it("can construct a ReadOnlyProductionBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const productionBlock: Rule = new ReadOnlyProductionBlockRule();
      assert.isNotNull(productionBlock);
    });
    it("can parse correctly (platform, use)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") > use(detectStartOfSentence)');
      const productionBlock: Rule = new ReadOnlyProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION_READONLY);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS_READONLY);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.PLATFORM));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.USE));
    });
    it("can parse correctly (ifs, any, context, layer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&newLayer = "") if(&layer = "shift") any(ShiftOutSingle) > context layer("default")');
      const productionBlock: Rule = new ReadOnlyProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION_READONLY);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS_READONLY);
      assert.isNotNull(lhsNode);
      const ifNodes = lhsNode.getChildrenOfType(NodeTypes.IF);
      assert.equal(ifNodes.length, 2);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.LAYER));
    });
  });
  describe("ContextProductionBlockRule Tests", () => {
    it("can construct a ContextProductionBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const productionBlock: Rule = new ContextProductionBlockRule();
      assert.isNotNull(productionBlock);
    });
    it("can parse correctly (two uChars, uChar)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17C1 U+17B6 > U+17C4');
      const productionBlock: Rule = new ContextProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION_CONTEXT);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS_CONTEXT);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (two uChars, two uChars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17C6 U+17BB > U+17BB U+17C6');
      const productionBlock: Rule = new ContextProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION_CONTEXT);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS_CONTEXT);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      const uCharNodes = rhsNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 2);
    });
  });
  describe("UsingKeysLhsBlockRule Tests", () => {
    it("can construct a UsingKeysLhsBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const lhsBlock: Rule = new UsingKeysLhsBlockRule();
      assert.isNotNull(lhsBlock);
    });
    it("can parse correctly (plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ any(c_key)');
      const lhsBlock: Rule = new UsingKeysLhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS_USING_KEYS);
      assert.isNotNull(lhsNode);
      const keystrokeNode = lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE)
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
      assert.isFalse(lhsNode.hasChildOfType(NodeTypes.LINE));
    });
  });
  describe("ReadOnlyLhsBlockRule Tests", () => {
    it("can construct a ReadOnlyLhsBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const lhsBlock: Rule = new ReadOnlyLhsBlockRule();
      assert.isNotNull(lhsBlock);
    });
    it("can parse correctly (match)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('match');
      const lhsBlock: Rule = new ReadOnlyLhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS_READONLY);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.MATCH));
      assert.isFalse(lhsNode.hasChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (nomatch)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nomatch');
      const lhsBlock: Rule = new ReadOnlyLhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS_READONLY);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.NOMATCH));
      assert.isFalse(lhsNode.hasChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (if-like)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch")');
      const lhsBlock: Rule = new ReadOnlyLhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS_READONLY);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.PLATFORM));
      assert.isFalse(lhsNode.hasChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (if-like, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") any(digit)');
      const lhsBlock: Rule = new ReadOnlyLhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS_READONLY);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.PLATFORM));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      assert.isFalse(lhsNode.hasChildOfType(NodeTypes.LINE));
    });
  });
  describe("UsingKeysInputBlockRule Tests", () => {
    it("can construct a UsingKeysInputBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const usingKeysInputBlock: Rule = new UsingKeysInputBlockRule();
      assert.isNotNull(usingKeysInputBlock);
    });
    it("can parse correctly (plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ any(c_key)');
      const usingKeysInputBlock: Rule = new UsingKeysInputBlockRule();
      assert.isTrue(usingKeysInputBlock.parse(root));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
      assert.isFalse(keystrokeNode.hasChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (if-like block, plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") + any(c_key)');
      const usingKeysInputBlock: Rule = new UsingKeysInputBlockRule();
      assert.isTrue(usingKeysInputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYSTROKE));
    });
    it("can parse correctly (inputContext, plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(output) + any(diacriticKey)');
      const usingKeysInputBlock: Rule = new UsingKeysInputBlockRule();
      assert.isTrue(usingKeysInputBlock.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.ANY));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
      assert.isFalse(root.hasChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (if-like, inputContext, plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") any(output) + any(diacriticKey)');
      const usingKeysInputBlock: Rule = new UsingKeysInputBlockRule();
      assert.isTrue(usingKeysInputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.ANY));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
      assert.isFalse(root.hasChildOfType(NodeTypes.LINE));
    });
  })
  describe("ReadOnlyInputBlockRule Tests", () => {
    it("can construct a ReadOnlyInputBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const readOnlyInputBlock: Rule = new ReadOnlyInputBlockRule();
      assert.isNotNull(readOnlyInputBlock);
    });
    it("can parse correctly (keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(c_key)');
      const readOnlyInputBlock: Rule = new ReadOnlyInputBlockRule();
      assert.isTrue(readOnlyInputBlock.parse(root));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
      assert.isFalse(keystrokeNode.hasChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (if-like block, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") any(c_key)');
      const readOnlyInputBlock: Rule = new ReadOnlyInputBlockRule();
      assert.isTrue(readOnlyInputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYSTROKE));
      assert.isFalse(root.hasChildOfType(NodeTypes.LINE));
    });
  });
  describe("ContextInputBlockRule Tests", () => {
    it("can construct a ContextInputBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const contextInputBlock: Rule = new ContextInputBlockRule();
      assert.isNotNull(contextInputBlock);
    });
    it("can parse correctly (two uChars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17C1 U+17B6)');
      const contextInputBlock: Rule = new ContextInputBlockRule();
      assert.isTrue(contextInputBlock.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const uCharNodes = inputContextNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 2);
      assert.equal(uCharNodes[0].getText(), 'U+17C1');
      assert.equal(uCharNodes[1].getText(), 'U+17B6');
      assert.isFalse(inputContextNode.hasChildOfType(NodeTypes.LINE));
    });
    it("can parse correctly (if-like block, two uChars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") U+17C1 U+17B6)');
      const contextInputBlock: Rule = new ContextInputBlockRule();
      assert.isTrue(contextInputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const uCharNodes = inputContextNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 2);
      assert.equal(uCharNodes[0].getText(), 'U+17C1');
      assert.equal(uCharNodes[1].getText(), 'U+17B6');
      assert.isFalse(inputContextNode.hasChildOfType(NodeTypes.LINE));
    });
  });
  describe("InputContextRule Tests", () => {
    it("can construct an InputContextRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const inputContext: Rule = new InputContextRule();
      assert.isNotNull(inputContext);
    });
    it("can parse correctly (any)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(c_shifter)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (two anys)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(digit) any(number)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const anyNodes = inputContextNode.getChildrenOfType(NodeTypes.ANY);
      assert.equal(anyNodes.length, 2);
    });
    it("can parse correctly (text, any)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17D2 any(c_out)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.U_CHAR));
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (any, context)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(digit) context(1)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.ANY));
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.CONTEXT));
    });
    it("can parse correctly (two anys, continuation)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(digit)\\\nany(number)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const anyNodes = inputContextNode.getChildrenOfType(NodeTypes.ANY);
      assert.equal(anyNodes.length, 2);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.LINE));
    });
  });
  describe("InputElementRule Tests", () => {
    it("can construct an InputElementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const inputElement: Rule = new InputElementRule();
      assert.isNotNull(inputElement);
    });
    it("can parse correctly (any)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(digit)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(root));
      const anyNode = root.getSoleChildOfType(NodeTypes.ANY);
      assert.isNotNull(anyNode);
      assert.isNotNull(anyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
    it("can parse correctly (notAny)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('notany(digit)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(root));
      const notAnyNode = root.getSoleChildOfType(NodeTypes.NOTANY);
      assert.isNotNull(notAnyNode);
      assert.isNotNull(notAnyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
    it("can parse correctly (deadKey)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('dk(storeName)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(root));
      const deadKeyNode = root.getSoleChildOfType(NodeTypes.DEADKEY);
      assert.isNotNull(deadKeyNode);
      assert.isNotNull(deadKeyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
    it("can parse correctly (contextStatement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('context(1)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(root));
      const contextNode = root.getSoleChildOfType(NodeTypes.CONTEXT);
      assert.isNotNull(contextNode);
      assert.equal(contextNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+1780');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.U_CHAR));
    });
  });
  describe("KeystrokeRule Tests", () => {
    it("can construct an KeystrokeRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const keystroke: Rule = new KeystrokeRule();
      assert.isNotNull(keystroke);
    });
    it("can parse correctly (any)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(digit)');
      const keystroke: Rule = new KeystrokeRule();
      assert.isTrue(keystroke.parse(root));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+1780');
      const keystroke: Rule = new KeystrokeRule();
      assert.isTrue(keystroke.parse(root));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.U_CHAR));
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
  describe("IfLikeBlockRule Tests", () => {
    it("can construct a IfLikeBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifLikeBlock: Rule = new IfLikeBlockRule();
      assert.isNotNull(ifLikeBlock);
    });
    it("can parse correctly (single if-like)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch")');
      const ifLikeBlock: Rule = new IfLikeBlockRule();
      assert.isTrue(ifLikeBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
    });
    it("can parse correctly (two if-like statements)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") baselayout("en-US")');
      const ifLikeBlock: Rule = new IfLikeBlockRule();
      assert.isTrue(ifLikeBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BASELAYOUT));
    });
    it("can parse correctly (two if-likes, continuation)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch")\\\nbaselayout("en-US")');
      const ifLikeBlock: Rule = new IfLikeBlockRule();
      assert.isTrue(ifLikeBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BASELAYOUT));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LINE));
    });
  });
  describe("IfLikeStatementRule Tests", () => {
    it("can construct a IfLikeStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isNotNull(ifLikeStatement);
    });
    it("can parse correctly (if)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = digit)');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isTrue(ifLikeStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      const storeNameNodes = ifNode.getChildrenOfType(NodeTypes.STORENAME);
      assert.equal(storeNameNodes.length, 2);
      assert.equal(storeNameNodes[0].getText(), 'number');
      assert.equal(storeNameNodes[1].getText(), 'digit');
    });
    it("can parse correctly (platform)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch")');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isTrue(ifLikeStatement.parse(root));
      const platformNode = root.getSoleChildOfType(NodeTypes.PLATFORM);
      assert.isNotNull(platformNode);
      const stringNode = platformNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"touch"');
    });
    it("can parse correctly (baselayout)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('baselayout("en-US")');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isTrue(ifLikeStatement.parse(root));
      const baselayoutNode = root.getSoleChildOfType(NodeTypes.BASELAYOUT);
      assert.isNotNull(baselayoutNode);
      const stringNode = baselayoutNode.getSoleChildOfType(NodeTypes.STRING)
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
    it("can parse correctly (store, store)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = digit)');
      const ifStatement: Rule = new IfStatementRule();
      assert.isTrue(ifStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      const storeNameNodes = ifNode.getChildrenOfType(NodeTypes.STORENAME);
      assert.equal(storeNameNodes.length, 2);
      assert.equal(storeNameNodes[0].getText(), 'number');
      assert.equal(storeNameNodes[1].getText(), 'digit');
    });
    it("can parse correctly (store, string)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = "1")');
      const ifStatement: Rule = new IfStatementRule();
      assert.isTrue(ifStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
    it("can parse correctly (system store, store)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap = filename)');
      const ifStatement: Rule = new IfStatementRule();
      assert.isTrue(ifStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
    });
    it("can parse correctly (system store, string)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap = "filename")');
      const ifStatement: Rule = new IfStatementRule();
      assert.isTrue(ifStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
  });
  describe("IfStoreStoreStatementRule Tests", () => {
    it("can construct a IfStoreStoreStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifStoreStoreStatement: Rule = new IfStoreStoreStatementRule();
      assert.isNotNull(ifStoreStoreStatement);
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = digit)');
      const ifStoreStoreStatement: Rule = new IfStoreStoreStatementRule();
      assert.isTrue(ifStoreStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      const storeNameNodes = ifNode.getChildrenOfType(NodeTypes.STORENAME);
      assert.equal(storeNameNodes.length, 2);
      assert.equal(storeNameNodes[0].getText(), 'number');
      assert.equal(storeNameNodes[1].getText(), 'digit');
    });
    it("can parse correctly (not equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number != digit)');
      const ifStoreStoreStatement: Rule = new IfStoreStoreStatementRule();
      assert.isTrue(ifStoreStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.NOT_EQUAL))
      const storeNameNodes = ifNode.getChildrenOfType(NodeTypes.STORENAME);
      assert.equal(storeNameNodes.length, 2);
      assert.equal(storeNameNodes[0].getText(), 'number');
      assert.equal(storeNameNodes[1].getText(), 'digit');
    });
    it("can parse correctly (equal, space before)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if( number = digit)');
      const ifStoreStoreStatement: Rule = new IfStoreStoreStatementRule();
      assert.isTrue(ifStoreStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      const storeNameNodes = ifNode.getChildrenOfType(NodeTypes.STORENAME);
      assert.equal(storeNameNodes.length, 2);
      assert.equal(storeNameNodes[0].getText(), 'number');
      assert.equal(storeNameNodes[1].getText(), 'digit');
    });
    it("can parse correctly (equal, space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = digit )');
      const ifStoreStoreStatement: Rule = new IfStoreStoreStatementRule();
      assert.isTrue(ifStoreStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      const storeNameNodes = ifNode.getChildrenOfType(NodeTypes.STORENAME);
      assert.equal(storeNameNodes.length, 2);
      assert.equal(storeNameNodes[0].getText(), 'number');
      assert.equal(storeNameNodes[1].getText(), 'digit');
    });
    it("can parse correctly (equal, space before and after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if( number = digit )');
      const ifStoreStoreStatement: Rule = new IfStoreStoreStatementRule();
      assert.isTrue(ifStoreStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      const storeNameNodes = ifNode.getChildrenOfType(NodeTypes.STORENAME);
      assert.equal(storeNameNodes.length, 2);
      assert.equal(storeNameNodes[0].getText(), 'number');
      assert.equal(storeNameNodes[1].getText(), 'digit');
    });
  });
  describe("IfStoreStringStatementRule Tests", () => {
    it("can construct a IfStoreStringStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifStoreStringStatement: Rule = new IfStoreStringStatementRule();
      assert.isNotNull(ifStoreStringStatement);
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = "1")');
      const ifStoreStringStatement: Rule = new IfStoreStringStatementRule();
      assert.isTrue(ifStoreStringStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
    it("can parse correctly (not equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number != "1")');
      const ifStoreStringStatement: Rule = new IfStoreStringStatementRule();
      assert.isTrue(ifStoreStringStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.NOT_EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
  });
  describe("IfSystemStoreStoreStatementRule Tests", () => {
    it("can construct a IfSystemStoreStoreStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifSystemStoreStoreStatement: Rule = new IfSystemStoreStoreStatementRule();
      assert.isNotNull(ifSystemStoreStoreStatement);
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap = filename)');
      const ifSystemStoreStoreStatement: Rule = new IfSystemStoreStoreStatementRule();
      assert.isTrue(ifSystemStoreStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap != filename)');
      const ifSystemStoreStoreStatement: Rule = new IfSystemStoreStoreStatementRule();
      assert.isTrue(ifSystemStoreStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.NOT_EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
    });
  });
  describe("IfSystemStoreStringStatementRule Tests", () => {
    it("can construct a IfSystemStoreStringStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifSystemStoreStringStatement: Rule = new IfSystemStoreStringStatementRule();
      assert.isNotNull(ifSystemStoreStringStatement);
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap = "filename")');
      const ifSystemStoreStringStatement: Rule = new IfSystemStoreStringStatementRule();
      assert.isTrue(ifSystemStoreStringStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap != "filename")');
      const ifSystemStoreStringStatement: Rule = new IfSystemStoreStringStatementRule();
      assert.isTrue(ifSystemStoreStringStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.NOT_EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
  });
  describe("IfSystemStoreNameRule Tests", () => {
    it("can construct a IfSystemStoreNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ifSystemStoreName: Rule = new IfSystemStoreNameRule();
      assert.isNotNull(ifSystemStoreName);
    });
    it("can parse correctly (platform)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform');
      const ifSystemStoreName: Rule = new IfSystemStoreNameRule();
      assert.isTrue(ifSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
    });
    it("can parse correctly (layer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer');
      const ifSystemStoreName: Rule = new IfSystemStoreNameRule();
      assert.isTrue(ifSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYER));
    });
    it("can parse correctly (baselayout)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('baselayout');
      const ifSystemStoreName: Rule = new IfSystemStoreNameRule();
      assert.isTrue(ifSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BASELAYOUT));
    });
    it("can parse correctly (newLayer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('newLayer');
      const ifSystemStoreName: Rule = new IfSystemStoreNameRule();
      assert.isTrue(ifSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NEWLAYER));
    });
    it("can parse correctly (oldLayer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('oldLayer');
      const ifSystemStoreName: Rule = new IfSystemStoreNameRule();
      assert.isTrue(ifSystemStoreName.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.OLDLAYER));
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
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.EQUAL));
    });
    it("can parse correctly (!=)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('!=');
      const comparison: Rule = new ComparisonRule();
      assert.isTrue(comparison.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NOT_EQUAL));
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
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER);
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
      const platformNode = root.getSoleChildOfType(NodeTypes.PLATFORM);
      assert.isNotNull(platformNode);
      const stringNode = platformNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"touch"');
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
  describe("BracketedStringRule Tests", () => {
    it("can construct a BracketedStringRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const bracketedString: Rule = new BracketedStringRule();
      assert.isNotNull(bracketedString);
    });
    it("can parse correctly", () => {
      Rule.tokenBuffer = stringToTokenBuffer('("a")');
      const bracketedString: Rule = new BracketedStringRule();
      assert.isTrue(bracketedString.parse(root));
      const stringNode: ASTNode = root.getSoleChildOfType(NodeTypes.STRING);
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"a"');
    });
    it("can parse correctly (space before name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('( "a")');
      const bracketedString: Rule = new BracketedStringRule();
      assert.isTrue(bracketedString.parse(root));
      const stringNode: ASTNode = root.getSoleChildOfType(NodeTypes.STRING);
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"a"');
    });
    it("can parse correctly (space after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('("a" )');
      const bracketedString: Rule = new BracketedStringRule();
      assert.isTrue(bracketedString.parse(root));
      const stringNode: ASTNode = root.getSoleChildOfType(NodeTypes.STRING);
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"a"');
    });
    it("can parse correctly (space before and after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('( "a" )');
      const bracketedStoreName: Rule = new BracketedStringRule();
      assert.isTrue(bracketedStoreName.parse(root));
      const stringNode: ASTNode = root.getSoleChildOfType(NodeTypes.STRING);
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"a"');
    });
  });
  describe("RhsBlockRule Tests", () => {
    it("can construct a RhsBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isNotNull(rhsBlock);
    });
    it("can parse correctly (context output block)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('context layer("shift")');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(root));
      const rhsNode = root.getSoleChildOfType(NodeTypes.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.LAYER));
    });
    it("can parse correctly (context only)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('context');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(root));
      const rhsNode = root.getSoleChildOfType(NodeTypes.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.CONTEXT));
    });
    it("can parse correctly (nul only)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(root));
      const rhsNode = root.getSoleChildOfType(NodeTypes.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.NUL));
    });
  });
  describe("OutputBlockRule Tests", () => {
    it("can construct a OutputBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const outputBlock: Rule = new OutputBlockRule();
      assert.isNotNull(outputBlock);
    });
    it("can parse correctly (single output statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use(main)');
      const outputBlock: Rule = new OutputBlockRule();
      assert.isTrue(outputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.USE));
    });
    it("can parse correctly (two output statements)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use(main) layer("shift")');
      const outputBlock: Rule = new OutputBlockRule();
      assert.isTrue(outputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.USE));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYER));
    });
    it("can parse correctly (two output statements, continuation)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use(main)\\\nlayer("shift")');
      const outputBlock: Rule = new OutputBlockRule();
      assert.isTrue(outputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.USE));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYER));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LINE));
    });
  });
  describe("OutputStatementRule Tests", () => {
    it("can construct a OutputStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isNotNull(outputStatement);
    });
    it("can parse correctly (use statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use(main)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const useNode = root.getSoleChildOfType(NodeTypes.USE);
      assert.isNotNull(useNode);
      assert.isNotNull(useNode.getSoleChildOfType(NodeTypes.GROUPNAME));
    });
    it("can parse correctly (call statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('call(storeName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const callNode = root.getSoleChildOfType(NodeTypes.CALL);
      assert.isNotNull(callNode);
      assert.isNotNull(callNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
    it("can parse correctly (set statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('set(storeName = "value")');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const setNode = root.getSoleChildOfType(NodeTypes.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.STORENAME));
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (save statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('save(storeName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const saveNode = root.getSoleChildOfType(NodeTypes.SAVE);
      assert.isNotNull(saveNode);
      assert.isNotNull(saveNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
    it("can parse correctly (deadkey statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('dk(storeName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const deadKeyNode = root.getSoleChildOfType(NodeTypes.DEADKEY);
      assert.isNotNull(deadKeyNode);
      assert.isNotNull(deadKeyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
    it("can parse correctly (set layer statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('set(&layer = "value")');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const setNode = root.getSoleChildOfType(NodeTypes.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.LAYER));
      assert.isNotNull(setNode.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (layer statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer("shift")');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER);
      assert.isNotNull(layerNode);
      assert.equal(layerNode.getSoleChildOfType(NodeTypes.STRING).getText(), '"shift"');
    });
    it("can parse correctly (index statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(c_out,1)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeTypes.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'c_out');
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (context statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('context(1)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const contextNode = root.getSoleChildOfType(NodeTypes.CONTEXT);
      assert.isNotNull(contextNode);
      assert.equal(contextNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+1780');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (beep)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('beep');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BEEP));
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
      const indexNode = root.getSoleChildOfType(NodeTypes.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (space after open)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index( digit,1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeTypes.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (space before close)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit,1 )');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeTypes.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (space after open and before close)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index( digit,1 )');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeTypes.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (space before comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit ,1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeTypes.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (space after comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit, 1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeTypes.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("can parse correctly (space before and after comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('index(digit , 1)');
      const indexStatement: Rule = new IndexStatementRule();
      assert.isTrue(indexStatement.parse(root));
      const indexNode = root.getSoleChildOfType(NodeTypes.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
      assert.equal(indexNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
  });
  describe("SpacedCommaRule Tests", () => {
    it("can construct a SpacedCommaRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const spacedComma: Rule = new SpacedCommaRule();
      assert.isNotNull(spacedComma);
    });
    it("can parse correctly (comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(',');
      const spacedComma: Rule = new SpacedCommaRule();
      assert.isTrue(spacedComma.parse(root));
    });
    it("can parse correctly (comma, space before)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' ,');
      const spacedComma: Rule = new SpacedCommaRule();
      assert.isTrue(spacedComma.parse(root));
    });
    it("can parse correctly (comma, space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(', ');
      const spacedComma: Rule = new SpacedCommaRule();
      assert.isTrue(spacedComma.parse(root));
    });
    it("can parse correctly (comma, space before and after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' , ');
      const spacedComma: Rule = new SpacedCommaRule();
      assert.isTrue(spacedComma.parse(root));
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
      const contextNode = root.getSoleChildOfType(NodeTypes.CONTEXT);
      assert.isNotNull(contextNode);
      assert.equal(contextNode.getSoleChildOfType(NodeTypes.OFFSET).getText(), '1');
    });
    it("rejects a context without brackets", () => {
      Rule.tokenBuffer = stringToTokenBuffer('context');
      const contextStatement: Rule = new ContextStatementRule();
      assert.isFalse(contextStatement.parse(root));
    });
  });
  describe("Analyser Tests", () => {
    it("can parse (part of) Khmer Angkor correctly", () => {
      const buffer: String = new String(readFileSync('test/fixtures/keyboards/khmer_angkor.kmn'));
      const lexer = new Lexer(buffer);
      const tokens: Token[] = lexer.parse();
      //const subset: Token[] = tokens.filter((token) => token.lineNum <= 660);
      //Rule.tokenBuffer = new TokenBuffer(subset);
      Rule.tokenBuffer = new TokenBuffer(tokens);
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
      const storeNames = [
        'ShiftOutSingle', 'vCombo1', 'vCombo2', 'vCombo3', 'ShiftOutAll',
        'digit', 'number', 'whitespace', 'c_key', 'c_out',
        'v_gen_key', 'v_gen', 'v_pseudo_key', 'v_pseudo', 'v_key',
        'v_out', 'v_any', 'v_combo_R', 'v_combo_N', 'v_combo',
        'ind_v_key', 'ind_v_out', 'diacritic_key', 'diacritic_out', 'c_shifter_key',
        'c_shifter', 'punct_key', 'punct_out', 'latin_punct_key', 'latin_punct_out',
        'spaces_key', 'spaces_out', 'currency_key', 'currency_out', 'digit_key',
        'digit_out', 'lek_attak_key', 'lek_attak_out', 'lunar_date_key', 'lunar_date_out',
        'input_subcons', 'subcons', 'arabic_digit_key', 'arabic_digit_out', 'v_above',
        'shiftable_c_1st', 'shiftable_BA', 'shiftable_c_2nd', 'shiftable_c_2nd_with_BA', 'c_2nd_combo_LO',
        'c_2nd_combo_MO', 'c_1st_combo_LO', 'c_1st_combo_MO', 'c_combo_SA', 'c_combo_QA',
        'c_combo_HA',
      ];
      assert.equal(storeNodes.length, storeNames.length);
      storeNames.forEach((name, idx) => {
        assert.equal(storeNodes[idx].getDescendents(NodeTypes.STORENAME)[0].getText(), name);
      });
      const groupNodes = root.getChildrenOfType(NodeTypes.GROUP);
      assert.equal(groupNodes.length, 5);
      assert.equal(groupNodes[0].getDescendents(NodeTypes.GROUPNAME)[0].getText(), 'NewContext');
      assert.equal(groupNodes[0].getDescendents(NodeTypes.READONLY).length, 1);
      assert.equal(groupNodes[1].getDescendents(NodeTypes.GROUPNAME)[0].getText(), 'PostKeystroke');
      assert.equal(groupNodes[1].getDescendents(NodeTypes.READONLY).length, 1);
      assert.equal(groupNodes[2].getDescendents(NodeTypes.GROUPNAME)[0].getText(), 'detectStartOfSentence');
      assert.equal(groupNodes[2].getDescendents(NodeTypes.READONLY).length, 1);
      assert.equal(groupNodes[3].getDescendents(NodeTypes.GROUPNAME)[0].getText(), 'main');
      assert.equal(groupNodes[3].getDescendents(NodeTypes.USING_KEYS).length, 1);
      assert.equal(groupNodes[4].getDescendents(NodeTypes.GROUPNAME)[0].getText(), 'normalise');
      assert.isFalse(groupNodes[4].hasChildOfType(NodeTypes.USING_KEYS));
      assert.isFalse(groupNodes[4].hasChildOfType(NodeTypes.READONLY));
      const readOnlyProductionNodes  = root.getChildrenOfType(NodeTypes.PRODUCTION_READONLY);
      const usingKeysProductionNodes = root.getChildrenOfType(NodeTypes.PRODUCTION_USING_KEYS);
      const contextProductionNodes   = root.getChildrenOfType(NodeTypes.PRODUCTION_CONTEXT);
      assert.equal(readOnlyProductionNodes.length, 7);
      assert.equal(usingKeysProductionNodes.length, 53);
      assert.equal(contextProductionNodes.length, 208);
      //assert.equal(root.toString(), '');
    });
  });
});

export function stringToTokenBuffer(buffer: String): TokenBuffer {
  const lexer = new Lexer(buffer);
  const tokens: Token[] = lexer.parse();
  return new TokenBuffer(tokens);
}
