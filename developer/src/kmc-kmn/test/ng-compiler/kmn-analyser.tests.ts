/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import 'mocha';
import { assert } from 'chai';
import { Rule, TokenRule } from '../../src/ng-compiler/recursive-descent.js';
import { Lexer, Token, TokenTypes } from '../../src/ng-compiler/lexer.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { AnyStatementRule, BaselayoutStatementRule, BeginStatementRule, CallStatementRule, DeadKeyStatementRule, InputElementRule, NotAnyStatementRule, SaveStatementRule, ModifierRule, PlainTextRule, RuleBlockRule, PermittedKeywordRule, GroupNameRule, LhsBlockRule, CompileTargetRule } from '../../src/ng-compiler/kmn-analyser.js';
import { ComparisonRule, ContentRule } from '../../src/ng-compiler/kmn-analyser.js';
import { InputBlockRule, ProductionBlockRule, ContextStatementRule } from '../../src/ng-compiler/kmn-analyser.js';
import { EntryPointRule, GroupStatementRule, GroupQualifierRule } from '../../src/ng-compiler/kmn-analyser.js';
import { IfLikeStatementRule, IfStatementRule, IfNormalStoreStatementRule } from '../../src/ng-compiler/kmn-analyser.js';
import { SystemStoreNameForIfRule, IfSystemStoreStatementRule, IndexStatementRule, InputContextRule } from '../../src/ng-compiler/kmn-analyser.js';
import { KeystrokeRule, KmnTreeRule, LayerStatementRule, LineRule } from '../../src/ng-compiler/kmn-analyser.js';
import { OutputStatementRule, OutsStatementRule, PlatformStatementRule } from '../../src/ng-compiler/kmn-analyser.js';
import { RhsBlockRule, SimpleTextRule } from '../../src/ng-compiler/kmn-analyser.js';
import { TextRangeRule, TextRule, UseStatementRule } from '../../src/ng-compiler/kmn-analyser.js';
import { UsingKeysRule, VirtualKeyRule } from '../../src/ng-compiler/kmn-analyser.js';
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
    it("can parse correctly (no space before, no comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (no space before, no comment, space before newline)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" \n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before, no comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename"\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (no space before, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" c a comment\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (space before, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename" c a comment\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeTypes.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (variable store assign, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 c a comment\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeTypes.STORE);
      assert.equal(children[0].getSoleChildOfType(NodeTypes.STORENAME).getText(), 'c_out');
      assert.equal(children[1].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (variable store assign, continuation, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\nU+1781 c a comment\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
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
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const storeNode = root.getSoleChildOfType(NodeTypes.STORE)
      assert.isNotNull(storeNode);
      const uCharNodes = storeNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 35);
    });
    it("can parse correctly (ruleBlock)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17D2 + [K_D] > context(1) U+178F\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (blank, no comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
    it("can parse correctly (blank, no comment, space before newline)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' \n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
    it("can parse correctly (blank, comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
    it("can parse correctly (blank, space before comment)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' c This tells Keyman which keys should have casing behavior applied\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      assert.equal(root.getSoleChild().nodeType, NodeTypes.LINE);
      assert.isFalse(root.getSoleChild().hasChild());
    });
    it("can parse correctly (compile target)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$keyman: store(&bitmap) "filename"\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 3);
      assert.equal(children[0].nodeType, NodeTypes.KEYMAN);
      assert.equal(children[1].nodeType, NodeTypes.BITMAP);
      assert.equal(children[1].getSoleChild().nodeType, NodeTypes.STRING);
      assert.equal(children[2].nodeType, NodeTypes.LINE);
    });
    it("can parse correctly (compile target, plus, virtual key, u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$keymanonly: + [CTRL "."] > U+135E\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(root));
      const children = root.getChildren();
      assert.equal(children.length, 3);
      assert.equal(children[0].nodeType, NodeTypes.KEYMANONLY);
      assert.equal(children[1].nodeType, NodeTypes.PRODUCTION);
      assert.equal(children[2].nodeType, NodeTypes.LINE);
    });
  });
  describe("CompileTargetRule Tests", () => {
    it("can construct a CompileTargetRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isNotNull(compileTarget);
    });
    it("can parse correctly (KEYMAN)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$keyman:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYMAN));
    });
    it("can parse correctly (KEYMANONLY)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$keymanonly:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYMANONLY));
    });
    it("can parse correctly (KEYMANWEB)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$keymanweb:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYMANWEB));
    });
    it("can parse correctly (KMFL)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$kmfl:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KMFL));
    });
    it("can parse correctly (WEAVER)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$weaver:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.WEAVER));
    });
  });
  describe("ContentRule Tests", () => {
    it("can construct a ContentRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const content: Rule = new ContentRule();
      assert.isNotNull(content);
    });
    it("can parse correctly (system store assign)", () => {
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
    it("can parse correctly (caps always off)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('caps always off');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.CAPSALWAYSOFF));
    });
    it("can parse correctly (caps on only)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('caps on only');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.CAPSONONLY));
    });
    it("can parse correctly (shift frees caps)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('shift frees caps');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.SHIFTFREESCAPS));
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
  describe("PlainTextRule Tests", () => {
    it("can construct a PlainTextRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const plainText: Rule = new PlainTextRule();
      assert.isNotNull(plainText);
    });
    it("can parse correctly (text range)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('"a".."c"');
      const plainText: Rule = new PlainTextRule();
      assert.isTrue(plainText.parse(root));
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
      const plainText: Rule = new PlainTextRule();
      assert.isTrue(plainText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (outs statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs(digit)');
      const plainText: Rule = new PlainTextRule();
      assert.isFalse(plainText.parse(root));
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
    it("can parse correctly (virtualKey)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[K_K]');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY));
    });
    it("can parse correctly (u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+1780');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (named constant)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$CCedilla');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NAMED_CONSTANT));
    });
    it("can parse correctly (hangul)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('$HANGUL_SYLLABLE_A');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.HANGUL));
    });
    it("can parse correctly (decimal, space)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('d99 ');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.DECIMAL));
    });
    it("can parse correctly (hexadecimal, space)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('xa99 ');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.HEXADECIMAL));
    });
    it("can parse correctly (octal, space)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('77 ');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.OCTAL));
    });
    it("can parse correctly (nul)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NUL));
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
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.MODIFIER));
    });
    it("can parse correctly (two shiftcode virtual key)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[SHIFT CTRL K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.KEY_CODE));
      const modifiers = virtualKeyNode.getChildrenOfType(NodeTypes.MODIFIER);
      assert.equal(modifiers.length, 2);
      assert.equal(modifiers[0].token.text, 'SHIFT');
      assert.equal(modifiers[1].token.text, 'CTRL');
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
    it("can parse correctly (virtual character)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('["A"]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.STRING));
    });
    it("can parse correctly (virtual character with modifier)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('[CTRL "A"]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(root));
      const virtualKeyNode = root.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.STRING));
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeTypes.MODIFIER));
    });
  });
  describe("ModifierRule Tests", () => {
    it("can construct a ModifierRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const modifier: Rule = new ModifierRule();
      assert.isNotNull(modifier);
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
        const modifier: Rule = new ModifierRule();
        root = new ASTNode(NodeTypes.TMP);
        assert.isTrue(modifier.parse(root));
        assert.equal(root.getSoleChildOfType(NodeTypes.MODIFIER).getText(), code);
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
    it("can parse correctly (space before name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs( digit)');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isTrue(outsStatement.parse(root));
      const outsNode = root.getSoleChildOfType(NodeTypes.OUTS);
      assert.isNotNull(outsNode);
      assert.equal(outsNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
    });
    it("can parse correctly (space after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs(digit )');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isTrue(outsStatement.parse(root));
      const outsNode = root.getSoleChildOfType(NodeTypes.OUTS);
      assert.isNotNull(outsNode);
      assert.equal(outsNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
    });
    it("can parse correctly (space before and after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('outs( digit )');
      const outsStatement: Rule = new OutsStatementRule();
      assert.isTrue(outsStatement.parse(root));
      const outsNode = root.getSoleChildOfType(NodeTypes.OUTS);
      assert.isNotNull(outsNode);
      assert.equal(outsNode.getSoleChildOfType(NodeTypes.STORENAME).getText(), 'digit');
    });
  });
  describe("RuleBlockRule Tests", () => {
    it("can construct a RuleBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const ruleBlock: Rule = new RuleBlockRule();
      assert.isNotNull(ruleBlock);
    });
    it("can parse correctly (production block)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17D2 + [K_D] > context(1) U+178F');
      const productionBlock: Rule = new RuleBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
  });
  describe("BeginStatementRule Tests", () => {
    it("can construct an BeginStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const beginBlock: Rule = new BeginStatementRule();
      assert.isNotNull(beginBlock);
    });
    it("can parse correctly (no entrypoint)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('begin > use(main)');
      const beginBlock: Rule = new BeginStatementRule();
      assert.isTrue(beginBlock.parse(root));
      const beginNode = root.getSoleChildOfType(NodeTypes.BEGIN);
      assert.isNotNull(beginNode);
      assert.isNotNull(beginNode.getSoleChildOfType(NodeTypes.USE));
    });
    it("can parse correctly (entrypoint)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('begin unicode > use(main)');
      const beginBlock: Rule = new BeginStatementRule();
      assert.isTrue(beginBlock.parse(root));
      const beginNode = root.getSoleChildOfType(NodeTypes.BEGIN);
      assert.isNotNull(beginNode);
      assert.isNotNull(beginNode.getSoleChildOfType(NodeTypes.UNICODE));
      assert.isNotNull(beginNode.getSoleChildOfType(NodeTypes.USE));
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
    it("can parse correctly (space before parameter)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use( main)');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(root));
      const useNode = root.getSoleChildOfType(NodeTypes.USE);
      assert.isNotNull(useNode);
      const groupNameNode: ASTNode = useNode.getSoleChildOfType(NodeTypes.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
    });
    it("can parse correctly (space after parameter)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use(main )');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(root));
      const useNode = root.getSoleChildOfType(NodeTypes.USE);
      assert.isNotNull(useNode);
      const groupNameNode: ASTNode = useNode.getSoleChildOfType(NodeTypes.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
    });
    it("can parse correctly (space before and after parameter)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('use( main )');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(root));
      const useNode = root.getSoleChildOfType(NodeTypes.USE);
      assert.isNotNull(useNode);
      const groupNameNode: ASTNode = useNode.getSoleChildOfType(NodeTypes.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
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
  describe("GroupNameRule Tests", () => {
    it("can construct a GroupNameRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const groupName: Rule = new GroupNameRule();
      assert.isNotNull(groupName);
    });
    it("can parse correctly (parameter)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('main');
      const groupName: Rule = new GroupNameRule();
      assert.isTrue(groupName.parse(root));
      assert.equal(root.getSoleChildOfType(NodeTypes.GROUPNAME).getText(), 'main');
    });
    it("can parse correctly (octal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('1');
      const groupName: Rule = new GroupNameRule();
      assert.isTrue(groupName.parse(root));
      assert.equal(root.getSoleChildOfType(NodeTypes.GROUPNAME).getText(), '1');
    });
    it("can parse correctly (permitted keyword)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('newcontext');
      const groupName: Rule = new GroupNameRule();
      assert.isTrue(groupName.parse(root));
      assert.equal(root.getSoleChildOfType(NodeTypes.GROUPNAME).getText(), 'newcontext');
    });
  });
  describe("PermittedKeywordRule Tests", () => {
    it("can construct a PermittedKeywordRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const permittedKeyword: Rule = new PermittedKeywordRule();
      assert.isNotNull(permittedKeyword);
    });
    it("can parse correctly", () => {
      [
        {input: 'always',        nodeType: NodeTypes.ALWAYS},
        {input: 'ansi',          nodeType: NodeTypes.ANSI},
        {input: 'beep',          nodeType: NodeTypes.BEEP},
        {input: 'begin',         nodeType: NodeTypes.BEGIN},
        {input: 'caps',          nodeType: NodeTypes.CAPS},
        {input: 'clearcontext',  nodeType: NodeTypes.CLEARCONTEXT},
        {input: 'context',       nodeType: NodeTypes.CONTEXT},
        {input: 'fix',           nodeType: NodeTypes.FIX},
        {input: 'frees',         nodeType: NodeTypes.FREES},
        {input: 'keys',          nodeType: NodeTypes.KEYS},
        {input: 'match',         nodeType: NodeTypes.MATCH},
        {input: 'newcontext',    nodeType: NodeTypes.NEWCONTEXT},
        {input: 'nomatch',       nodeType: NodeTypes.NOMATCH},
        {input: 'nul',           nodeType: NodeTypes.NUL},
        {input: 'off',           nodeType: NodeTypes.OFF},
        {input: 'on',            nodeType: NodeTypes.ON},
        {input: 'only',          nodeType: NodeTypes.ONLY},
        {input: 'postkeystroke', nodeType: NodeTypes.POSTKEYSTROKE},
        {input: 'readonly',      nodeType: NodeTypes.READONLY},
        {input: 'return',        nodeType: NodeTypes.RETURN},
        {input: 'shift',         nodeType: NodeTypes.SHIFT},
        {input: 'unicode',       nodeType: NodeTypes.UNICODE},
        {input: 'using',         nodeType: NodeTypes.USING},
      ].forEach((testCase) => {
        Rule.tokenBuffer = stringToTokenBuffer(testCase.input);
        const permittedKeyword: Rule = new PermittedKeywordRule();
        root = new ASTNode(NodeTypes.TMP);
        assert.isTrue(permittedKeyword.parse(root));
        assert.isNotNull(root.getSoleChildOfType(testCase.nodeType), `${testCase.input}`);
      });
    });
  });
  describe("GroupStatementRule Tests", () => {
    it("can construct an GroupStatementRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isNotNull(groupStatement);
    });
    it("can parse correctly (parameter, no qualifier)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(main)');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.GROUPNAME));
    });
    it("can parse correctly (octal, no qualifier)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(1)');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.GROUPNAME));
    });
    it("can parse correctly (permitted keyword, no qualifier)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(newcontext)');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.GROUPNAME));
    });
    it("can parse correctly (using keys)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(main) using keys');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.USING_KEYS));
      assert.isNull(groupNode.getSoleChildOfType(NodeTypes.READONLY));
    });
    it("can parse correctly (readonly)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('group(main) readonly');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(root));
      const groupNode = root.getSoleChildOfType(NodeTypes.GROUP);
      assert.isNotNull(groupNode);
      assert.isNull(groupNode.getSoleChildOfType(NodeTypes.USING_KEYS));
      assert.isNotNull(groupNode.getSoleChildOfType(NodeTypes.READONLY));
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
  describe("ProductionBlockRule Tests", () => {
    it("can construct a ProductionBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isNotNull(productionBlock);
    });
    it("can parse correctly (plus, any, index)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ any(c_key) > index(c_out,1)');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.INDEX));
    });
    it("can parse correctly (plus, virtual key, two uChars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ [SHIFT K_A] > U+17B6 U+17C6');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      const uCharNodes = rhsNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 2);
    });
    it("can parse correctly (u_char, plus, virtual key, context, u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17D2 + [K_D] > context(1) U+178F');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (if, plus, string, string, set, reset, set, reset)", () => {
      Rule.tokenBuffer = stringToTokenBuffer("if(foo = '1') + 'a' > 'foo.' set(foo='2') reset(foo) set(foo='3') reset(foo)");
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.IF));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      const outputNodes = rhsNode.getChildren();
      assert.equal(outputNodes.length, 5);
      assert.equal(outputNodes[0].nodeType, NodeTypes.STRING);
      assert.equal(outputNodes[1].nodeType, NodeTypes.SET);
      assert.equal(outputNodes[2].nodeType, NodeTypes.RESET);
      assert.equal(outputNodes[3].nodeType, NodeTypes.SET);
      assert.equal(outputNodes[4].nodeType, NodeTypes.RESET);
    });
    it("can parse correctly (dk, string, dk, string, dk, plus, string, string, context, string)", () => {
      Rule.tokenBuffer = stringToTokenBuffer("dk(1) 'a' dk(2) 'b' dk(3) + 'z' > '<' context '>'");
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      const inputContextNode = lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const inputNodes = inputContextNode.getChildren();
      assert.equal(inputNodes.length, 5);
      assert.equal(inputNodes[0].nodeType, NodeTypes.DEADKEY);
      assert.equal(inputNodes[1].nodeType, NodeTypes.STRING);
      assert.equal(inputNodes[2].nodeType, NodeTypes.DEADKEY);
      assert.equal(inputNodes[3].nodeType, NodeTypes.STRING);
      assert.equal(inputNodes[4].nodeType, NodeTypes.DEADKEY);
      const keystrokeNode = lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      const outputNodes = rhsNode.getChildren();
      assert.equal(outputNodes.length, 3);
      assert.equal(outputNodes[0].nodeType, NodeTypes.STRING);
      assert.equal(outputNodes[1].nodeType, NodeTypes.CONTEXT);
      assert.equal(outputNodes[2].nodeType, NodeTypes.STRING);
    });
    it("can parse correctly (two uChars, uChar)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17C1 U+17B6 > U+17C4');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
    it("can parse correctly (two uChars, two uChars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17C6 U+17BB > U+17BB U+17C6');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      const uCharNodes = rhsNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 2);
    });
    it("can parse correctly (platform, use)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") > use(detectStartOfSentence)');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.USE));
    });
    it("can parse correctly (ifs, any, context, layer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&newLayer = "") if(&layer = "shift") any(ShiftOutSingle) > context layer("default")');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      const ifNodes = lhsNode.getChildrenOfType(NodeTypes.IF);
      assert.equal(ifNodes.length, 2);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT));
    });
    it("can parse correctly (plus, virtual key, u_char)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ [CTRL "."] > U+135E');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(root));
      const productionNode = root.getSoleChildOfType(NodeTypes.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeTypes.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.U_CHAR));
    });
  });
  describe("LhsBlockRule Tests", () => {
    it("can construct a LhsBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isNotNull(lhsBlock);
    });
    it("can parse correctly (match)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('match');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.MATCH));
    });
    it("can parse correctly (nomatch)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nomatch');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.NOMATCH));
    });
    it("can parse correctly (if-like)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch")');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
    });
    it("can parse correctly (if-like, input context)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") any(digit)');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
    });
    it("can parse correctly (plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ any(c_key)');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(root));
      const lhsNode = root.getSoleChildOfType(NodeTypes.LHS);
      assert.isNotNull(lhsNode);
      const keystrokeNode = lhsNode.getSoleChildOfType(NodeTypes.KEYSTROKE)
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
    });
  });
  describe("InputBlockRule Tests", () => {
    it("can construct a InputBlockRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const inputBlock: Rule = new InputBlockRule();
      assert.isNotNull(inputBlock);
    });
    it("can parse correctly (two uChars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('U+17C1 U+17B6');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const uCharNodes = inputContextNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 2);
      assert.equal(uCharNodes[0].getText(), 'U+17C1');
      assert.equal(uCharNodes[1].getText(), 'U+17B6');
    });
    it("can parse correctly (if-like block, two uChars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") U+17C1 U+17B6');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const uCharNodes = inputContextNode.getChildrenOfType(NodeTypes.U_CHAR);
      assert.equal(uCharNodes.length, 2);
      assert.equal(uCharNodes[0].getText(), 'U+17C1');
      assert.equal(uCharNodes[1].getText(), 'U+17B6');
    });
    it("can parse correctly (nul, if-like block, deadkey)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul platform("touch") dk(1)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getChildrenOfType(NodeTypes.DEADKEY));
    });
    it("can parse correctly (keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (if-like block, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
    });
    it("can parse correctly (nul, if-like block, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul platform("touch") any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT));
    });
    it("can parse correctly (plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (if-like block, plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") + any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.KEYSTROKE));
    });
    it("can parse correctly (inputContext, plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('any(output) + any(diacriticKey)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.ANY));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (if-like, inputContext, plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch") any(output) + any(diacriticKey)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeTypes.ANY));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (nul, if-like, plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul platform("touch") + any(diacriticKey)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (nul, if-like, inputContext, plus, keystroke)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul platform("touch") dk(1) + any(diacriticKey)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
      const inputContextNode = root.getSoleChildOfType(NodeTypes.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getChildrenOfType(NodeTypes.DEADKEY));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (nul only)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NUL));
    });
    it("can parse correctly (nul, if-like block)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul platform("touch")');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT));
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
    it("can parse correctly (deadKey [as text])", () => {
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
      Rule.tokenBuffer = stringToTokenBuffer('+ any(digit)');
      const keystroke: Rule = new KeystrokeRule();
      assert.isTrue(keystroke.parse(root));
      const keystrokeNode = root.getSoleChildOfType(NodeTypes.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeTypes.ANY));
    });
    it("can parse correctly (text)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('+ U+1780');
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
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
    it("can parse correctly (platform)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('platform("touch")');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isTrue(ifLikeStatement.parse(root));
      const platformNode = root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT);
      assert.isNotNull(platformNode);
      const stringNode = platformNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"touch"');
    });
    it("can parse correctly (baselayout)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('baselayout("en-US")');
      const ifLikeStatement: Rule = new IfLikeStatementRule();
      assert.isTrue(ifLikeStatement.parse(root));
      const baselayoutNode = root.getSoleChildOfType(NodeTypes.BASELAYOUT_SHORTCUT);
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
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
    it("can parse correctly (not equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number != "1")');
      const ifNormalStoreStatement: Rule = new IfNormalStoreStatementRule();
      assert.isTrue(ifNormalStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.NOT_EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
    it("can parse correctly (equal, two u_chars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(number = U+1780 U+1781)');
      const ifNormalStoreStatement: Rule = new IfNormalStoreStatementRule();
      assert.isTrue(ifNormalStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STORENAME))
      assert.equal(ifNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
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
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
    it("can parse correctly (equal)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap != "filename")');
      const ifSystemStoreStatement: Rule = new IfSystemStoreStatementRule();
      assert.isTrue(ifSystemStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.NOT_EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.STRING))
    });
    it("can parse correctly (equal, two u_chars)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('if(&bitmap = U+1780 U+1781)');
      const ifSystemStoreStatement: Rule = new IfSystemStoreStatementRule();
      assert.isTrue(ifSystemStoreStatement.parse(root));
      const ifNode = root.getSoleChildOfType(NodeTypes.IF);
      assert.isNotNull(ifNode);
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.EQUAL))
      assert.isNotNull(ifNode.getSoleChildOfType(NodeTypes.BITMAP))
      assert.equal(ifNode.getChildrenOfType(NodeTypes.U_CHAR).length, 2);
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
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BITMAP));
    });
    it("can parse correctly (baselayout)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&baselayout');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.BASELAYOUT));
    });
    it("can parse correctly (layer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&layer');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.LAYER));
    });
    it("can parse correctly (newLayer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&newLayer');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NEWLAYER));
    });
    it("can parse correctly (oldLayer)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&oldLayer');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.OLDLAYER));
    });
    it("can parse correctly (platform)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('&platform');
      const systemStoreNameForIf: Rule = new SystemStoreNameForIfRule();
      assert.isTrue(systemStoreNameForIf.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.PLATFORM));
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
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (spacve before name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer( "shift")');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (space after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer("shift" )');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      const stringNode = layerNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"shift"');
    });
    it("can parse correctly (space before and after name)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('layer( "shift" )');
      const layerStatement: Rule = new LayerStatementRule();
      assert.isTrue(layerStatement.parse(root));
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
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
      const platformNode = root.getSoleChildOfType(NodeTypes.PLATFORM_SHORTCUT);
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
      const baselayoutNode = root.getSoleChildOfType(NodeTypes.BASELAYOUT_SHORTCUT);
      assert.isNotNull(baselayoutNode);
      const stringNode = baselayoutNode.getSoleChildOfType(NodeTypes.STRING)
      assert.isNotNull(stringNode);
      assert.equal(stringNode.getText(), '"en-US"');
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
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT));
    });
    it("can parse correctly (context only)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('context');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(root));
      const rhsNode = root.getSoleChildOfType(NodeTypes.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.CONTEXT));
    });
    it("can parse correctly (return only)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('return');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(root));
      const rhsNode = root.getSoleChildOfType(NodeTypes.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeTypes.RETURN));
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
    it("can parse correctly (reset statement)", () => {
      Rule.tokenBuffer = stringToTokenBuffer('reset(storeName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const resetNode = root.getSoleChildOfType(NodeTypes.RESET);
      assert.isNotNull(resetNode);
      assert.isNotNull(resetNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
    it("can parse correctly (deadkey statement [as text])", () => {
      Rule.tokenBuffer = stringToTokenBuffer('dk(storeName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      const deadKeyNode = root.getSoleChildOfType(NodeTypes.DEADKEY);
      assert.isNotNull(deadKeyNode);
      assert.isNotNull(deadKeyNode.getSoleChildOfType(NodeTypes.STORENAME));
    });
    it("can parse correctly (set system store statement)", () => {
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
      const layerNode = root.getSoleChildOfType(NodeTypes.LAYER_SHORTCUT);
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
    it("can parse correctly (nul [as text])", () => {
      Rule.tokenBuffer = stringToTokenBuffer('nul');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(root));
      assert.isNotNull(root.getSoleChildOfType(NodeTypes.NUL));
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
  describe("CommaRule Tests", () => {
    it("can construct a CommaRule", () => {
      Rule.tokenBuffer = stringToTokenBuffer('');
      const comma: Rule = new TokenRule(TokenTypes.COMMA);
      assert.isNotNull(comma);
    });
    it("can parse correctly (comma)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(',');
      const comma: Rule = new TokenRule(TokenTypes.COMMA);
      assert.isTrue(comma.parse(root));
    });
    it("can parse correctly (comma, space before)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' ,');
      const comma: Rule = new TokenRule(TokenTypes.COMMA);
      assert.isTrue(comma.parse(root));
    });
    it("can parse correctly (comma, space after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(', ');
      const comma: Rule = new TokenRule(TokenTypes.COMMA);
      assert.isTrue(comma.parse(root));
    });
    it("can parse correctly (comma, space before and after)", () => {
      Rule.tokenBuffer = stringToTokenBuffer(' , ');
      const comma: Rule = new TokenRule(TokenTypes.COMMA);
      assert.isTrue(comma.parse(root));
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
    it("can parse Khmer Angkor correctly", () => {
      const buffer: String    = new String(readFileSync('test/fixtures/keyboards/khmer_angkor.kmn'));
      Rule.tokenBuffer        = stringToTokenBuffer(buffer);
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
      const productionNodes   = root.getChildrenOfType(NodeTypes.PRODUCTION);
      assert.equal(productionNodes.length, 268);
      //assert.equal(root.toString(), '');
    });
    it("can parse Khmer Angkor correctly (round trip text)", () => {
      const buffer: String = new String(readFileSync('test/fixtures/keyboards/khmer_angkor.kmn'));
      Rule.tokenBuffer = stringToTokenBuffer(buffer);
      const kmnTreeRule: Rule = new KmnTreeRule();
      assert.isTrue(kmnTreeRule.parse(root));
      assert.deepEqual(root.toText(), buffer.toString());
    });
    it("can provide round trip text for baseline keyboards", () => {
      [
        'k_000___null_keyboard',
        'k_001___basic_input_unicodei',
        'k_002___basic_input_unicode',
        'k_003___nul',
        'k_004___basic_input__shift_2_',
        'k_005___nul_with_initial_context',
        'k_006___vkey_input__shift_ctrl_',
        'k_007___vkey_input__ctrl_alt_',
        'k_008___vkey_input__ctrl_alt_2_',
        'k_012___ralt',
        'k_013___deadkeys',
        'k_014___groups_and_virtual_keys',
        'k_015___ralt_2',
        'k_017___space_mnemonic_kbd',
        'k_018___nul_testing',
        'k_019___multiple_deadkeys',
        'k_020___deadkeys_and_backspace',
        'k_021___options',
        'k_022___options_with_preset',
        'k_023___options_with_save',
        'k_024___options_with_save_and_preset',
        'k_025___options_with_reset',
        'k_026___system_stores',
        'k_027___system_stores_2',
        'k_028___smp',
        'k_029___beep',
        'k_030___multiple_groups',
        'k_031___caps_lock',
        'k_032___caps_control',
        'k_033___caps_always_off',
        'k_034___options_double_set_reset',
        'k_035___options_double_set_staged',
        'k_036___options___double_reset_staged',
        'k_037___options___double_reset',
        'k_038___punctkeys',
        'k_039___generic_ctrlalt',
        'k_040___long_context',
        'k_041___long_context_and_deadkeys',
        'k_042___long_context_and_split_deadkeys',
        'k_043___output_and_keystroke',
        'k_044___if_and_context',
        'k_045___deadkey_and_context',
        'k_046___deadkey_and_contextex',
        'k_047___caps_always_off_initially_on',
        'k_048___modifier_keys_keep_context',
        'k_049___enter_invalidates_context',
        'k_050___nul_and_context',
        'k_051___if_and_context',
        'k_052___nul_and_index',
        'k_054___nul_and_contextex',
        'k_055___deadkey_cancelled_by_arrow',
      ].forEach((name) => {
        const buffer: String = new String(readFileSync(`../../../common/test/keyboards/baseline/${name}.kmn`));
        Rule.tokenBuffer = stringToTokenBuffer(buffer);
        const kmnTreeRule: Rule = new KmnTreeRule();
        root = new ASTNode(NodeTypes.TMP);
        assert.isTrue(kmnTreeRule.parse(root));
        assert.deepEqual(root.toText(), buffer.toString());
      });
    });
    it("can provide round trip text for repository keyboards (0-99)", () => {
      [
        'experimental/a/alephwithbeth/source/alephwithbeth',
        'experimental/a/amuzgo_guerrero/source/amuzgo_guerrero',
        'experimental/b/beria_erfe_phonetic/source/beria_erfe_phonetic',
        'experimental/c/carian/source/carian',
        'experimental/c/chalchiteko/source/chalchiteko',
        'experimental/c/chorasmian/source/chorasmian',
        'experimental/c/cim/source/cim',
        'experimental/c/colchis_phonetic/source/colchis_phonetic',
        'experimental/e/elfdalian/source/elfdalian',
        'experimental/e/elymaic/source/elymaic',
        'experimental/f/finongan/source/finongan',
        'experimental/g/gaahmg/source/gaahmg',
        'experimental/gff/gff_geez_emufi/source/gff_geez_emufi',
        'experimental/gff/gff_sbs/source/gff_sbs',
        'experimental/i/ife_mn/source/ife_mn',
        'experimental/i/irula_tamil_winscript/source/irula_tamil_winscript',
        'experimental/i/ishkashimi_cyrillic/source/ishkashimi_cyrillic',
        'experimental/k/kadu_kadu/source/kadu_kadu',
        'experimental/k/karambolpoular/source/karambolpoular',
        'experimental/k/kashmiri_phonetic/source/kashmiri_phonetic',
        'experimental/k/koreguaje/source/koreguaje',
        'experimental/kreative/kreative_sitelenpona_ucsur/source/kreative_sitelenpona_ucsur',
        'experimental/m/manga_ajami_azerty/source/manga_ajami_azerty',
        'experimental/m/manga_ajami_qwerty/source/manga_ajami_qwerty',
        'experimental/m/manga_latin_azerty/source/manga_latin_azerty',
        'experimental/m/manga_latin_qwerty/source/manga_latin_qwerty',
        'experimental/m/meitei_legacy/source/meitei_legacy',
        'experimental/m/miluk_hanis_siuslaw/source/miluk_hanis_siuslaw',
        'experimental/m/munji/source/munji',
        'experimental/mjh/mjh_aligali/source/mjh_aligali',
        'experimental/mjh/mjh_hudum/source/mjh_hudum',
        'experimental/mjh/mjh_manchu/source/mjh_manchu',
        'experimental/mjh/mjh_sibe/source/mjh_sibe',
        'experimental/mjh/mjh_todo/source/mjh_todo',
        'experimental/n/naijatype/source/naijatype',
        'experimental/n/nasa_yuwe/source/nasa_yuwe',
        'experimental/o/old_english/source/old_english',
        'experimental/p/paleohispanic/source/paleohispanic',
        'experimental/p/pingelap/source/pingelap',
        'experimental/p/pulaar_ajamiya_qwerty/source/pulaar_ajamiya_qwerty',
        'experimental/r/rossel/source/rossel',
        'experimental/r/ruanglat/source/ruanglat',
        'experimental/r/runeboard/source/runeboard',
        'experimental/r/rusyn/source/rusyn',
        'experimental/s/santali_latin/source/santali_latin',
        'experimental/s/satere/source/satere',
        'experimental/s/shaw_2layer/source/shaw_2layer',
        'experimental/s/shebek/source/shebek',
        'experimental/s/sp_lentan_ucsur/source/sp_lentan_ucsur',
        'experimental/s/sp_pochin_ucsur/source/sp_pochin_ucsur',
        'experimental/s/sp_wakalito_ucsur/source/sp_wakalito_ucsur',
        'experimental/sil/sil_lavukaleve/source/sil_lavukaleve',
        'experimental/t/taigi_poj/source/taigi_poj',
        'experimental/t/tangsa_lakhum/source/tangsa_lakhum',
        'experimental/t/teggargrent_lat/source/teggargrent_lat',
        'experimental/t/tengwar_quenya/source/tengwar_quenya',
        'experimental/t/tsakonian/source/tsakonian',
        'experimental/u/unifon/source/unifon',
        'experimental/w/wakhi_cyrillic/source/wakhi_cyrillic',
        'experimental/w/wakhi_standard/source/wakhi_standard',
        'experimental/w/wancho/source/wancho',
        'experimental/w/wenchow/source/wenchow',
        'experimental/w/winchus/source/winchus',
        'legacy/gff/gff_awn_7/archive/gff-awn-7',
        'legacy/gff/gff_bcq_7/archive/gff-bcq-7',
        'legacy/gff/gff_byn_7/archive/gff-byn-7',
        'legacy/gff/gff_gez_7/archive/gff-gez-7',
        'legacy/gff/gff_mym_7/archive/gff-mym-7',
        'legacy/gff/gff_sgw_7/archive/gff-sgw-7',
        'legacy/gff/gff_tir_er_7/archive/gff-tir-ER-7',
        'legacy/gff/gff_tir_et_7/archive/gff-tir-ET-7',
        'release/a/adiga_danef/source/adiga_danef',
        'release/a/afghan_turkmen/source/afghan_turkmen',
        'release/a/ahom_star/source/ahom_star',
        'release/a/aksarabali_panlex/source/aksarabali_panlex',
        'release/a/anglo_furthorc_english/source/anglo_furthorc_english',
        'release/a/anii/source/anii',
        'release/a/arabic_izza/source/arabic_izza',
        'release/a/arabic_w_o_dots/source/arabic_w_o_dots',
        'release/a/aramaic_hebrew/source/aramaic_hebrew',
        'release/a/arbore/source/arbore',
        'release/a/armenian_mnemonic/source/armenian_mnemonic',
        'release/a/armenian_mnemonic_r/source/armenian_mnemonic_r',
        'release/a/ausephon/source/ausephon',
        'release/a/avestan_phonetic/source/avestan_phonetic',
        'release/athinkra/athinkra_vai/source/athinkra_vai',
        'release/athinkra/athinkra_vai_typewriter/source/athinkra_vai_typewriter',
        'release/b/balochi_inpage/source/balochi_inpage',
        'release/b/balochi_latin/source/balochi_latin',
        'release/b/balochi_persian/source/balochi_persian',
        'release/b/balochi_phonetic/source/balochi_phonetic',
        'release/b/balochi_scientific/source/balochi_scientific',
        'release/b/balochi_urdu/source/balochi_urdu',
        'release/b/bamum/source/bamum',
        'release/b/bangla_joy/source/bangla_joy',
        'release/b/bangla_munir/source/bangla_munir',
        'release/b/bangla_probhat/source/bangla_probhat',
        'release/b/banne/source/banne',
        'release/b/bassa_vah/source/bassa_vah',
      ].splice(0, 34).forEach((name) => {
        const buffer: String = new String(readFileSync(`../../../../keyboards/${name}.kmn`));
        Rule.tokenBuffer = stringToTokenBuffer(buffer);
        const kmnTreeRule: Rule = new KmnTreeRule();
        root = new ASTNode(NodeTypes.TMP);
        assert.isTrue(kmnTreeRule.parse(root));
        assert.deepEqual(root.toText(), buffer.toString(), `${name}.kmn`);
      });
    });
  });
});

export function stringToTokenBuffer(buffer: String): TokenBuffer {
  const lexer = new Lexer(buffer);
  const tokens: Token[] = lexer.parse();
  return new TokenBuffer(tokens);
}
