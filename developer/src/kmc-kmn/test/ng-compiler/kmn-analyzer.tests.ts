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
import { BeginStatementRule, CompileTargetRule, ContentRule, EntryPointRule, GroupNameRule, Parser } from '../../src/ng-compiler/kmn-analyzer.js';
import { GroupQualifierRule, GroupStatementRule, InputBlockRule, InputContextRule, InputElementRule } from '../../src/ng-compiler/kmn-analyzer.js';
import { KeystrokeRule, KmnTreeRule, LhsBlockRule, LineRule, ModifierRule } from '../../src/ng-compiler/kmn-analyzer.js';
import { OutputStatementRule, PermittedKeywordRule, PlainTextRule, ProductionBlockRule, RhsBlockRule } from '../../src/ng-compiler/kmn-analyzer.js';
import { RuleBlockRule, SimpleTextRule, TextRangeRule, TextRule, UseStatementRule } from '../../src/ng-compiler/kmn-analyzer.js';
import { UsingKeysRule, VirtualKeyRule } from '../../src/ng-compiler/kmn-analyzer.js';
import { NodeType } from "../../src/ng-compiler/node-type.js";
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { existsSync } from 'node:fs';
import { baselineKeyboardNames, PATH_TO_BASELINE, PATH_TO_REPOSITORY, repositoryKeyboardNames } from './keyboard-names.js';
import { readFile } from './token-buffer.tests.js';

let tokenBuffer: TokenBuffer = null;
let root: ASTNode            = null;

describe("KMN Analyser Tests", () => {
  beforeEach(() => {
    root = new ASTNode(NodeType.TMP);
  });
  describe("KmnTreeRule Tests", () => {
    it("can construct a KmnTreeRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const kmnTree: Rule = new KmnTreeRule();
      assert.isNotNull(kmnTree);
    });
    it("can parse correctly (three lines)", () => {
      tokenBuffer = stringToTokenBuffer('store(&VERSION) "10.0"\nstore(&NAME) "Khmer Angkor"\nstore(&COPYRIGHT) "© SIL Global"\n');
      const kmnTree: Rule = new KmnTreeRule();
      assert.isTrue(kmnTree.parse(tokenBuffer, root));
      const storesNode = root.getSoleChildOfType(NodeType.STORES);
      assert.isNotNull(storesNode);
      const children = storesNode.getChildren();
      assert.equal(children.length, 3);
      assert.equal(children[0].nodeType, NodeType.VERSION);
      assert.equal(children[1].nodeType, NodeType.NAME);
      assert.equal(children[2].nodeType, NodeType.COPYRIGHT);
      const sourceCodeNode = root.getSoleChildOfType(NodeType.SOURCE_CODE);
      assert.isNotNull(sourceCodeNode);
      assert.equal(sourceCodeNode.getChildrenOfType(NodeType.LINE).length, 3);
    });
    it("can parse correctly (comment on final line with no newline)", () => {
      tokenBuffer = stringToTokenBuffer('group(emit) using keys\n  c empty final group causes keystroke to be emitted to app');
      const kmnTree: Rule = new KmnTreeRule();
      assert.isTrue(kmnTree.parse(tokenBuffer, root));
    });
  });
  describe("LineRule Tests", () => {
    it("can construct a LineRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const line: Rule = new LineRule();
      assert.isNotNull(line);
    });
    it("can parse correctly (no space before, no comment)", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeType.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeType.STRING);
      assert.equal(children[1].nodeType, NodeType.LINE);
    });
    it("can parse correctly (no space before, no comment, space before newline)", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" \n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeType.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeType.STRING);
      assert.equal(children[1].nodeType, NodeType.LINE);
    });
    it("can parse correctly (space before, no comment)", () => {
      tokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename"\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeType.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeType.STRING);
      assert.equal(children[1].nodeType, NodeType.LINE);
    });
    it("can parse correctly (no space before, comment)", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename" c a comment\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeType.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeType.STRING);
      assert.equal(children[1].nodeType, NodeType.LINE);
    });
    it("can parse correctly (space before, comment)", () => {
      tokenBuffer = stringToTokenBuffer(' store(&bitmap) "filename" c a comment\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeType.BITMAP);
      assert.equal(children[0].getSoleChild().nodeType, NodeType.STRING);
      assert.equal(children[1].nodeType, NodeType.LINE);
    });
    it("can parse correctly (variable store assign, comment)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780 c a comment\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeType.STORE);
      assert.equal(children[0].getSoleChildOfType(NodeType.STORENAME).getText(), 'c_out');
      assert.equal(children[1].nodeType, NodeType.LINE);
    });
    it("can parse correctly (variable store assign, continuation, comment)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780\\\nU+1781 c a comment\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 2);
      assert.equal(children[0].nodeType, NodeType.STORE);
      assert.equal(children[0].getSoleChildOfType(NodeType.STORENAME).getText(), 'c_out');
      assert.equal(children[0].getChildrenOfType(NodeType.U_CHAR).length, 2);
      assert.equal(children[1].nodeType, NodeType.LINE);
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
      tokenBuffer = stringToTokenBuffer(str);
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      const uCharNodes = storeNode.getChildrenOfType(NodeType.U_CHAR);
      assert.equal(uCharNodes.length, 35);
    });
    it("can parse correctly (ruleBlock)", () => {
      tokenBuffer = stringToTokenBuffer('U+17D2 + [K_D] > context(1) U+178F\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.INPUT_CONTEXT));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.U_CHAR));
    });
    it("can parse correctly (blank, no comment)", () => {
      tokenBuffer = stringToTokenBuffer('\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      assert.equal(root.getSoleChild().nodeType, NodeType.LINE);
      assert.isFalse(root.getSoleChild().hasChildren());
    });
    it("can parse correctly (blank, no comment, space before newline)", () => {
      tokenBuffer = stringToTokenBuffer(' \n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      assert.equal(root.getSoleChild().nodeType, NodeType.LINE);
      assert.isFalse(root.getSoleChild().hasChildren());
    });
    it("can parse correctly (blank, comment)", () => {
      tokenBuffer = stringToTokenBuffer('c This tells Keyman which keys should have casing behavior applied\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      assert.equal(root.getSoleChild().nodeType, NodeType.LINE);
      assert.isFalse(root.getSoleChild().hasChildren());
    });
    it("can parse correctly (blank, space before comment)", () => {
      tokenBuffer = stringToTokenBuffer(' c This tells Keyman which keys should have casing behavior applied\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      assert.equal(root.getSoleChild().nodeType, NodeType.LINE);
      assert.isFalse(root.getSoleChild().hasChildren());
    });
    it("can parse correctly (compile target)", () => {
      tokenBuffer = stringToTokenBuffer('$keyman: store(&bitmap) "filename"\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 3);
      assert.equal(children[0].nodeType, NodeType.KEYMAN);
      assert.equal(children[1].nodeType, NodeType.BITMAP);
      assert.equal(children[1].getSoleChild().nodeType, NodeType.STRING);
      assert.equal(children[2].nodeType, NodeType.LINE);
    });
    it("can parse correctly (compile target, plus, virtual key, u_char)", () => {
      tokenBuffer = stringToTokenBuffer('$keymanonly: + [CTRL "."] > U+135E\n');
      const line: Rule = new LineRule();
      assert.isTrue(line.parse(tokenBuffer, root));
      const children = root.getChildren();
      assert.equal(children.length, 3);
      assert.equal(children[0].nodeType, NodeType.KEYMANONLY);
      assert.equal(children[1].nodeType, NodeType.PRODUCTION);
      assert.equal(children[2].nodeType, NodeType.LINE);
    });
  });
  // describe("FinalLineRule Tests", () => {
  //   it("can construct a FinalLineRule", () => {
  //     tokenBuffer = stringToTokenBuffer('');
  //     const line: Rule = new FinalLineRule();
  //     assert.isNotNull(line);
  //   });
  //   it("can parse correctly (empty line)", () => {
  //     tokenBuffer = stringToTokenBuffer('');
  //     const line: Rule = new FinalLineRule();
  //     assert.isTrue(line.parse(tokenBuffer, root));
  //     assert.isFalse(root.hasChildren());
  //   });
  //   it("can parse correctly (space)", () => {
  //     tokenBuffer = stringToTokenBuffer(' ');
  //     const line: Rule = new FinalLineRule();
  //     assert.isTrue(line.parse(tokenBuffer, root));
  //     const lineNode = root.getSoleChildOfType(NodeType.LINE);
  //     assert.isNotNull(lineNode);
  //     assert.deepEqual(lineNode.token.line, ' ');
  //   });
  //   it("can parse correctly (comment)", () => {
  //     tokenBuffer = stringToTokenBuffer('c final line');
  //     const line: Rule = new FinalLineRule();
  //     assert.isTrue(line.parse(tokenBuffer, root));
  //     const lineNode = root.getSoleChildOfType(NodeType.LINE);
  //     assert.isNotNull(lineNode);
  //     assert.deepEqual(lineNode.token.line, 'c final line');
  //   });
  // });
  describe("CompileTargetRule Tests", () => {
    it("can construct a CompileTargetRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isNotNull(compileTarget);
    });
    it("can parse correctly (KEYMAN)", () => {
      tokenBuffer = stringToTokenBuffer('$keyman:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.KEYMAN));
    });
    it("can parse correctly (KEYMANONLY)", () => {
      tokenBuffer = stringToTokenBuffer('$keymanonly:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.KEYMANONLY));
    });
    it("can parse correctly (KEYMANWEB)", () => {
      tokenBuffer = stringToTokenBuffer('$keymanweb:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.KEYMANWEB));
    });
    it("can parse correctly (KMFL)", () => {
      tokenBuffer = stringToTokenBuffer('$kmfl:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.KMFL));
    });
    it("can parse correctly (WEAVER)", () => {
      tokenBuffer = stringToTokenBuffer('$weaver:');
      const compileTarget: Rule = new CompileTargetRule();
      assert.isTrue(compileTarget.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.WEAVER));
    });
  });
  describe("ContentRule Tests", () => {
    it("can construct a ContentRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const content: Rule = new ContentRule();
      assert.isNotNull(content);
    });
    it("can parse correctly (system store assign)", () => {
      tokenBuffer = stringToTokenBuffer('store(&bitmap) "filename"');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BITMAP));
      assert.isNotNull(root.getSoleChild().getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (casedkeys store assign)", () => {
      tokenBuffer = stringToTokenBuffer('store(&casedkeys) [K_A]');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      const casedkeysNode = root.getSoleChildOfType(NodeType.CASEDKEYS)
      assert.isNotNull(casedkeysNode);
      assert.isNotNull(casedkeysNode.getSoleChildOfType(NodeType.VIRTUAL_KEY));
    });
    it("can parse correctly (hotkey store assign)", () => {
      tokenBuffer = stringToTokenBuffer('store(&hotkey) [K_A]');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      const hotkeyNode = root.getSoleChildOfType(NodeType.HOTKEY)
      assert.isNotNull(hotkeyNode);
      assert.isNotNull(hotkeyNode.getSoleChildOfType(NodeType.VIRTUAL_KEY));
    });
    it("can parse correctly (caps always off)", () => {
      tokenBuffer = stringToTokenBuffer('caps always off');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.CAPSALWAYSOFF));
    });
    it("can parse correctly (caps on only)", () => {
      tokenBuffer = stringToTokenBuffer('caps on only');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.CAPSONONLY));
    });
    it("can parse correctly (shift frees caps)", () => {
      tokenBuffer = stringToTokenBuffer('shift frees caps');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.SHIFTFREESCAPS));
    });
    it("can parse correctly (header assign)", () => {
      tokenBuffer = stringToTokenBuffer('hotkey [SHIFT K_H]');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.HOTKEY_HEADER));
    });
    it("can parse correctly (normal store assign)", () => {
      tokenBuffer = stringToTokenBuffer('store(c_out) U+1780');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      const storeNode = root.getSoleChildOfType(NodeType.STORE)
      assert.isNotNull(storeNode);
      assert.isNotNull(storeNode.getSoleChildOfType(NodeType.U_CHAR));
    });
    it("can parse correctly (plus, any, index)", () => {
      tokenBuffer = stringToTokenBuffer('+ any(c_key) > index(c_out,1)');
      const content: Rule = new ContentRule();
      assert.isTrue(content.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      assert.isNotNull(productionNode.getSoleChildOfType(NodeType.LHS));
      assert.isNotNull(productionNode.getSoleChildOfType(NodeType.RHS));
    });
  });
  describe("TextRule Tests", () => {
    it("can construct a TextRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const text: Rule = new TextRule();
      assert.isNotNull(text);
    });
    it("can parse correctly (text range)", () => {
      tokenBuffer = stringToTokenBuffer('"a".."c"');
      const text: Rule = new TextRule();
      assert.isTrue(text.parse(tokenBuffer, root));
      const rangeNode = root.getSoleChildOfType(NodeType.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeType.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeType.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (simple text)", () => {
      tokenBuffer = stringToTokenBuffer('"a"');
      const text: Rule = new TextRule();
      assert.isTrue(text.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (outs statement)", () => {
      tokenBuffer = stringToTokenBuffer('outs(digit)');
      const text: Rule = new TextRule();
      assert.isTrue(text.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.OUTS));
    });
  });
  describe("PlainTextRule Tests", () => {
    it("can construct a PlainTextRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const plainText: Rule = new PlainTextRule();
      assert.isNotNull(plainText);
    });
    it("can parse correctly (text range)", () => {
      tokenBuffer = stringToTokenBuffer('"a".."c"');
      const plainText: Rule = new PlainTextRule();
      assert.isTrue(plainText.parse(tokenBuffer, root));
      const rangeNode = root.getSoleChildOfType(NodeType.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeType.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeType.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (simple text)", () => {
      tokenBuffer = stringToTokenBuffer('"a"');
      const plainText: Rule = new PlainTextRule();
      assert.isTrue(plainText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (outs statement)", () => {
      tokenBuffer = stringToTokenBuffer('outs(digit)');
      const plainText: Rule = new PlainTextRule();
      assert.isFalse(plainText.parse(tokenBuffer, root));
    });
  });
  describe("SimpleTextRule Tests", () => {
    it("can construct a SimpleTextRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const simpleText: Rule = new SimpleTextRule();
      assert.isNotNull(simpleText);
    });
    it("can parse correctly (string)", () => {
      tokenBuffer = stringToTokenBuffer('"a"');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (virtualKey)", () => {
      tokenBuffer = stringToTokenBuffer('[K_K]');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.VIRTUAL_KEY));
    });
    it("can parse correctly (u_char)", () => {
      tokenBuffer = stringToTokenBuffer('U+1780');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.U_CHAR));
    });
    it("can parse correctly (named constant)", () => {
      tokenBuffer = stringToTokenBuffer('$CCedilla');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NAMED_CONSTANT));
    });
    it("can parse correctly (hangul)", () => {
      tokenBuffer = stringToTokenBuffer('$HANGUL_SYLLABLE_A');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.HANGUL));
    });
    it("can parse correctly (decimal, space)", () => {
      tokenBuffer = stringToTokenBuffer('d99 ');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.DECIMAL));
    });
    it("can parse correctly (hexadecimal, space)", () => {
      tokenBuffer = stringToTokenBuffer('xa99 ');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.HEXADECIMAL));
    });
    it("can parse correctly (octal, space)", () => {
      tokenBuffer = stringToTokenBuffer('77 ');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.OCTAL));
    });
    it("can parse correctly (nul)", () => {
      tokenBuffer = stringToTokenBuffer('nul');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NUL));
    });
    it("can parse correctly (deadkey)", () => {
      tokenBuffer = stringToTokenBuffer('dk(1)');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.DEADKEY));
    });
    it("can parse correctly (beep)", () => {
      tokenBuffer = stringToTokenBuffer('beep');
      const simpleText: Rule = new SimpleTextRule();
      assert.isTrue(simpleText.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BEEP));
    });
  });
  describe("TextRangeRule Tests", () => {
    it("can construct a TextRangeRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const textRange: Rule = new TextRangeRule();
      assert.isNotNull(textRange);
    });
    it("can parse correctly (string range)", () => {
      tokenBuffer = stringToTokenBuffer('"a".."c"');
      const textRange: Rule = new TextRangeRule();
      assert.isTrue(textRange.parse(tokenBuffer, root));
      const rangeNode = root.getSoleChildOfType(NodeType.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeType.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeType.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (string range, space before range)", () => {
      tokenBuffer = stringToTokenBuffer('"a" .."c"');
      const textRange: Rule = new TextRangeRule();
      assert.isTrue(textRange.parse(tokenBuffer, root));
      const rangeNode = root.getSoleChildOfType(NodeType.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeType.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeType.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (string range, space after range)", () => {
      tokenBuffer = stringToTokenBuffer('"a".. "c"');
      const textRange: Rule = new TextRangeRule();
      assert.isTrue(textRange.parse(tokenBuffer, root));
      const rangeNode = root.getSoleChildOfType(NodeType.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeType.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeType.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (string range, space before and after range)", () => {
      tokenBuffer = stringToTokenBuffer('"a" .. "c"');
      const textRange: Rule = new TextRangeRule();
      assert.isTrue(textRange.parse(tokenBuffer, root));
      const rangeNode = root.getSoleChildOfType(NodeType.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeType.STRING);
      assert.equal(children[0].getText(), '"a"');
      assert.equal(children[1].nodeType, NodeType.STRING);
      assert.equal(children[1].getText(), '"c"');
    });
    it("can parse correctly (virtual key range)", () => {
      tokenBuffer = stringToTokenBuffer('[K_A]..[K_C]');
      const textRange: Rule = new TextRangeRule();
      assert.isTrue(textRange.parse(tokenBuffer, root));
      const rangeNode = root.getSoleChildOfType(NodeType.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeType.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].nodeType, NodeType.VIRTUAL_KEY);
      assert.equal(children[1].getSoleChild().getText(), 'K_C');
    });
    it("can parse correctly (multiple virtual key range)", () => {
      tokenBuffer = stringToTokenBuffer('[K_A]..[K_C]..[K_E]');
      const textRange: Rule = new TextRangeRule();
      assert.isTrue(textRange.parse(tokenBuffer, root));
      const rangeNode = root.getSoleChildOfType(NodeType.RANGE);
      assert.isNotNull(rangeNode);
      const children = rangeNode.getChildren();
      assert.equal(children[0].nodeType, NodeType.VIRTUAL_KEY);
      assert.equal(children[0].getSoleChild().getText(), 'K_A');
      assert.equal(children[1].nodeType, NodeType.VIRTUAL_KEY);
      assert.equal(children[1].getSoleChild().getText(), 'K_C');
      assert.equal(children[2].nodeType, NodeType.VIRTUAL_KEY);
      assert.equal(children[2].getSoleChild().getText(), 'K_E');
    });
  });
  describe("VirtualKeyRule Tests", () => {
    it("can construct a VirtualKeyRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isNotNull(virtualKey);
    });
    it("can parse correctly (simple virtual key)", () => {
      tokenBuffer = stringToTokenBuffer('[K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.KEY_CODE));
    });
    it("can parse correctly (single shiftcode virtual key)", () => {
      tokenBuffer = stringToTokenBuffer('[SHIFT K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.KEY_CODE));
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.MODIFIER));
    });
    it("can parse correctly (two shiftcode virtual key)", () => {
      tokenBuffer = stringToTokenBuffer('[SHIFT CTRL K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.KEY_CODE));
      const modifiers = virtualKeyNode.getChildrenOfType(NodeType.MODIFIER);
      assert.equal(modifiers.length, 2);
      assert.equal(modifiers[0].token.text, 'SHIFT');
      assert.equal(modifiers[1].token.text, 'CTRL');
    });
    it("can parse correctly (simple virtual key, space before)", () => {
      tokenBuffer = stringToTokenBuffer('[ K_K]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.KEY_CODE));
    });
    it("can parse correctly (simple virtual key, space after)", () => {
      tokenBuffer = stringToTokenBuffer('[K_K ]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.KEY_CODE));
    });
    it("can parse correctly (simple virtual key, space before and after)", () => {
      tokenBuffer = stringToTokenBuffer('[ K_K ]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.KEY_CODE));
    });
    it("can parse correctly (virtual character)", () => {
      tokenBuffer = stringToTokenBuffer('["A"]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (virtual character with modifier)", () => {
      tokenBuffer = stringToTokenBuffer('[CTRL "A"]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.STRING));
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.MODIFIER));
    });
    it("can parse correctly (ISO9995 code)", () => {
      tokenBuffer = stringToTokenBuffer('[CTRL A10]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.KEY_CODE));
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.MODIFIER));
    });
    it("can parse correctly (ISO9995 code parsed as DECIMAL)", () => {
      tokenBuffer = stringToTokenBuffer('[CTRL D10]');
      const virtualKey: Rule = new VirtualKeyRule();
      assert.isTrue(virtualKey.parse(tokenBuffer, root));
      const virtualKeyNode = root.getSoleChildOfType(NodeType.VIRTUAL_KEY);
      assert.isNotNull(virtualKeyNode);
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.DECIMAL));
      assert.isNotNull(virtualKeyNode.getSoleChildOfType(NodeType.MODIFIER));
    });
  });
  describe("ModifierRule Tests", () => {
    it("can construct a ModifierRule", () => {
      tokenBuffer = stringToTokenBuffer('');
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
        tokenBuffer = stringToTokenBuffer(`${code} `);
        const modifier: Rule = new ModifierRule();
        root = new ASTNode(NodeType.TMP);
        assert.isTrue(modifier.parse(tokenBuffer, root));
        assert.equal(root.getSoleChildOfType(NodeType.MODIFIER).getText(), code);
      });
    });
  });
  describe("RuleBlockRule Tests", () => {
    it("can construct a RuleBlockRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const ruleBlock: Rule = new RuleBlockRule();
      assert.isNotNull(ruleBlock);
    });
    it("can parse correctly (production block)", () => {
      tokenBuffer = stringToTokenBuffer('U+17D2 + [K_D] > context(1) U+178F');
      const productionBlock: Rule = new RuleBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.INPUT_CONTEXT));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.U_CHAR));
    });
  });
  describe("BeginStatementRule Tests", () => {
    it("can construct an BeginStatementRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const beginBlock: Rule = new BeginStatementRule();
      assert.isNotNull(beginBlock);
    });
    it("can parse correctly (no entrypoint)", () => {
      tokenBuffer = stringToTokenBuffer('begin > use(main)');
      const beginBlock: Rule = new BeginStatementRule();
      assert.isTrue(beginBlock.parse(tokenBuffer, root));
      const beginNode = root.getSoleChildOfType(NodeType.BEGIN);
      assert.isNotNull(beginNode);
      assert.isNotNull(beginNode.getSoleChildOfType(NodeType.USE));
    });
    it("can parse correctly (entrypoint)", () => {
      tokenBuffer = stringToTokenBuffer('begin unicode > use(main)');
      const beginBlock: Rule = new BeginStatementRule();
      assert.isTrue(beginBlock.parse(tokenBuffer, root));
      const beginNode = root.getSoleChildOfType(NodeType.BEGIN);
      assert.isNotNull(beginNode);
      assert.isNotNull(beginNode.getSoleChildOfType(NodeType.UNICODE));
      assert.isNotNull(beginNode.getSoleChildOfType(NodeType.USE));
    });
  });
  describe("EntryPointRule Tests", () => {
    it("can construct an EntryPointRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const entryPoint: Rule = new EntryPointRule();
      assert.isNotNull(entryPoint);
    });
    it("can parse correctly (unicode)", () => {
      tokenBuffer = stringToTokenBuffer('unicode');
      const entryPoint: Rule = new EntryPointRule();
      assert.isTrue(entryPoint.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.UNICODE));
    });
    it("can parse correctly (newcontext)", () => {
      tokenBuffer = stringToTokenBuffer('newcontext');
      const entryPoint: Rule = new EntryPointRule();
      assert.isTrue(entryPoint.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NEWCONTEXT));
    });
    it("can parse correctly (postkeystroke)", () => {
      tokenBuffer = stringToTokenBuffer('postkeystroke');
      const entryPoint: Rule = new EntryPointRule();
      assert.isTrue(entryPoint.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.POSTKEYSTROKE));
    });
    it("can parse correctly (ansi)", () => {
      tokenBuffer = stringToTokenBuffer('ansi');
      const entryPoint: Rule = new EntryPointRule();
      assert.isTrue(entryPoint.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.ANSI));
    });
  });
  describe("UseStatementRule Tests", () => {
    it("can construct an UseStatementRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const useStatement: Rule = new UseStatementRule();
      assert.isNotNull(useStatement);
    });
    it("can parse correctly (parameter)", () => {
      tokenBuffer = stringToTokenBuffer('use(main)');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(tokenBuffer, root));
      const useNode = root.getSoleChildOfType(NodeType.USE);
      assert.isNotNull(useNode);
      assert.isNotNull(useNode.getSoleChildOfType(NodeType.GROUPNAME));
    });
    it("can parse correctly (space before parameter)", () => {
      tokenBuffer = stringToTokenBuffer('use( main)');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(tokenBuffer, root));
      const useNode = root.getSoleChildOfType(NodeType.USE);
      assert.isNotNull(useNode);
      const groupNameNode: ASTNode = useNode.getSoleChildOfType(NodeType.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
    });
    it("can parse correctly (space after parameter)", () => {
      tokenBuffer = stringToTokenBuffer('use(main )');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(tokenBuffer, root));
      const useNode = root.getSoleChildOfType(NodeType.USE);
      assert.isNotNull(useNode);
      const groupNameNode: ASTNode = useNode.getSoleChildOfType(NodeType.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
    });
    it("can parse correctly (space before and after parameter)", () => {
      tokenBuffer = stringToTokenBuffer('use( main )');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(tokenBuffer, root));
      const useNode = root.getSoleChildOfType(NodeType.USE);
      assert.isNotNull(useNode);
      const groupNameNode: ASTNode = useNode.getSoleChildOfType(NodeType.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getText(), 'main');
    });
    it("can parse correctly (permitted keyword)", () => {
      tokenBuffer = stringToTokenBuffer('use(postkeystroke)');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(tokenBuffer, root));
      const useNode = root.getSoleChildOfType(NodeType.USE);
      assert.isNotNull(useNode);
      assert.isNotNull(useNode.getSoleChildOfType(NodeType.GROUPNAME));
    });
    it("can parse correctly (multi word name)", () => {
      tokenBuffer = stringToTokenBuffer('use(Unicode Group)');
      const useStatement: Rule = new UseStatementRule();
      assert.isTrue(useStatement.parse(tokenBuffer, root));
      const useNode = root.getSoleChildOfType(NodeType.USE);
      assert.isNotNull(useNode);
      assert.isNotNull(useNode.getSoleChildOfType(NodeType.GROUPNAME));
    });
  });
  describe("GroupNameRule Tests", () => {
    it("can construct a GroupNameRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const groupName: Rule = new GroupNameRule();
      assert.isNotNull(groupName);
    });
    it("can parse correctly (parameter)", () => {
      tokenBuffer = stringToTokenBuffer('main');
      const groupName: Rule = new GroupNameRule();
      assert.isTrue(groupName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.GROUPNAME).getText(), 'main');
    });
    it("can parse correctly (octal)", () => {
      tokenBuffer = stringToTokenBuffer('1');
      const groupName: Rule = new GroupNameRule();
      assert.isTrue(groupName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.GROUPNAME).getText(), '1');
    });
    it("can parse correctly (permitted keyword)", () => {
      tokenBuffer = stringToTokenBuffer('newcontext');
      const groupName: Rule = new GroupNameRule();
      assert.isTrue(groupName.parse(tokenBuffer, root));
      assert.equal(root.getSoleChildOfType(NodeType.GROUPNAME).getText(), 'newcontext');
    });
    it("can parse correctly (multi word name)", () => {
      tokenBuffer = stringToTokenBuffer('Unicode Group');
      const groupName: Rule = new GroupNameRule();
      assert.isTrue(groupName.parse(tokenBuffer, root));
      const groupNameNode = root.getSoleChildOfType(NodeType.GROUPNAME);
      assert.isNotNull(groupNameNode);
      assert.equal(groupNameNode.getSoleChildOfType(NodeType.UNICODE).getText(), 'Unicode');
      assert.equal(groupNameNode.getSoleChildOfType(NodeType.PARAMETER).getText(), 'Group');
    });
  });
  describe("PermittedKeywordRule Tests", () => {
    it("can construct a PermittedKeywordRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const permittedKeyword: Rule = new PermittedKeywordRule();
      assert.isNotNull(permittedKeyword);
    });
    it("can parse correctly", () => {
      // *_HEADER tokens must be followed by a space
      [
        {input: 'always',        nodeType: NodeType.ALWAYS},
        {input: 'ansi',          nodeType: NodeType.ANSI},
        {input: 'beep',          nodeType: NodeType.BEEP},
        {input: 'begin',         nodeType: NodeType.BEGIN},
        {input: 'bitmap ',       nodeType: NodeType.BITMAP_HEADER},
        {input: 'caps',          nodeType: NodeType.CAPS},
        {input: 'context',       nodeType: NodeType.CONTEXT},
        {input: 'copyright ',    nodeType: NodeType.COPYRIGHT_HEADER},
        {input: 'd1 ',           nodeType: NodeType.DECIMAL},
        {input: 'frees',         nodeType: NodeType.FREES},
        {input: 'xa1 ',          nodeType: NodeType.HEXADECIMAL},
        {input: 'hotkey ',       nodeType: NodeType.HOTKEY_HEADER},
        {input: 'keys',          nodeType: NodeType.KEYS},
        {input: 'language ',     nodeType: NodeType.LANGUAGE_HEADER},
        {input: 'layout ',       nodeType: NodeType.LAYOUT_HEADER},
        {input: 'match',         nodeType: NodeType.MATCH},
        {input: 'name ',         nodeType: NodeType.NAME_HEADER},
        {input: 'newcontext',    nodeType: NodeType.NEWCONTEXT},
        {input: 'nomatch',       nodeType: NodeType.NOMATCH},
        {input: 'nul',           nodeType: NodeType.NUL},
        {input: '10 ',           nodeType: NodeType.OCTAL},
        {input: 'off',           nodeType: NodeType.OFF},
        {input: 'on',            nodeType: NodeType.ON},
        {input: 'only',          nodeType: NodeType.ONLY},
        {input: 'postkeystroke', nodeType: NodeType.POSTKEYSTROKE},
        {input: 'readonly',      nodeType: NodeType.READONLY},
        {input: 'return',        nodeType: NodeType.RETURN},
        {input: 'shift',         nodeType: NodeType.SHIFT},
        {input: 'unicode',       nodeType: NodeType.UNICODE},
        {input: 'using',         nodeType: NodeType.USING},
        {input: 'version ',      nodeType: NodeType.VERSION_HEADER},
      ].forEach((testCase) => {
        tokenBuffer = stringToTokenBuffer(testCase.input);
        const permittedKeyword: Rule = new PermittedKeywordRule();
        root = new ASTNode(NodeType.TMP);
        assert.isTrue(permittedKeyword.parse(tokenBuffer, root));
        assert.isNotNull(root.getSoleChildOfType(testCase.nodeType), `${testCase.input}`);
      });
    });
  });
  describe("GroupStatementRule Tests", () => {
    it("can construct an GroupStatementRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isNotNull(groupStatement);
    });
    it("can parse correctly (parameter, no qualifier)", () => {
      tokenBuffer = stringToTokenBuffer('group(main)');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(tokenBuffer, root));
      const groupNode = root.getSoleChildOfType(NodeType.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeType.GROUPNAME));
    });
    it("can parse correctly (octal, no qualifier)", () => {
      tokenBuffer = stringToTokenBuffer('group(1)');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(tokenBuffer, root));
      const groupNode = root.getSoleChildOfType(NodeType.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeType.GROUPNAME));
    });
    it("can parse correctly (permitted keyword, no qualifier)", () => {
      tokenBuffer = stringToTokenBuffer('group(newcontext)');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(tokenBuffer, root));
      const groupNode = root.getSoleChildOfType(NodeType.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeType.GROUPNAME));
    });
    it("can parse correctly (using keys)", () => {
      tokenBuffer = stringToTokenBuffer('group(main) using keys');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(tokenBuffer, root));
      const groupNode = root.getSoleChildOfType(NodeType.GROUP);
      assert.isNotNull(groupNode);
      assert.isNotNull(groupNode.getSoleChildOfType(NodeType.USING_KEYS));
      assert.isNull(groupNode.getSoleChildOfType(NodeType.READONLY));
    });
    it("can parse correctly (readonly)", () => {
      tokenBuffer = stringToTokenBuffer('group(main) readonly');
      const groupStatement: Rule = new GroupStatementRule();
      assert.isTrue(groupStatement.parse(tokenBuffer, root));
      const groupNode = root.getSoleChildOfType(NodeType.GROUP);
      assert.isNotNull(groupNode);
      assert.isNull(groupNode.getSoleChildOfType(NodeType.USING_KEYS));
      assert.isNotNull(groupNode.getSoleChildOfType(NodeType.READONLY));
    });
  });
  describe("GroupQualifierRule Tests", () => {
    it("can construct an GroupQualifierRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const groupQualifier: Rule = new GroupQualifierRule();
      assert.isNotNull(groupQualifier);
    });
    it("can parse correctly (using keys)", () => {
      tokenBuffer = stringToTokenBuffer('using keys');
      const groupQualifier: Rule = new GroupQualifierRule();
      assert.isTrue(groupQualifier.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.USING_KEYS));
    });
    it("can parse correctly (readonly)", () => {
      tokenBuffer = stringToTokenBuffer('readonly');
      const groupQualifier: Rule = new GroupQualifierRule();
      assert.isTrue(groupQualifier.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.READONLY));
    });
  });
  describe("UsingKeysRule Tests", () => {
    it("can construct an UsingKeysRuleRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const usingKeys: Rule = new UsingKeysRule();
      assert.isNotNull(usingKeys);
    });
    it("can parse correctly", () => {
      tokenBuffer = stringToTokenBuffer('using keys');
      const usingKeys: Rule = new UsingKeysRule();
      assert.isTrue(usingKeys.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.USING_KEYS));
    });
  });
  describe("ProductionBlockRule Tests", () => {
    it("can construct a ProductionBlockRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isNotNull(productionBlock);
    });
    it("can parse correctly (plus, any, index)", () => {
      tokenBuffer = stringToTokenBuffer('+ any(c_key) > index(c_out,1)');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.INDEX));
    });
    it("can parse correctly (plus, virtual key, two uChars)", () => {
      tokenBuffer = stringToTokenBuffer('+ [SHIFT K_A] > U+17B6 U+17C6');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      const uCharNodes = rhsNode.getChildrenOfType(NodeType.U_CHAR);
      assert.equal(uCharNodes.length, 2);
    });
    it("can parse correctly (u_char, plus, virtual key, context, u_char)", () => {
      tokenBuffer = stringToTokenBuffer('U+17D2 + [K_D] > context(1) U+178F');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.INPUT_CONTEXT));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.U_CHAR));
    });
    it("can parse correctly (if, plus, string, string, set, reset, set, reset)", () => {
      tokenBuffer = stringToTokenBuffer("if(foo = '1') + 'a' > 'foo.' set(foo='2') reset(foo) set(foo='3') reset(foo)");
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.IF));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      const outputNodes = rhsNode.getChildren();
      assert.equal(outputNodes.length, 5);
      assert.equal(outputNodes[0].nodeType, NodeType.STRING);
      assert.equal(outputNodes[1].nodeType, NodeType.SET);
      assert.equal(outputNodes[2].nodeType, NodeType.RESET);
      assert.equal(outputNodes[3].nodeType, NodeType.SET);
      assert.equal(outputNodes[4].nodeType, NodeType.RESET);
    });
    it("can parse correctly (dk, string, dk, string, dk, plus, string, string, context, string)", () => {
      tokenBuffer = stringToTokenBuffer("dk(1) 'a' dk(2) 'b' dk(3) + 'z' > '<' context '>'");
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      const inputContextNode = lhsNode.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const inputNodes = inputContextNode.getChildren();
      assert.equal(inputNodes.length, 5);
      assert.equal(inputNodes[0].nodeType, NodeType.DEADKEY);
      assert.equal(inputNodes[1].nodeType, NodeType.STRING);
      assert.equal(inputNodes[2].nodeType, NodeType.DEADKEY);
      assert.equal(inputNodes[3].nodeType, NodeType.STRING);
      assert.equal(inputNodes[4].nodeType, NodeType.DEADKEY);
      const keystrokeNode = lhsNode.getSoleChildOfType(NodeType.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      const outputNodes = rhsNode.getChildren();
      assert.equal(outputNodes.length, 3);
      assert.equal(outputNodes[0].nodeType, NodeType.STRING);
      assert.equal(outputNodes[1].nodeType, NodeType.CONTEXT);
      assert.equal(outputNodes[2].nodeType, NodeType.STRING);
    });
    it("can parse correctly (two uChars, uChar)", () => {
      tokenBuffer = stringToTokenBuffer('U+17C1 U+17B6 > U+17C4');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.INPUT_CONTEXT));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.U_CHAR));
    });
    it("can parse correctly (two uChars, two uChars)", () => {
      tokenBuffer = stringToTokenBuffer('U+17C6 U+17BB > U+17BB U+17C6');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.INPUT_CONTEXT));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      const uCharNodes = rhsNode.getChildrenOfType(NodeType.U_CHAR);
      assert.equal(uCharNodes.length, 2);
    });
    it("can parse correctly (platform, use)", () => {
      tokenBuffer = stringToTokenBuffer('platform("touch") > use(detectStartOfSentence)');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.USE));
    });
    it("can parse correctly (ifs, any, context, layer)", () => {
      tokenBuffer = stringToTokenBuffer('if(&newLayer = "") if(&layer = "shift") any(ShiftOutSingle) > context layer("default")');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      const ifNodes = lhsNode.getChildrenOfType(NodeType.IF);
      assert.equal(ifNodes.length, 2);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.INPUT_CONTEXT));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.LAYER_SHORTCUT));
    });
    it("can parse correctly (plus, virtual key, u_char)", () => {
      tokenBuffer = stringToTokenBuffer('+ [CTRL "."] > U+135E');
      const productionBlock: Rule = new ProductionBlockRule();
      assert.isTrue(productionBlock.parse(tokenBuffer, root));
      const productionNode = root.getSoleChildOfType(NodeType.PRODUCTION);
      assert.isNotNull(productionNode);
      const lhsNode = productionNode.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.KEYSTROKE));
      const rhsNode = productionNode.getSoleChildOfType(NodeType.RHS)
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.U_CHAR));
    });
  });
  describe("LhsBlockRule Tests", () => {
    it("can construct a LhsBlockRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isNotNull(lhsBlock);
    });
    it("can parse correctly (match)", () => {
      tokenBuffer = stringToTokenBuffer('match');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(tokenBuffer, root));
      const lhsNode = root.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.MATCH));
    });
    it("can parse correctly (nomatch)", () => {
      tokenBuffer = stringToTokenBuffer('nomatch');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(tokenBuffer, root));
      const lhsNode = root.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.NOMATCH));
    });
    it("can parse correctly (if-like)", () => {
      tokenBuffer = stringToTokenBuffer('platform("touch")');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(tokenBuffer, root));
      const lhsNode = root.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
    });
    it("can parse correctly (if-like, input context)", () => {
      tokenBuffer = stringToTokenBuffer('platform("touch") any(digit)');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(tokenBuffer, root));
      const lhsNode = root.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      assert.isNotNull(lhsNode.getSoleChildOfType(NodeType.INPUT_CONTEXT));
    });
    it("can parse correctly (plus, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('+ any(c_key)');
      const lhsBlock: Rule = new LhsBlockRule();
      assert.isTrue(lhsBlock.parse(tokenBuffer, root));
      const lhsNode = root.getSoleChildOfType(NodeType.LHS);
      assert.isNotNull(lhsNode);
      const keystrokeNode = lhsNode.getSoleChildOfType(NodeType.KEYSTROKE)
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeType.ANY));
    });
  });
  describe("InputBlockRule Tests", () => {
    it("can construct a InputBlockRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const inputBlock: Rule = new InputBlockRule();
      assert.isNotNull(inputBlock);
    });
    it("can parse correctly (two uChars)", () => {
      tokenBuffer = stringToTokenBuffer('U+17C1 U+17B6');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const uCharNodes = inputContextNode.getChildrenOfType(NodeType.U_CHAR);
      assert.equal(uCharNodes.length, 2);
      assert.equal(uCharNodes[0].getText(), 'U+17C1');
      assert.equal(uCharNodes[1].getText(), 'U+17B6');
    });
    it("can parse correctly (if-like block, two uChars)", () => {
      tokenBuffer = stringToTokenBuffer('platform("touch") U+17C1 U+17B6');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const uCharNodes = inputContextNode.getChildrenOfType(NodeType.U_CHAR);
      assert.equal(uCharNodes.length, 2);
      assert.equal(uCharNodes[0].getText(), 'U+17C1');
      assert.equal(uCharNodes[1].getText(), 'U+17B6');
    });
    it("can parse correctly (nul, if-like block, deadkey)", () => {
      tokenBuffer = stringToTokenBuffer('nul platform("touch") dk(1)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getChildrenOfType(NodeType.DEADKEY));
    });
    it("can parse correctly (keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (if-like block, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('platform("touch") any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      assert.isNotNull(root.getSoleChildOfType(NodeType.INPUT_CONTEXT));
    });
    it("can parse correctly (nul, if-like block, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('nul platform("touch") any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      assert.isNotNull(root.getSoleChildOfType(NodeType.INPUT_CONTEXT));
    });
    it("can parse correctly (plus, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('+ any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      const keystrokeNode = root.getSoleChildOfType(NodeType.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (if-like block, plus, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('platform("touch") + any(c_key)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      assert.isNotNull(root.getSoleChildOfType(NodeType.KEYSTROKE));
    });
    it("can parse correctly (inputContext, plus, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('any(output) + any(diacriticKey)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeType.ANY));
      const keystrokeNode = root.getSoleChildOfType(NodeType.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (if-like, inputContext, plus, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('platform("touch") any(output) + any(diacriticKey)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeType.ANY));
      const keystrokeNode = root.getSoleChildOfType(NodeType.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (nul, if-like, plus, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('nul platform("touch") + any(diacriticKey)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      const keystrokeNode = root.getSoleChildOfType(NodeType.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (nul, if-like, inputContext, plus, keystroke)", () => {
      tokenBuffer = stringToTokenBuffer('nul platform("touch") dk(1) + any(diacriticKey)');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getChildrenOfType(NodeType.DEADKEY));
      const keystrokeNode = root.getSoleChildOfType(NodeType.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (nul only)", () => {
      tokenBuffer = stringToTokenBuffer('nul');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NUL));
    });
    it("can parse correctly (nul, if-like block)", () => {
      tokenBuffer = stringToTokenBuffer('nul platform("touch")');
      const inputBlock: Rule = new InputBlockRule();
      assert.isTrue(inputBlock.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NUL));
      assert.isNotNull(root.getSoleChildOfType(NodeType.PLATFORM_SHORTCUT));
    });
  });
  describe("InputContextRule Tests", () => {
    it("can construct an InputContextRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const inputContext: Rule = new InputContextRule();
      assert.isNotNull(inputContext);
    });
    it("can parse correctly (any)", () => {
      tokenBuffer = stringToTokenBuffer('any(c_shifter)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(tokenBuffer, root));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (two anys)", () => {
      tokenBuffer = stringToTokenBuffer('any(digit) any(number)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(tokenBuffer, root));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const anyNodes = inputContextNode.getChildrenOfType(NodeType.ANY);
      assert.equal(anyNodes.length, 2);
    });
    it("can parse correctly (text, any)", () => {
      tokenBuffer = stringToTokenBuffer('U+17D2 any(c_out)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(tokenBuffer, root));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeType.U_CHAR));
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (any, context)", () => {
      tokenBuffer = stringToTokenBuffer('any(digit) context(1)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(tokenBuffer, root));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeType.ANY));
      assert.isNotNull(inputContextNode.getSoleChildOfType(NodeType.CONTEXT));
    });
    it("can parse correctly (two anys, continuation)", () => {
      tokenBuffer = stringToTokenBuffer('any(digit)\\\nany(number)');
      const inputContext: Rule = new InputContextRule();
      assert.isTrue(inputContext.parse(tokenBuffer, root));
      const inputContextNode = root.getSoleChildOfType(NodeType.INPUT_CONTEXT);
      assert.isNotNull(inputContextNode);
      const anyNodes = inputContextNode.getChildrenOfType(NodeType.ANY);
      assert.equal(anyNodes.length, 2);
    });
  });
  describe("InputElementRule Tests", () => {
    it("can construct an InputElementRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const inputElement: Rule = new InputElementRule();
      assert.isNotNull(inputElement);
    });
    it("can parse correctly (any)", () => {
      tokenBuffer = stringToTokenBuffer('any(digit)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(tokenBuffer, root));
      const anyNode = root.getSoleChildOfType(NodeType.ANY);
      assert.isNotNull(anyNode);
      assert.isNotNull(anyNode.getSoleChildOfType(NodeType.STORENAME));
    });
    it("can parse correctly (notAny)", () => {
      tokenBuffer = stringToTokenBuffer('notany(digit)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(tokenBuffer, root));
      const notAnyNode = root.getSoleChildOfType(NodeType.NOTANY);
      assert.isNotNull(notAnyNode);
      assert.isNotNull(notAnyNode.getSoleChildOfType(NodeType.STORENAME));
    });
    it("can parse correctly (deadKey [as text])", () => {
      tokenBuffer = stringToTokenBuffer('dk(deadkeyName)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(tokenBuffer, root));
      const deadKeyNode = root.getSoleChildOfType(NodeType.DEADKEY);
      assert.isNotNull(deadKeyNode);
      assert.isNotNull(deadKeyNode.getSoleChildOfType(NodeType.DEADKEYNAME));
    });
    it("can parse correctly (contextStatement)", () => {
      tokenBuffer = stringToTokenBuffer('context(1)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(tokenBuffer, root));
      const contextNode = root.getSoleChildOfType(NodeType.CONTEXT);
      assert.isNotNull(contextNode);
      assert.equal(contextNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (indexStatement)", () => {
      tokenBuffer = stringToTokenBuffer('index(cons, 1)');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(tokenBuffer, root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'cons');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (text)", () => {
      tokenBuffer = stringToTokenBuffer('U+1780');
      const inputElement: Rule = new InputElementRule();
      assert.isTrue(inputElement.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.U_CHAR));
    });
  });
  describe("KeystrokeRule Tests", () => {
    it("can construct an KeystrokeRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const keystroke: Rule = new KeystrokeRule();
      assert.isNotNull(keystroke);
    });
    it("can parse correctly (any)", () => {
      tokenBuffer = stringToTokenBuffer('+ any(digit)');
      const keystroke: Rule = new KeystrokeRule();
      assert.isTrue(keystroke.parse(tokenBuffer, root));
      const keystrokeNode = root.getSoleChildOfType(NodeType.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeType.ANY));
    });
    it("can parse correctly (text)", () => {
      tokenBuffer = stringToTokenBuffer('+ U+1780');
      const keystroke: Rule = new KeystrokeRule();
      assert.isTrue(keystroke.parse(tokenBuffer, root));
      const keystrokeNode = root.getSoleChildOfType(NodeType.KEYSTROKE);
      assert.isNotNull(keystrokeNode);
      assert.isNotNull(keystrokeNode.getSoleChildOfType(NodeType.U_CHAR));
    });
  });
  describe("RhsBlockRule Tests", () => {
    it("can construct a RhsBlockRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isNotNull(rhsBlock);
    });
    it("can parse correctly (context output block)", () => {
      tokenBuffer = stringToTokenBuffer('context layer("shift")');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(tokenBuffer, root));
      const rhsNode = root.getSoleChildOfType(NodeType.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.CONTEXT));
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.LAYER_SHORTCUT));
    });
    it("can parse correctly (context only)", () => {
      tokenBuffer = stringToTokenBuffer('context');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(tokenBuffer, root));
      const rhsNode = root.getSoleChildOfType(NodeType.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.CONTEXT));
    });
    it("can parse correctly (return only)", () => {
      tokenBuffer = stringToTokenBuffer('return');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(tokenBuffer, root));
      const rhsNode = root.getSoleChildOfType(NodeType.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.RETURN));
    });
    it("can parse correctly (nul only)", () => {
      tokenBuffer = stringToTokenBuffer('nul');
      const rhsBlock: Rule = new RhsBlockRule();
      assert.isTrue(rhsBlock.parse(tokenBuffer, root));
      const rhsNode = root.getSoleChildOfType(NodeType.RHS);
      assert.isNotNull(rhsNode);
      assert.isNotNull(rhsNode.getSoleChildOfType(NodeType.NUL));
    });
  });
  describe("OutputStatementRule Tests", () => {
    it("can construct a OutputStatementRule", () => {
      tokenBuffer = stringToTokenBuffer('');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isNotNull(outputStatement);
    });
    it("can parse correctly (use statement)", () => {
      tokenBuffer = stringToTokenBuffer('use(main)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const useNode = root.getSoleChildOfType(NodeType.USE);
      assert.isNotNull(useNode);
      assert.isNotNull(useNode.getSoleChildOfType(NodeType.GROUPNAME));
    });
    it("can parse correctly (call statement)", () => {
      tokenBuffer = stringToTokenBuffer('call(storeName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const callNode = root.getSoleChildOfType(NodeType.CALL);
      assert.isNotNull(callNode);
      assert.isNotNull(callNode.getSoleChildOfType(NodeType.STORENAME));
    });
    it("can parse correctly (set statement)", () => {
      tokenBuffer = stringToTokenBuffer('set(storeName = "value")');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const setNode = root.getSoleChildOfType(NodeType.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.STORENAME));
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (save statement)", () => {
      tokenBuffer = stringToTokenBuffer('save(storeName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const saveNode = root.getSoleChildOfType(NodeType.SAVE);
      assert.isNotNull(saveNode);
      assert.isNotNull(saveNode.getSoleChildOfType(NodeType.STORENAME));
    });
    it("can parse correctly (reset statement)", () => {
      tokenBuffer = stringToTokenBuffer('reset(storeName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const resetNode = root.getSoleChildOfType(NodeType.RESET);
      assert.isNotNull(resetNode);
      assert.isNotNull(resetNode.getSoleChildOfType(NodeType.STORENAME));
    });
    it("can parse correctly (deadkey statement [as text])", () => {
      tokenBuffer = stringToTokenBuffer('dk(deadkeyName)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const deadKeyNode = root.getSoleChildOfType(NodeType.DEADKEY);
      assert.isNotNull(deadKeyNode);
      assert.isNotNull(deadKeyNode.getSoleChildOfType(NodeType.DEADKEYNAME));
    });
    it("can parse correctly (set system store statement)", () => {
      tokenBuffer = stringToTokenBuffer('set(&layer = "value")');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const setNode = root.getSoleChildOfType(NodeType.SET);
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.LAYER));
      assert.isNotNull(setNode.getSoleChildOfType(NodeType.STRING));
    });
    it("can parse correctly (layer statement)", () => {
      tokenBuffer = stringToTokenBuffer('layer("shift")');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const layerNode = root.getSoleChildOfType(NodeType.LAYER_SHORTCUT);
      assert.isNotNull(layerNode);
      assert.equal(layerNode.getSoleChildOfType(NodeType.STRING).getText(), '"shift"');
    });
    it("can parse correctly (index statement)", () => {
      tokenBuffer = stringToTokenBuffer('index(c_out,1)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const indexNode = root.getSoleChildOfType(NodeType.INDEX);
      assert.isNotNull(indexNode);
      assert.equal(indexNode.getSoleChildOfType(NodeType.STORENAME).getText(), 'c_out');
      assert.equal(indexNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (context statement)", () => {
      tokenBuffer = stringToTokenBuffer('context(1)');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      const contextNode = root.getSoleChildOfType(NodeType.CONTEXT);
      assert.isNotNull(contextNode);
      assert.equal(contextNode.getSoleChildOfType(NodeType.OFFSET).getText(), '1');
    });
    it("can parse correctly (text)", () => {
      tokenBuffer = stringToTokenBuffer('U+1780');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.U_CHAR));
    });
    it("can parse correctly (nul [as text])", () => {
      tokenBuffer = stringToTokenBuffer('nul');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.NUL));
    });
    it("can parse correctly (beep)", () => {
      tokenBuffer = stringToTokenBuffer('beep');
      const outputStatement: Rule = new OutputStatementRule();
      assert.isTrue(outputStatement.parse(tokenBuffer, root));
      assert.isNotNull(root.getSoleChildOfType(NodeType.BEEP));
    });
  });
  describe("Parser Tests", () => {
    it("can construct a Parser", () => {
      tokenBuffer = stringToTokenBuffer('');
      const parser: Parser = new Parser(tokenBuffer);
      assert.isNotNull(parser);
    });
    it("can parse Khmer Angkor correctly", () => {
      const buffer: string = readFile('test/fixtures/keyboards/khmer_angkor.kmn');
      tokenBuffer          = stringToTokenBuffer(buffer);
      const parser: Parser = new Parser(tokenBuffer);
      root = parser.parse();
      const storesNode = root.getSoleChildOfType(NodeType.STORES);
      assert.isNotNull(storesNode);
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.VERSION));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.NAME));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.COPYRIGHT));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.MESSAGE));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.TARGETS));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.DISPLAYMAP));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.LAYOUTFILE));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.KEYBOARDVERSION));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.BITMAP));
      assert.isNotNull(storesNode.getSoleChildOfType(NodeType.VISUALKEYBOARD));
      const beginNodes = root.getChildrenOfType(NodeType.BEGIN);
      assert.equal(beginNodes.length, 2);
      assert.equal(beginNodes[0].getDescendents(NodeType.GROUPNAME)[0].getText(), 'main');
      assert.equal(beginNodes[1].getDescendents(NodeType.GROUPNAME)[0].getText(), 'PostKeystroke');
      const storeNodes = storesNode.getChildrenOfType(NodeType.STORE);
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
        assert.equal(storeNodes[idx].getDescendents(NodeType.STORENAME)[0].getText(), name);
      });
      const groupNodes = root.getChildrenOfType(NodeType.GROUP);
      assert.equal(groupNodes.length, 5);
      assert.equal(groupNodes[0].getSoleChildOfType(NodeType.GROUPNAME).getText(), 'NewContext');
      assert.isNotNull(groupNodes[0].getSoleChildOfType(NodeType.READONLY));
      assert.equal(groupNodes[0].getChildrenOfType(NodeType.PRODUCTION).length, 1);
      assert.equal(groupNodes[1].getSoleChildOfType(NodeType.GROUPNAME).getText(), 'PostKeystroke');
      assert.isNotNull(groupNodes[1].getSoleChildOfType(NodeType.READONLY));
      assert.equal(groupNodes[1].getChildrenOfType(NodeType.PRODUCTION).length, 4);
      assert.equal(groupNodes[2].getSoleChildOfType(NodeType.GROUPNAME).getText(), 'detectStartOfSentence');
      assert.isNotNull(groupNodes[2].getSoleChildOfType(NodeType.READONLY));
      assert.equal(groupNodes[2].getChildrenOfType(NodeType.PRODUCTION).length, 1);
      assert.equal(groupNodes[3].getSoleChildOfType(NodeType.GROUPNAME).getText(), 'main');
      assert.isNotNull(groupNodes[3].getSoleChildOfType(NodeType.USING_KEYS));
      assert.equal(groupNodes[3].getChildrenOfType(NodeType.PRODUCTION).length, 54);
      assert.equal(groupNodes[4].getSoleChildOfType(NodeType.GROUPNAME).getText(), 'normalise');
      assert.isFalse(groupNodes[4].hasChildrenOfType(NodeType.USING_KEYS));
      assert.equal(groupNodes[4].getChildrenOfType(NodeType.PRODUCTION).length, 208);
      assert.isFalse(groupNodes[4].hasChildrenOfType(NodeType.READONLY));
      assert.isNotNull(root.getSoleChildOfType(NodeType.SOURCE_CODE));
      //assert.equal(root.toString(), '');
    });
    it("can parse Khmer Angkor correctly (round trip text)", () => {
      const buffer: string = readFile('test/fixtures/keyboards/khmer_angkor.kmn');
      tokenBuffer = stringToTokenBuffer(buffer);
      const parser: Parser = new Parser(tokenBuffer);
      root = parser.parse();
      assert.equal(root.toText(), buffer);
    });
    it("can provide round trip text for baseline keyboards", function() {
      if (!existsSync(PATH_TO_BASELINE)) {
        this.skip();
      }
      baselineKeyboardNames().forEach((name) => {
        const buffer: string = readFile(`${PATH_TO_BASELINE}${name}.kmn`);
        tokenBuffer = stringToTokenBuffer(buffer);
        const parser: Parser = new Parser(tokenBuffer);
        root = parser.parse();
        assert.equal(root.toText(), buffer, `${name}.kmn`);
      });
    });
    // if the keyboard repository is absent, this loop will not execute as it will be of
    // zero length, so all repository keyboard round trip tests will be (silently) skipped
    for (let i = 0; i < repositoryKeyboardNames().length; i+=100) {
      const start = i;
      const end   = Math.min(i+100, repositoryKeyboardNames().length);
      it(`can provide round trip text for repository keyboards (${start}-${end-1})`, function() {
        repositoryKeyboardNames().slice(start, end).forEach((name) => {
          const buffer: string = readFile(`${PATH_TO_REPOSITORY}${name}.kmn`);
          tokenBuffer = stringToTokenBuffer(buffer);
          const parser: Parser = new Parser(tokenBuffer);
          root = parser.parse();
          assert.equal(root.toText(), buffer, `${name}.kmn`);
        });
      }).timeout(50000);
    }
  });
});

export function stringToTokenBuffer(buffer: string): TokenBuffer {
  const lexer = new Lexer(buffer);
  const tokens: Token[] = lexer.parse();
  return new TokenBuffer(tokens);
}
