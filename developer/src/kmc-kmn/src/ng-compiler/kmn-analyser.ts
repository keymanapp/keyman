/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { Token, TokenTypes } from "./lexer.js";
import { AlternateRule, TokenRule, OptionalRule, Rule, SequenceRule, SingleChildRule, parameterSequence, OneOrManyRule, ManyRule } from "./recursive-descent.js";
import { TokenBuffer } from "./token-buffer.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";

export class KmnTreeRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const line: Rule = new LineRule(tokenBuffer);
    this.rule = new ManyRule(tokenBuffer, line);
  }
}

export class LineRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const contentLine: Rule = new ContentLineRule(tokenBuffer);
    const blankLine: Rule   = new BlankLineRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [contentLine, blankLine]);
  }
}

export class ContentLineRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule(tokenBuffer);
    const content: Rule       = new ContentRule(tokenBuffer);
    const optCommentEnd: Rule = new OptionalCommentEndRule(tokenBuffer);
    const newline: Rule       = new TokenRule(tokenBuffer, TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule(tokenBuffer, [
      optWhitespace, content, optCommentEnd, newline
    ]);
  }
}

export class BlankLineRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const optCommentEnd: Rule = new OptionalCommentEndRule(tokenBuffer);
    const newline: Rule       = new TokenRule(tokenBuffer, TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule(tokenBuffer, [optCommentEnd, newline]);
  }
}

export class OptionalCommentEndRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const commentEndRule: Rule = new CommentEndRule(tokenBuffer);
    this.rule = new OptionalRule(tokenBuffer, commentEndRule);
  }
}

export class CommentEndRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule(tokenBuffer);
    const comment: Rule       = new TokenRule(tokenBuffer, TokenTypes.COMMENT);
    this.rule = new SequenceRule(tokenBuffer, [optWhitespace, comment]);
  }
}

export class PaddingRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const whitespace          = new TokenRule(tokenBuffer, TokenTypes.WHITESPACE);
    const continuationNewline = new ContinuationNewlineRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [whitespace, continuationNewline]);
  }
}

export class ContinuationNewlineRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule(tokenBuffer);
    const continuation: Rule  = new TokenRule(tokenBuffer, TokenTypes.CONTINUATION);
    const newline: Rule       = new TokenRule(tokenBuffer, TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule(tokenBuffer,
      [optWhitespace, continuation, optWhitespace, newline]
    );
  }
}

export class ContentRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const stringSystemStoreAssign: Rule = new StringSystemStoreAssignRule(tokenBuffer);
    const casedkeysStoreAssign: Rule    = new CasedkeysStoreAssignRule(tokenBuffer);
    const hotkeyStoreAssign: Rule       = new HotkeyStoreAssignRule(tokenBuffer);
    const variableStoreAssign: Rule     = new VariableStoreAssignRule(tokenBuffer);
    const ruleBlock: Rule               = new ruleBlockRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [
      stringSystemStoreAssign,
      casedkeysStoreAssign,
      hotkeyStoreAssign,
      variableStoreAssign,
      ruleBlock,
    ]);
  }
}

export class StringSystemStoreAssignRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const stringSystemStore: Rule = new StringSystemStoreRule(tokenBuffer);
    const padding: Rule           = new PaddingRule(tokenBuffer);
    const stringRule: Rule        = new TokenRule(tokenBuffer, TokenTypes.STRING, true);
    this.rule = new SequenceRule(tokenBuffer, [
      stringSystemStore, padding, stringRule,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const stringNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.STRING);
      const lineNode: ASTNode   = tmp.removeSoleChildOfType(NodeTypes.LINE);
      const storeNode: ASTNode  = tmp.getSoleChild();
      storeNode.addChild(stringNode);
      storeNode.addChild(lineNode);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

abstract class AbstractSystemStoreRule extends SingleChildRule {
  protected store: Rule;
  protected leftBracket: Rule;
  protected optWhitespace: Rule;
  protected amphasand: Rule;
  protected rightBracket: Rule;

  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.store         = new TokenRule(tokenBuffer, TokenTypes.STORE);
    this.leftBracket   = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    this.optWhitespace = new OptionalWhiteSpaceRule(tokenBuffer);
    this.amphasand     = new TokenRule(tokenBuffer, TokenTypes.AMPHASAND);
    this.rightBracket  = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
  }
}

export class StringSystemStoreRule extends AbstractSystemStoreRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store,
      this.leftBracket,
      this.optWhitespace,
      this.amphasand,
      stringSystemStoreName,
      this.optWhitespace,
      this.rightBracket,
    ]);
  }
}

export class StringSystemStoreNameRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const tokenRules: TokenRule[] = [];
    [
      TokenTypes.BITMAP,
      TokenTypes.COPYRIGHT,
      TokenTypes.DISPLAYMAP,
      TokenTypes.ETHNOLOGUECODE,
      TokenTypes.INCLUDECODES,
      TokenTypes.KEYBOARDVERSION,
      TokenTypes.KMW_EMBEDCSS,
      TokenTypes.KMW_EMBEDJS,
      TokenTypes.KMW_HELPFILE,
      TokenTypes.KMW_HELPTEXT,
      TokenTypes.KMW_RTL,
      TokenTypes.LANGUAGE,
      TokenTypes.LAYOUTFILE,
      TokenTypes.MESSAGE,
      TokenTypes.MNEMONICLAYOUT,
      TokenTypes.NAME,
      TokenTypes.TARGETS,
      TokenTypes.VERSION,
      TokenTypes.VISUALKEYBOARD,
      TokenTypes.WINDOWSLANGUAGES,
    ].forEach((tokenType) => {
      tokenRules.push(new TokenRule(tokenBuffer, tokenType, true));
    });
    this.rule = new AlternateRule(tokenBuffer, tokenRules);
  }
}

export class CasedkeysStoreAssignRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const casedkeysStore: Rule      = new CasedkeysStoreRule(tokenBuffer);
    const paddedText: Rule          = new PaddedTextRule(tokenBuffer);
    const oneOrManyPaddedText: Rule = new OneOrManyRule(tokenBuffer, paddedText);
    this.rule = new SequenceRule(tokenBuffer, [casedkeysStore, oneOrManyPaddedText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const casedkeysNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.CASEDKEYS);
      const lineNodes: ASTNode[]   = tmp.removeChildrenOfType(NodeTypes.LINE);
      const textNodes: ASTNode[]   = tmp.getChildren();
      casedkeysNode.addChildren(textNodes);
      casedkeysNode.addChildren(lineNodes);
      node.addChild(casedkeysNode);
    }
    return parseSuccess;
  }
}

export class CasedkeysStoreRule extends AbstractSystemStoreRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const casedkeys: Rule = new TokenRule(tokenBuffer, TokenTypes.CASEDKEYS, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store,
      this.leftBracket,
      this.optWhitespace,
      this.amphasand,
      casedkeys,
      this.optWhitespace,
      this.rightBracket,
    ]);
  }
}

export class HotkeyStoreAssignRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const hotkeyStore: Rule = new HotkeyStoreRule(tokenBuffer);
    const padding: Rule     = new PaddingRule(tokenBuffer);
    const virtualKey: Rule  = new VirtualKeyRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [hotkeyStore, padding, virtualKey]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const hotkeyNode: ASTNode     = tmp.removeSoleChildOfType(NodeTypes.HOTKEY);
      const lineNode: ASTNode       = tmp.removeSoleChildOfType(NodeTypes.LINE);
      const virtualKeyNode: ASTNode = tmp.getSoleChildOfType(NodeTypes.VIRTUAL_KEY);
      hotkeyNode.addChild(virtualKeyNode);
      hotkeyNode.addChild(lineNode);
      node.addChild(hotkeyNode);
    }
    return parseSuccess;
  }
}

export class HotkeyStoreRule extends AbstractSystemStoreRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const hotkey: Rule = new TokenRule(tokenBuffer, TokenTypes.HOTKEY, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store,
      this.leftBracket,
      this.optWhitespace,
      this.amphasand,
      hotkey,
      this.optWhitespace,
      this.rightBracket,
    ]);
  }
}

export class OptionalWhiteSpaceRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const whitespace: Rule = new TokenRule(tokenBuffer, TokenTypes.WHITESPACE);
    this.rule = new OptionalRule(tokenBuffer, whitespace);
  }
}

export class VariableStoreAssignRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const variableStore: Rule       = new VariableStoreRule(tokenBuffer);
    const paddedText: Rule          = new PaddedTextRule(tokenBuffer);
    const oneOrManyPaddedText: Rule = new OneOrManyRule(tokenBuffer, paddedText);
    this.rule = new SequenceRule(tokenBuffer, [variableStore, oneOrManyPaddedText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode: ASTNode     = tmp.removeSoleChildOfType(NodeTypes.STORE);
      const storeNameNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.STORENAME);
      const lineNodes: ASTNode[]   = tmp.removeChildrenOfType(NodeTypes.LINE);
      const textNodes: ASTNode[]   = tmp.getChildren();
      storeNode.addChild(storeNameNode);
      storeNode.addChildren(textNodes);
      storeNode.addChildren(lineNodes);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

export class PaddedTextRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const padding: Rule = new PaddingRule(tokenBuffer);
    const text: Rule    = new TextRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [padding, text]);
  }
}

export class TextRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const textRange: Rule  = new TextRangeRule(tokenBuffer);
    const simpleText: Rule = new SimpleTextRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [textRange, simpleText]);
  }
}

export class SimpleTextRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const stringRule = new TokenRule(tokenBuffer, TokenTypes.STRING, true);
    const virtualKey = new VirtualKeyRule(tokenBuffer);
    const uChar      = new TokenRule(tokenBuffer, TokenTypes.U_CHAR, true);
    this.rule = new AlternateRule(tokenBuffer, [ stringRule, virtualKey, uChar]);
  }
}

export class TextRangeRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const simpleText: Rule        = new SimpleTextRule(tokenBuffer);
    const rangeEnd: Rule          = new RangeEndRule(tokenBuffer);
    const oneOrManyRangeEnd: Rule = new OneOrManyRule(tokenBuffer, rangeEnd);
    this.rule = new SequenceRule(tokenBuffer, [simpleText, oneOrManyRangeEnd]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const rangeNode: ASTNode = new ASTNode(NodeTypes.RANGE);
      rangeNode.addChildren(tmp.getChildren());
      node.addChild(rangeNode);
    }
    return parseSuccess;
  }
}

export class RangeEndRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule(tokenBuffer);
    const range: Rule         = new TokenRule(tokenBuffer, TokenTypes.RANGE);
    const simpleText: Rule    = new SimpleTextRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      optWhitespace, range, optWhitespace, simpleText
    ]);
  }
}

export class VirtualKeyRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const leftSquare: Rule          = new TokenRule(tokenBuffer, TokenTypes.LEFT_SQ);
    const optWhitespace: Rule       = new OptionalWhiteSpaceRule(tokenBuffer);
    const spacedShiftCode: Rule     = new SpacedShiftCodeRule(tokenBuffer);
    const manySpacedShiftCode: Rule = new ManyRule(tokenBuffer, spacedShiftCode);
    const keyCode: Rule             = new TokenRule(tokenBuffer, TokenTypes.KEY_CODE, true);
    const rightSquare: Rule          = new TokenRule(tokenBuffer, TokenTypes.RIGHT_SQ);
    this.rule = new SequenceRule(tokenBuffer, [
      leftSquare, optWhitespace, manySpacedShiftCode, keyCode, optWhitespace, rightSquare
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNodes: ASTNode[] = tmp.removeChildrenOfType(NodeTypes.LINE);
      const virtualKeyNode       = new ASTNode(NodeTypes.VIRTUAL_KEY);
      virtualKeyNode.addChildren(tmp.getChildren());
      node.addChild(virtualKeyNode);
      node.addChildren(lineNodes);
    }
    return parseSuccess;
  }
}

export class SpacedShiftCodeRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const shiftCode: Rule  = new TokenRule(tokenBuffer, TokenTypes.SHIFT_CODE, true);
    const whitespace: Rule = new TokenRule(tokenBuffer, TokenTypes.WHITESPACE);
    this.rule = new SequenceRule(tokenBuffer, [shiftCode, whitespace]);
  }
}

export class VariableStoreRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const store: Rule             = new TokenRule(tokenBuffer, TokenTypes.STORE, true);
    const leftBracket: Rule       = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    const optWhitespace: Rule     = new OptionalWhiteSpaceRule(tokenBuffer);
    const variableStoreName: Rule = new VariableStoreNameRule(tokenBuffer);
    const rightBracket: Rule      = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule(tokenBuffer, [
      store, leftBracket, optWhitespace, variableStoreName, optWhitespace, rightBracket,
    ]);
  }
}

export class VariableStoreNameRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
  }

  public parse(node: ASTNode): boolean {
    const parameters: Token[]   = [];
    const parseSuccess: boolean = parameterSequence(this.tokenBuffer, parameters, 1);
    if (parseSuccess) {
      node.addToken(NodeTypes.STORENAME, parameters[0]);
    }
    return parseSuccess;
  };
}

export class ruleBlockRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const beginBlock: Rule  = new BeginBlockRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [beginBlock]);
  }
}

export class AnyStatementRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const any: Rule               = new TokenRule(tokenBuffer, TokenTypes.ANY, true);
    const leftBracket: Rule       = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    const optWhitespace: Rule     = new OptionalWhiteSpaceRule(tokenBuffer);
    const variableStoreName: Rule = new VariableStoreNameRule(tokenBuffer);
    const rightBracket: Rule      = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule(tokenBuffer, [
      any, leftBracket, optWhitespace, variableStoreName, optWhitespace, rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const anyNode       = tmp.getSoleChildOfType(NodeTypes.ANY);
      const storeNameNode = tmp.getSoleChildOfType(NodeTypes.STORENAME);
      anyNode.addChild(storeNameNode);
      node.addChild(anyNode);
    }
    return parseSuccess;
  }
}

export class BaselayoutStatementRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const baselayout: Rule    = new TokenRule(tokenBuffer, TokenTypes.BASELAYOUT, true);
    const leftBracket: Rule   = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule(tokenBuffer);
    const stringRule: Rule    = new TokenRule(tokenBuffer, TokenTypes.STRING, true);
    const rightBracket: Rule  = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule(tokenBuffer, [
      baselayout, leftBracket, optWhitespace, stringRule, optWhitespace, rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const baselayoutNode  = tmp.getSoleChildOfType(NodeTypes.BASELAYOUT);
      const stringNode      = tmp.getSoleChildOfType(NodeTypes.STRING);
      baselayoutNode.addChild(stringNode);
      node.addChild(baselayoutNode);
    }
    return parseSuccess;
  }
}

export class BeepStatementRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.rule = new TokenRule(tokenBuffer, TokenTypes.BEEP, true);
  }
}

export class BeginStatementRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const begin: Rule                = new TokenRule(tokenBuffer, TokenTypes.BEGIN, true);
    const optSpacedEntryPoint: Rule  = new OptionalSpacedEntryPointRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [begin, optSpacedEntryPoint]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const beginNode      = tmp.removeSoleChildOfType(NodeTypes.BEGIN);
      const entryPointNode = tmp.getSoleChild();
      beginNode.addChild(entryPointNode);
      node.addChild(beginNode);
    }
    return parseSuccess;
  }
}

export class SpacedEntryPointRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const entryPoint: Rule  = new EntryPointRule(tokenBuffer);
    const whitespace: Rule = new TokenRule(tokenBuffer, TokenTypes.WHITESPACE);
    this.rule = new SequenceRule(tokenBuffer, [whitespace, entryPoint]);
  }
}

export class EntryPointRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const unicode: Rule       = new TokenRule(tokenBuffer, TokenTypes.UNICODE, true);
    const newcontext: Rule    = new TokenRule(tokenBuffer, TokenTypes.NEWCONTEXT, true);
    const postkeystroke: Rule = new TokenRule(tokenBuffer, TokenTypes.POSTKEYSTROKE, true);
    const ansi: Rule          = new TokenRule(tokenBuffer, TokenTypes.ANSI, true);
    this.rule = new AlternateRule(tokenBuffer, [
      unicode, newcontext, postkeystroke, ansi,
    ]);
  }
}

export class OptionalSpacedEntryPointRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const spacedEntryPointRule: Rule = new SpacedEntryPointRule(tokenBuffer);
    this.rule = new OptionalRule(tokenBuffer, spacedEntryPointRule);
  }
}

export class UseStatementRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const use: Rule                = new TokenRule(tokenBuffer, TokenTypes.USE, true);
    const leftBracket: Rule        = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    const optWhitespace: Rule      = new OptionalWhiteSpaceRule(tokenBuffer);
    const groupNameOrKeyword: Rule = new GroupNameRule(tokenBuffer);
    const rightBracket: Rule       = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule(tokenBuffer, [
      use, leftBracket, optWhitespace, groupNameOrKeyword, optWhitespace, rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const useNode       = tmp.getSoleChildOfType(NodeTypes.USE);
      const groupNameNode = tmp.getSoleChildOfType(NodeTypes.GROUPNAME);
      useNode.addChild(groupNameNode);
      node.addChild(useNode);
    }
    return parseSuccess;
  }
}

export class GroupNameOrKeywordRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const groupName: Rule        = new GroupNameRule(tokenBuffer);
    const permittedKeyword: Rule = new PermittedKeywordRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [groupName, permittedKeyword]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      if (!tmp.hasChildOfType(NodeTypes.GROUPNAME)) {
        node.addToken(NodeTypes.GROUPNAME, tmp.getSoleChild().token);
      } else {
        node.addChild(tmp.getSoleChildOfType(NodeTypes.GROUPNAME));
      }
    }
    return parseSuccess;
  }
}

export class PermittedKeywordRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const tokenRules: TokenRule[] = [];
    [
      TokenTypes.POSTKEYSTROKE,
    ].forEach((tokenType) => {
      tokenRules.push(new TokenRule(tokenBuffer, tokenType, true));
    });
    this.rule = new AlternateRule(tokenBuffer, tokenRules);
  }
}

export class GroupNameRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
  }

  public parse(node: ASTNode): boolean {
    const parameters: Token[]   = [];
    const parseSuccess: boolean = parameterSequence(this.tokenBuffer, parameters, 1);
    if (parseSuccess) {
      node.addToken(NodeTypes.GROUPNAME, parameters[0]);
    }
    return parseSuccess;
  };
}

export class SpacedChevronRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const whitespace: Rule = new TokenRule(tokenBuffer, TokenTypes.WHITESPACE);
    const chevron: Rule    = new TokenRule(tokenBuffer, TokenTypes.CHEVRON);
    this.rule = new SequenceRule(tokenBuffer, [whitespace, chevron, whitespace]);
  }
}

export class BeginBlockRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const beginStatement: Rule = new BeginStatementRule(tokenBuffer);
    const spacedChevron: Rule  = new SpacedChevronRule(tokenBuffer);
    const useStatement: Rule   = new UseStatementRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [beginStatement, spacedChevron, useStatement]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const beginNode = tmp.getSoleChildOfType(NodeTypes.BEGIN);
      const useNode   = tmp.getSoleChildOfType(NodeTypes.USE);
      beginNode.addChild(useNode);
      node.addChild(beginNode);
    }
    return parseSuccess;
  }
}
