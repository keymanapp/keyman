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
    const commentEndRule: Rule  = new CommentEndRule(tokenBuffer);
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
    const variableStoreAssign: Rule     = new VariableStoreAssignRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [
      stringSystemStoreAssign, variableStoreAssign
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
      node.addChild(storeNode);
      if (lineNode !== null) {
        node.addChild(lineNode);
      }
    }
    return parseSuccess;
  }
}

export class StringSystemStoreRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const store: Rule                 = new TokenRule(tokenBuffer, TokenTypes.STORE);
    const leftBracket: Rule           = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    const optWhitespace: Rule         = new OptionalWhiteSpaceRule(tokenBuffer);
    const amphasand: Rule             = new TokenRule(tokenBuffer, TokenTypes.AMPHASAND);
    const stringSystemStoreName: Rule = new StringSystemStoreNameRule(tokenBuffer);
    const rightBracket: Rule          = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule(tokenBuffer, [
      store, leftBracket, optWhitespace, amphasand, stringSystemStoreName, optWhitespace, rightBracket,
    ]);
  }
}

export class StringSystemStoreNameRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const bitmap: Rule           = new TokenRule(tokenBuffer, TokenTypes.BITMAP, true);
    const copyright: Rule        = new TokenRule(tokenBuffer, TokenTypes.COPYRIGHT, true);
    const displaymap: Rule       = new TokenRule(tokenBuffer, TokenTypes.DISPLAYMAP, true);
    const ethnologuecode: Rule   = new TokenRule(tokenBuffer, TokenTypes.ETHNOLOGUECODE, true);
    const includecodes: Rule     = new TokenRule(tokenBuffer, TokenTypes.INCLUDECODES, true);
    const keyboardversion: Rule  = new TokenRule(tokenBuffer, TokenTypes.KEYBOARDVERSION, true);
    const kmw_embedcss: Rule     = new TokenRule(tokenBuffer, TokenTypes.KMW_EMBEDCSS, true);
    const kmw_embedjs: Rule      = new TokenRule(tokenBuffer, TokenTypes.KMW_EMBEDJS, true);
    const kmw_helpfile: Rule     = new TokenRule(tokenBuffer, TokenTypes.KMW_HELPFILE, true);
    const kmw_helptext: Rule     = new TokenRule(tokenBuffer, TokenTypes.KMW_HELPTEXT, true);
    const kmw_rtl: Rule          = new TokenRule(tokenBuffer, TokenTypes.KMW_RTL, true);
    const language: Rule         = new TokenRule(tokenBuffer, TokenTypes.LANGUAGE, true);
    const layoutfile: Rule       = new TokenRule(tokenBuffer, TokenTypes.LAYOUTFILE, true);
    const message: Rule          = new TokenRule(tokenBuffer, TokenTypes.MESSAGE, true);
    const mnemoniclayout: Rule   = new TokenRule(tokenBuffer, TokenTypes.MNEMONICLAYOUT, true);
    const name: Rule             = new TokenRule(tokenBuffer, TokenTypes.NAME, true);
    const targets: Rule          = new TokenRule(tokenBuffer, TokenTypes.TARGETS, true);
    const version: Rule          = new TokenRule(tokenBuffer, TokenTypes.VERSION, true);
    const visualkeyboard: Rule   = new TokenRule(tokenBuffer, TokenTypes.VISUALKEYBOARD, true);
    const windowslanguages: Rule = new TokenRule(tokenBuffer, TokenTypes.WINDOWSLANGUAGES, true);
    this.rule = new AlternateRule(tokenBuffer, [
      bitmap, copyright, displaymap, ethnologuecode, includecodes, keyboardversion, kmw_embedcss,
      kmw_embedjs, kmw_helpfile, kmw_helptext, kmw_rtl, language, layoutfile, message,
      mnemoniclayout, name, targets, version, visualkeyboard, windowslanguages,
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
      const storeNode: ASTNode   = tmp.removeSoleChildOfType(NodeTypes.STORE);
      const lineNodes: ASTNode[] = tmp.removeChildrenOfType(NodeTypes.LINE);
      const textNodes: ASTNode[] = tmp.getChildren();
      storeNode.addChildren(textNodes);
      node.addChild(storeNode);
      node.addChildren(lineNodes);
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
    const store: Rule             = new TokenRule(tokenBuffer, TokenTypes.STORE);
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
      node.addToken(NodeTypes.STORE, parameters[0]);
    }
    return parseSuccess;
  };
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
      node.addChild(casedkeysNode);
      node.addChildren(lineNodes);
    }
    return parseSuccess;
  }
}

export class CasedkeysStoreRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const store: Rule         = new TokenRule(tokenBuffer, TokenTypes.STORE);
    const leftBracket: Rule   = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule(tokenBuffer);
    const amphasand: Rule     = new TokenRule(tokenBuffer, TokenTypes.AMPHASAND);
    const casedkeys: Rule     = new TokenRule(tokenBuffer, TokenTypes.CASEDKEYS, true);
    const rightBracket: Rule  = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule(tokenBuffer, [
      store, leftBracket, optWhitespace, amphasand, casedkeys, optWhitespace, rightBracket,
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
      node.addChild(hotkeyNode);
      if (lineNode !== null) {
        node.addChild(lineNode);
      }
    }
    return parseSuccess;
  }
}

export class HotkeyStoreRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const store: Rule         = new TokenRule(tokenBuffer, TokenTypes.STORE);
    const leftBracket: Rule   = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule(tokenBuffer);
    const amphasand: Rule     = new TokenRule(tokenBuffer, TokenTypes.AMPHASAND);
    const hotkey: Rule     = new TokenRule(tokenBuffer, TokenTypes.HOTKEY, true);
    const rightBracket: Rule  = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule(tokenBuffer, [
      store, leftBracket, optWhitespace, amphasand, hotkey, optWhitespace, rightBracket,
    ]);
  }
}
