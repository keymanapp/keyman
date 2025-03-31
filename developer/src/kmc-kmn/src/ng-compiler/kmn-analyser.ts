/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { Token, TokenTypes } from "./lexer.js";
import { AlternateRule, TokenRule, OptionalRule, Rule, SequenceRule, SingleChildRule, parameterSequence, OneOrManyRule } from "./recursive-descent.js";
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
    this.rule = new StringSystemStoreAssignRule(tokenBuffer);
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
    const bitmap: Rule       = new TokenRule(tokenBuffer, TokenTypes.BITMAP, true);
    const copyright: Rule    = new TokenRule(tokenBuffer, TokenTypes.COPYRIGHT, true);
    const includecodes: Rule = new TokenRule(tokenBuffer, TokenTypes.INCLUDECODES, true);
    this.rule = new AlternateRule(tokenBuffer, [
      bitmap, copyright, includecodes,
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
    const stringRule = new TokenRule(tokenBuffer, TokenTypes.STRING, true);
    const uChar      = new TokenRule(tokenBuffer, TokenTypes.U_CHAR, true);
    this.rule = new AlternateRule(tokenBuffer, [stringRule, uChar]);
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
