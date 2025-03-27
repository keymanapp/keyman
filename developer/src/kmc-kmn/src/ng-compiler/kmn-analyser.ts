/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-18
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 */

import { TokenTypes } from "./lexer.js";
import { AlternateRule, TokenRule, OptionalRule, Rule, SequenceRule, SingleChildRule, OneOrManyRule } from "./recursive-descent.js";
import { TokenBuffer } from "./token-buffer.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";

export class MultiLineRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const continuationLine          = new ContinuationLineRule(tokenBuffer);
    const oneOrManyContinuationLine = new OneOrManyRule(tokenBuffer, continuationLine);
    const soloLine                  = new SoloLineRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [oneOrManyContinuationLine, soloLine]);
  }
}

export class SoloLineRule extends SingleChildRule {
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

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNode: ASTNode    = tmp.removeSoleChildOfType(NodeTypes.LINE);
      const contentNode: ASTNode = tmp.getSoleChild();
      lineNode.addChild(contentNode);
      node.addChild(lineNode);
    }
    return parseSuccess;
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

export class ContinuationLineRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const optWhitespace: Rule   = new OptionalWhiteSpaceRule(tokenBuffer);
    const content: Rule         = new ContentRule(tokenBuffer);
    const whitespace: Rule      = new TokenRule(tokenBuffer, TokenTypes.WHITESPACE);
    const continuationEnd: Rule = new ContinuationEndRule(tokenBuffer);
    const newline: Rule         = new TokenRule(tokenBuffer, TokenTypes.NEWLINE, true);
    this.rule = new SequenceRule(tokenBuffer, [
      optWhitespace, content, whitespace, continuationEnd, newline
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const lineNode: ASTNode    = tmp.removeSoleChildOfType(NodeTypes.LINE);
      const contentNode: ASTNode = tmp.getSoleChild();
      lineNode.addChild(contentNode);
      node.addChild(lineNode);
    }
    return parseSuccess;
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

export class ContinuationEndRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const continuation: Rule  = new TokenRule(tokenBuffer, TokenTypes.CONTINUATION);
    const optWhitespace: Rule = new OptionalWhiteSpaceRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [continuation, optWhitespace]);
  }
}

export class ContentRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.rule = new SystemStoreAssignRule(tokenBuffer);
  }
}

export class SystemStoreAssignRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const bitmapStoreAssign: Rule       = new BitmapStoreAssignRule(tokenBuffer);
    const copyrightStoreAssign: Rule    = new CopyrightStoreAssignRule(tokenBuffer);
    const includecodesStoreAssign: Rule = new IncludecodesStoreAssignRule(tokenBuffer);
    this.rule = new AlternateRule(tokenBuffer, [
      bitmapStoreAssign, copyrightStoreAssign, includecodesStoreAssign,
    ]);
  }
}

abstract class SystemStoreAssignAbstractRule extends SingleChildRule {
  protected whitespace: Rule     = null;
  protected stringRule: Rule     = null;
  protected storeType: NodeTypes = null;

  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.whitespace = new TokenRule(tokenBuffer, TokenTypes.WHITESPACE);
    this.stringRule = new TokenRule(tokenBuffer, TokenTypes.STRING, true);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode: ASTNode  = tmp.getSoleChildOfType(this.storeType);
      const stringNode: ASTNode = tmp.getSoleChildOfType(NodeTypes.STRING);
      storeNode.addChild(stringNode);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

abstract class SystemStoreAbstractRule extends SingleChildRule {
  protected store: Rule           = null;
  protected leftBracket: Rule     = null;
  protected optWhitespace: Rule   = null;
  protected amphasand: Rule       = null;
  protected rightBracket: Rule    = null;

  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.store         = new TokenRule(tokenBuffer, TokenTypes.STORE);
    this.leftBracket   = new TokenRule(tokenBuffer, TokenTypes.LEFT_BR);
    this.optWhitespace = new OptionalWhiteSpaceRule(tokenBuffer);
    this.amphasand     = new TokenRule(tokenBuffer, TokenTypes.AMPHASAND);
    this.rightBracket  = new TokenRule(tokenBuffer, TokenTypes.RIGHT_BR);
  }
}

export class BitmapStoreAssignRule extends SystemStoreAssignAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.BITMAP;
    const bitmapStore: Rule = new BitmapStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      bitmapStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class BitmapStoreRule extends SystemStoreAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const bitmap: Rule = new TokenRule(tokenBuffer, TokenTypes.BITMAP, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store, this.leftBracket, this.optWhitespace, this.amphasand, bitmap, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class CopyrightStoreAssignRule extends SystemStoreAssignAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.COPYRIGHT;
    const copyrightStore: Rule = new CopyrightStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      copyrightStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class CopyrightStoreRule extends SystemStoreAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const copyright: Rule = new TokenRule(tokenBuffer, TokenTypes.COPYRIGHT, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store, this.leftBracket, this.optWhitespace, this.amphasand, copyright, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class IncludecodesStoreAssignRule extends SystemStoreAssignAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    this.storeType = NodeTypes.INCLUDECODES;
    const includecodesStore: Rule = new IncludecodesStoreRule(tokenBuffer);
    this.rule = new SequenceRule(tokenBuffer, [
      includecodesStore, this.whitespace, this.stringRule,
    ]);
  }
}

export class IncludecodesStoreRule extends SystemStoreAbstractRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const includecodes: Rule = new TokenRule(tokenBuffer, TokenTypes.INCLUDECODES, true);
    this.rule = new SequenceRule(tokenBuffer, [
      this.store, this.leftBracket, this.optWhitespace, this.amphasand, includecodes, this.optWhitespace, this.rightBracket,
    ]);
  }
}

export class OptionalWhiteSpaceRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
    const whitespace: Rule  = new TokenRule(tokenBuffer, TokenTypes.WHITESPACE);
    this.rule = new OptionalRule(tokenBuffer, whitespace);
  }
}
