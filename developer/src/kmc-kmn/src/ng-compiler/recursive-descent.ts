/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (TokenBuffer)
 */

import { Token, TokenTypes } from "./lexer.js";
import { TokenBuffer } from "./token-buffer.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";

export abstract class Rule {
  protected tokenBuffer: TokenBuffer;
  protected rule: Rule;

  public constructor(tokenBuffer: TokenBuffer, rule: Rule=null) {
    this.tokenBuffer = tokenBuffer;
    this.rule        = rule;
  }

  public parse(node: ASTNode): boolean {
    return this.rule.parse(node);
  }
}

export class SequenceRule extends Rule {
  private rules: Rule[];

  public constructor(tokenBuffer: TokenBuffer, rules: Rule[]) {
    super(tokenBuffer);
    this.rules = rules;
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    const save: number = this.tokenBuffer.currentPosition;
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);

    for (const rule of this.rules) {
      if (!rule.parse(tmp)) {
        parseSuccess = false;
        break;
      }
    }
    if (parseSuccess) {
      node.addChildren(tmp.getChildren());
    } else {
      this.tokenBuffer.resetCurrentPosition(save);
    }

    return parseSuccess;
  }
}

export class OptionalRule extends Rule {
  public constructor(tokenBuffer: TokenBuffer, rule: Rule) {
    super(tokenBuffer, rule);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    const save: number = this.tokenBuffer.currentPosition;
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    parseSuccess = this.rule.parse(tmp);

    if (parseSuccess) {
      node.addChildren(tmp.getChildren());
    } else {
      this.tokenBuffer.resetCurrentPosition(save);
      // TODO generate warning
    }

    return true;
  }
}

export class ManyRule extends Rule {
  public constructor(tokenBuffer: TokenBuffer, rule: Rule) {
    super(tokenBuffer, rule);
  }

  public parse(node: ASTNode): boolean {
    while (this.rule.parse(node));
    return true;
  }
}

export class OneOrManyRule extends Rule {
  public constructor(tokenBuffer: TokenBuffer, rule: Rule) {
    super(tokenBuffer, rule);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    let save: number = this.tokenBuffer.currentPosition;
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);

    if (this.rule.parse(tmp)) {
      parseSuccess = true;
      while (this.rule.parse(tmp));
      node.addChildren(tmp.getChildren());
    } else {
      this.tokenBuffer.resetCurrentPosition(save);
    }

    return parseSuccess;
  }
}

export class KeywordRule extends Rule {
  private tokenType: TokenTypes;

  public constructor(tokenBuffer: TokenBuffer, tokenType: TokenTypes) {
    super(tokenBuffer);
    this.tokenType = tokenType;
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const token: Token = this.tokenBuffer.nextToken();

    if (token.isTokenType(this.tokenType)) {
      parseSuccess = true;
      this.tokenBuffer.popToken();
    } else {
      parseSuccess = false;
    }

    return parseSuccess;
  }
}

export function parameterSequence(tokenBuffer: TokenBuffer, identifiers: Token[], numExpected: number): boolean {
  let parseSuccess: boolean = true;
  const save: number = tokenBuffer.currentPosition;
  let token: Token = tokenBuffer.nextToken();

  for (let num=0; num<numExpected; num++) {
    if (token.isTokenType(TokenTypes.TT_PARAMETER)) {
      tokenBuffer.popToken();
      identifiers.push(token);
      token = tokenBuffer.nextToken();
    } else {
      parseSuccess = false;
      break;
    }
  }

  if (!parseSuccess)
    tokenBuffer.resetCurrentPosition(save);

  return parseSuccess;
}
