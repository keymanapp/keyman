/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Recursive Descent/Kleene Operator Rules)
 */

import { Token, TokenTypes } from "./lexer.js";
import { TokenBuffer } from "./token-buffer.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";

export abstract class Rule { // equivalent to a no-child rule
  protected tokenBuffer: TokenBuffer;

  public constructor(tokenBuffer: TokenBuffer) {
    // TODO error if tokenBuffer is null
    this.tokenBuffer = tokenBuffer;
  }

  public abstract parse(node: ASTNode): boolean;
}

export abstract class SingleChildRule extends Rule {
  protected rule: Rule;

  public constructor(tokenBuffer: TokenBuffer, rule: Rule=null) {
    super(tokenBuffer);
    this.rule = rule;
  }

  public parse(node: ASTNode): boolean {
    return this.rule === null ? false : this.rule.parse(node);
  }
}

export abstract class MultiChildRule extends Rule {
  protected rules: Rule[];

  public constructor(tokenBuffer: TokenBuffer, rules: Rule[]) {
    // TODO error if rules is null
    super(tokenBuffer);
    this.rules = rules;
  }
}

export class SequenceRule extends MultiChildRule {
  public constructor(tokenBuffer: TokenBuffer, rules: Rule[]) {
    super(tokenBuffer, rules);
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

export class AlternateRule extends MultiChildRule {
  public constructor(tokenBuffer: TokenBuffer, rules: Rule[]) {
    super(tokenBuffer, rules);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const save: number = this.tokenBuffer.currentPosition;
    let tmp: ASTNode;

    for (const rule of this.rules) {
      tmp = new ASTNode(NodeTypes.TMP);
      if (rule.parse(tmp)) {
        parseSuccess = true;
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

export class OptionalRule extends SingleChildRule {
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

export class ManyRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer, rule: Rule) {
    super(tokenBuffer, rule);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    while (parseSuccess) {
      let save: number = this.tokenBuffer.currentPosition;
      let tmp: ASTNode = new ASTNode(NodeTypes.TMP);
      parseSuccess     = this.rule.parse(tmp);
      if (parseSuccess) {
        node.addChildren(tmp.getChildren());
      } else {
        this.tokenBuffer.resetCurrentPosition(save);
      }
    };
    return true;
  }
}

export class OneOrManyRule extends SingleChildRule {
  public constructor(tokenBuffer: TokenBuffer, rule: Rule) {
    super(tokenBuffer, rule);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    let save: number = this.tokenBuffer.currentPosition;
    let tmp: ASTNode = new ASTNode(NodeTypes.TMP);

    parseSuccess = this.rule.parse(tmp);
    if (parseSuccess) {
      node.addChildren(tmp.getChildren());
    } else {
      this.tokenBuffer.resetCurrentPosition(save);
      return false;
    }

    while (parseSuccess) {
      save         = this.tokenBuffer.currentPosition;
      tmp          = new ASTNode(NodeTypes.TMP);
      parseSuccess = this.rule.parse(tmp);
      if (parseSuccess) {
        node.addChildren(tmp.getChildren());
      } else {
        this.tokenBuffer.resetCurrentPosition(save);
      }
    }
    return true;
  }
}

export class KeywordRule extends Rule {
  private tokenType: TokenTypes;
  private nodeType: NodeTypes;

  public constructor(tokenBuffer: TokenBuffer, tokenType: TokenTypes, nodeType: NodeTypes=null) {
    super(tokenBuffer);
    this.tokenType = tokenType;
    this.nodeType  = nodeType;
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const token: Token = this.tokenBuffer.nextToken();

    if (token.isTokenType(this.tokenType)) {
      parseSuccess = true;
      this.tokenBuffer.popToken();
      if (this.nodeType !== null) {
        node.addChild(new ASTNode(this.nodeType, token));
      }
    } else {
      parseSuccess = false;
    }

    return parseSuccess;
  }
}

export function parameterSequence(tokenBuffer: TokenBuffer, parameters: Token[], numExpected: number): boolean {
  return tokenSequence(tokenBuffer, parameters, TokenTypes.PARAMETER, numExpected);
}

export function tokenSequence(tokenBuffer: TokenBuffer, tokens: Token[], tokenType: TokenTypes, numExpected: number): boolean {
  let parseSuccess: boolean = true;
  const save: number = tokenBuffer.currentPosition;
  let token: Token = tokenBuffer.nextToken();
  const tmpTokens: Token[] = [];

  for (let num=0; num<numExpected; num++) {
    if (token.isTokenType(tokenType)) {
      tokenBuffer.popToken();
      tmpTokens.push(token);
      token = tokenBuffer.nextToken();
    } else {
      parseSuccess = false;
      break;
    }
  }

  if (parseSuccess) {
    tokens.push(...tmpTokens);
  } else {
    tokenBuffer.resetCurrentPosition(save);
  }

  return parseSuccess;
}
