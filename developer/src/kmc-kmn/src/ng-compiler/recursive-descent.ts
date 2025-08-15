/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Recursive Descent/Kleene Operator Rules)
 */

import { TokenTypes } from "./token-types.js";
import { Token } from "./lexer.js";
import { TokenBuffer } from "./token-buffer.js";
import { NodeTypes } from "./node-types.js";
import { ASTNode } from "./tree-construction.js";
import { TOKEN_TO_NODE } from "./token-to-node.js";

export abstract class Rule { // equivalent to a no-child rule
  protected static _tokenBuffer: TokenBuffer = null;

  public static set tokenBuffer(tokenBuffer: TokenBuffer) {
    // TODO error if tokenBuffer is null
    Rule._tokenBuffer = tokenBuffer;
  }

  public static get tokenBuffer(): TokenBuffer { return Rule._tokenBuffer; }

  public abstract parse(node: ASTNode): boolean;
}

export abstract class SingleChildRule extends Rule {
  protected rule: Rule;

  public constructor(rule: Rule=null) {
    super();
    this.rule = rule;
  }

  public parse(node: ASTNode): boolean {
    return this.rule === null ? false : this.rule.parse(node);
  }
}

export abstract class MultiChildRule extends Rule {
  protected rules: Rule[];

  public constructor(rules: Rule[]) {
    // TODO error if rules is null
    super();
    this.rules = rules;
  }
}

export class SequenceRule extends MultiChildRule {
  public constructor(rules: Rule[]) {
    super(rules);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    const save: number = Rule.tokenBuffer.currentPosition;
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
      Rule.tokenBuffer.resetCurrentPosition(save);
    }

    return parseSuccess;
  }
}

export class AlternateRule extends MultiChildRule {
  public constructor(rules: Rule[]) {
    super(rules);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const save: number = Rule.tokenBuffer.currentPosition;
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
      Rule.tokenBuffer.resetCurrentPosition(save);
    }

    return parseSuccess;
  }
}

export class OptionalRule extends SingleChildRule {
  public constructor(rule: Rule) {
    super(rule);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    const save: number = Rule.tokenBuffer.currentPosition;
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    parseSuccess = this.rule.parse(tmp);

    if (parseSuccess) {
      node.addChildren(tmp.getChildren());
    } else {
      Rule.tokenBuffer.resetCurrentPosition(save);
      // TODO generate warning
    }

    return true;
  }
}

export class ManyRule extends SingleChildRule {
  public constructor(rule: Rule) {
    super(rule);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    while (parseSuccess) {
      const save: number = Rule.tokenBuffer.currentPosition;
      const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
      parseSuccess     = this.rule.parse(tmp);
      if (parseSuccess) {
        node.addChildren(tmp.getChildren());
      } else {
        Rule.tokenBuffer.resetCurrentPosition(save);
      }
    };
    return true;
  }
}

export class OneOrManyRule extends SingleChildRule {
  public constructor(rule: Rule) {
    super(rule);
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    let save: number = Rule.tokenBuffer.currentPosition;
    let tmp: ASTNode = new ASTNode(NodeTypes.TMP);

    parseSuccess = this.rule.parse(tmp);
    if (parseSuccess) {
      node.addChildren(tmp.getChildren());
    } else {
      Rule.tokenBuffer.resetCurrentPosition(save);
      return false;
    }

    while (parseSuccess) {
      save         = Rule.tokenBuffer.currentPosition;
      tmp          = new ASTNode(NodeTypes.TMP);
      parseSuccess = this.rule.parse(tmp);
      if (parseSuccess) {
        node.addChildren(tmp.getChildren());
      } else {
        Rule.tokenBuffer.resetCurrentPosition(save);
      }
    }
    return true;
  }
}

export class TokenRule extends Rule {
  private static tokenToNodeMap: Map<TokenTypes, NodeTypes>;
  private tokenType: TokenTypes;
  private addNode: boolean;

  public constructor(tokenType: TokenTypes, addNode: boolean=false) {
    super();
    this.tokenType = tokenType;
    this.addNode   = addNode;
  }

  private static tokenToNode = TOKEN_TO_NODE;

  static {
    TokenRule.tokenToNodeMap = new Map<TokenTypes, NodeTypes>();
    for (const map of TokenRule.tokenToNode) {
      TokenRule.tokenToNodeMap.set(map.tokenType, map.nodeType);
    }
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const token: Token = Rule.tokenBuffer.nextToken();

    if (token.isTokenType(this.tokenType)) {
      parseSuccess = true;
      Rule.tokenBuffer.popToken();
      if (this.addNode) {
        const nodeType: NodeTypes = TokenRule.tokenToNodeMap.get(token.tokenType);
        if (nodeType !== undefined) {
          node.addChild(new ASTNode(nodeType, token));
        }
      }
    } else {
      parseSuccess = false;
    }

    return parseSuccess;
  }

  public static getNodeType(tokenType: TokenTypes): NodeTypes {
    return TokenRule.tokenToNodeMap.get(tokenType);
  }
}

export class AlternateTokenRule extends Rule {
  private tokenTypes: Set<TokenTypes>;
  private addNode: boolean;

  public constructor(tokenTypes: TokenTypes[], addNode: boolean=false) {
    super();
    this.tokenTypes = new Set<TokenTypes>(tokenTypes);
    this.addNode    = addNode;
  }

  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const token: Token = Rule.tokenBuffer.nextToken();

    if (this.tokenTypes.has(token.tokenType)) {
      parseSuccess = true;
      Rule.tokenBuffer.popToken();
      if (this.addNode) {
        const nodeType: NodeTypes = TokenRule.getNodeType(token.tokenType);
        if (nodeType !== undefined) {
          node.addChild(new ASTNode(nodeType, token));
        }
      }
    }

    return parseSuccess;
  }
}

export function parameterSequence(parameters: Token[], numExpected: number): boolean {
  return tokenSequence(parameters, TokenTypes.PARAMETER, numExpected);
}

export function tokenSequence(tokens: Token[], tokenType: TokenTypes, numExpected: number): boolean {
  let parseSuccess: boolean = true;
  const save: number = Rule.tokenBuffer.currentPosition;
  let token: Token = Rule.tokenBuffer.nextToken();
  const tmpTokens: Token[] = [];

  for (let num=0; num<numExpected; num++) {
    if (token.isTokenType(tokenType)) {
      Rule.tokenBuffer.popToken();
      tmpTokens.push(token);
      token = Rule.tokenBuffer.nextToken();
    } else {
      parseSuccess = false;
      break;
    }
  }

  if (parseSuccess) {
    tokens.push(...tmpTokens);
  } else {
    Rule.tokenBuffer.resetCurrentPosition(save);
  }

  return parseSuccess;
}
