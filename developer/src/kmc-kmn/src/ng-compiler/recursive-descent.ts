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

/**
 * Rule is the abstract base class of all the recursive-descent
 * syntax analyser rules
 */
export abstract class Rule { // equivalent to a no-child rule
  protected static _tokenBuffer: TokenBuffer = null;

  public static set tokenBuffer(tokenBuffer: TokenBuffer) {
    // TODO error if tokenBuffer is null
    Rule._tokenBuffer = tokenBuffer;
  }

  public static get tokenBuffer(): TokenBuffer { return Rule._tokenBuffer; }

  /**
   * Parse the tokenBuffer, building it into an abstract syntax tree (AST)
   *
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public abstract parse(node: ASTNode): boolean;
}

/**
 * SingleChildRule is the abstract base class of all the recursive-descent
 * syntax analyser rules with a single child
 */
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

/**
 * MultiChildRule is the abstract base class of all the recursive-descent
 * syntax analyser rules with multiple children
 */
export abstract class MultiChildRule extends Rule {
  protected rules: Rule[];

  public constructor(rules: Rule[]) {
    // TODO error if rules is null
    super();
    this.rules = rules;
  }
}

/**
 * SequenceRule represents sequential rules in the recursive-descent
 * syntax analyser
 */
export class SequenceRule extends MultiChildRule {
  public constructor(rules: Rule[]) {
    super(rules);
  }

  /**
   * Parse each rule in turn, succeeding only if all succeed.
   * In the event of failure, reset the TokenBuffer to the position
   * saved at the start of the attempted parse.
   *
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
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

/**
 * AlternateRule represents alternative rules in the recursive-descent
 * syntax analyser
 */
export class AlternateRule extends MultiChildRule {
  public constructor(rules: Rule[]) {
    super(rules);
  }

  /**
   * Parse each rule in turn, succeeding if any succeed.
   * In the event of failure, reset the TokenBuffer to the position
   * saved at the start of the attempted parse.
   *
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
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

/**
 * OptionalRule represents optional rules in the recursive-descent
 * syntax analyser ('?' in the BNF)
 */
export class OptionalRule extends SingleChildRule {
  public constructor(rule: Rule) {
    super(rule);
  }

  /**
   * Parses the stored rule, always succeeding as the rule is optional.
   * In the event of failure of the stored rule, reset the TokenBuffer
   * to the position saved at the start of the attempted parse.
   *
   * @param node where to build the AST
   * @returns true
   */
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

/**
 * ManyRule represents 'many' rules in the recursive-descent
 * syntax analyser ('*' in the BNF)
 */
export class ManyRule extends SingleChildRule {
  public constructor(rule: Rule) {
    super(rule);
  }

  /**
   * Parses the stored rule as many times as possible, always succeeding
   * as the rule is optional.
   * In the event of failure of the stored rule, reset the TokenBuffer
   * to the position saved at the start of that individual attempt to parse.
   *
   * @param node where to build the AST
   * @returns true
   */
  public parse(node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    while (parseSuccess) {
      const save: number = Rule.tokenBuffer.currentPosition;
      const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
      parseSuccess       = this.rule.parse(tmp);
      if (parseSuccess) {
        node.addChildren(tmp.getChildren());
      } else {
        Rule.tokenBuffer.resetCurrentPosition(save);
      }
    };
    return true;
  }
}

/**
 * OneOrManyRule represents one-or-many rules in the recursive-descent
 * syntax analyser ('+' in the BNF)
 */
export class OneOrManyRule extends SingleChildRule {
  public constructor(rule: Rule) {
    super(rule);
  }

  /**
   * Parses the stored rule as many times as possible, succeeding if the
   * rule suceeds at least once.
   * In the event of failure of the stored rule, reset the TokenBuffer
   * to the position saved at the start of that individual attempt to parse.
   *
   * @param node where to build the AST
   * @returns true
   */
  public parse(node: ASTNode): boolean {
    let anyParseSuccess: boolean = false;
    let parseSuccess: boolean    = true;
    do {
      const save: number = Rule.tokenBuffer.currentPosition;
      const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
      parseSuccess       = this.rule.parse(tmp);
      if (parseSuccess) {
        node.addChildren(tmp.getChildren());
        anyParseSuccess = true;
      } else {
        Rule.tokenBuffer.resetCurrentPosition(save);
      }
    } while (parseSuccess);
    return anyParseSuccess;
  }
}

/**
 * TokenRule represents a Token (terminal) in the BNF
 */
export class TokenRule extends Rule {
  private static tokenToNodeMap: Map<TokenTypes, NodeTypes>;
  private tokenType: TokenTypes;
  private addNode: boolean; // whether to add an ASTNode if the rule succeeds

  /**
   * Construct a TokenRule
   *
   * @param tokenType the TokenType to potentially match
   * @param addNode whether to add an ASTNode if the rule succeeds
   */
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

  /**
   * Parse the next Token looking to match the stored TokenType.
   * If matched, the rule succeeds.  Moreover, if adding an ASTNode has
   * been requested (addNode), and there is a valid mapping from the
   * TokenType to a NodeType, then add the ASTNode. If there is no match,
   * reset the TokenBuffer to the position saved at the start of the method.
   *
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
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
          // TODO: warning if there is no valid mapping
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

/**
 * AlternateTokenRule is an optimisation of an AlternateRule where
 * the rules array would consist entirely of TokenRules (all with the
 * same addNode setting). Instead, the TokenType of the current token
 * is directly compared against a supplied array of TokenTypes looking
 * for the first match.
 */
export class AlternateTokenRule extends Rule {
  private tokenTypes: Set<TokenTypes>; // a Set is used to avoid unnecessary duplicates
  private addNode: boolean;

  /**
   * Construct an AlternateTokenRule
   *
   * @param tokenTypes the array of TokenTypes to be potentially matched
   * @param addNode whether to add an ASTNode if the rule succeeds
   */
  public constructor(tokenTypes: TokenTypes[], addNode: boolean=false) {
    super();
    this.tokenTypes = new Set<TokenTypes>(tokenTypes);
    // TODO: warning if there is a duplicate TokenType in the supplied array
    this.addNode    = addNode;
  }

  /**
   * Check if the current TokenType is included in the tokenTypes Set.
   * If so, the rule succeeds. Moreover, if adding an ASTNode has
   * been requested (addNode), and there is a valid mapping from the
   * TokenType to a NodeType, then add the ASTNode. If there is no match,
   * reset the TokenBuffer to the position saved at the start of the method.
   *
   * @param node where to build the AST
   * @returns true if the current token was successfully matched
   */
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
