/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-03-13
 *
 * KMC KMN Next Generation Parser (Recursive Descent/Kleene Operator Rules)
 */

import { TokenType } from "./token-type.js";
import { Token } from "./lexer.js";
import { TokenBuffer } from "./token-buffer.js";
import { NodeType } from "./node-type.js";
import { ASTNode } from "./tree-construction.js";
import { TOKEN_TO_NODE } from "./token-to-node.js";
import { ASTStrategy } from "./ast-strategy.js";

/**
 * Rule is the abstract base class of all the recursive-descent
 * syntax analyzer rules.
 */
export abstract class Rule { // equivalent to a no-child rule
  /**
   * Parse the tokenBuffer, building it into an abstract syntax tree (AST).
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public abstract parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean;
}

/**
 * SingleChildRule is the abstract base class of all the recursive-descent
 * syntax analyzer rules with a single child.
 */
export abstract class SingleChildRule extends Rule {
  /**
   * Construct a SingleChildRule
   */
  public constructor(
    /** the single child rule */
    protected rule: Rule=null
  ) {
    super();
  }

  /**
   * Parse the tokenBuffer, building it into an abstract syntax tree (AST).
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed (null if there is no rule)
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    return this.rule === null ? false : this.rule.parse(tokenBuffer, node);
  }
}

/**
 * SingleChildRule is the abstract base class of all the recursive-descent
 * syntax analyzer rules with a single child that use an ASTStrategy to
 * rebuild the tree after a successful parse.
 */
export abstract class SingleChildRuleWithASTStrategy extends SingleChildRule {
  /**
   * Construct a SingleChildRuleWithASTStrategy
   */
  public constructor(
    /** the strategy used on the tree after a successful parse */
    protected readonly strategy: ASTStrategy,
    /** the single child rule */
    rule: Rule=null
  ) {
    super(rule);
  }

  /**
   * Parse the rule and, if successful, apply the ASTStrategy
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeType.TMP);
    const parseSuccess: boolean = this.rule.parse(tokenBuffer, tmp);
    if (parseSuccess) {
      node.addChild(this.strategy.apply(tmp).getSoleChild());
    }
    return parseSuccess;
  };
}

/**
 * MultiChildRule is the abstract base class of all the recursive-descent
 * syntax analyzer rules with multiple children.
 */
export abstract class MultiChildRule extends Rule {
  /**
   * Construct a MultiChildRule
   */
  public constructor(
    /** the array of child rules */
    protected rules: Rule[]
  ) {
    // TODO-NG-COMPILER error if rules is null
    super();
  }
}

/**
 * SequenceRule represents sequential rules in the recursive-descent
 * syntax analyzer (rules eparated by spaces in the BNF).
 */
export class SequenceRule extends MultiChildRule {
  /**
   * Construct a SequenceRule
   */
  public constructor(
    /** the array of sequential child rules */
    rules: Rule[]
  ) {
    super(rules);
  }

  /**
   * Parse each rule in turn, succeeding only if all succeed.
   * In the event of failure, reset the tokenBuffer to the position
   * saved at the start of the attempted parse.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    const save: number = tokenBuffer.currentPosition;
    const tmp: ASTNode = new ASTNode(NodeType.TMP);

    for (const rule of this.rules) {
      if (!rule.parse(tokenBuffer, tmp)) {
        parseSuccess = false;
        break;
      }
    }
    if (parseSuccess) {
      node.addChildren(tmp.getChildren());
    } else {
      tokenBuffer.resetCurrentPosition(save);
    }

    return parseSuccess;
  }
}

/**
 * AlternateRule represents alternative rules in the recursive-descent
 * syntax analyzer (rules eparated by '|' in the BNF).
 */
export class AlternateRule extends MultiChildRule {
  /**
   * Construct an AlternateRule
   */
  public constructor(
    /** the array of alternate child rules */
    rules: Rule[]
  ) {
    super(rules);
  }

  /**
   * Parse each rule in turn, succeeding if any succeed. In the event of failure,
   * reset the tokenBuffer to the position saved at the start of the attempted parse.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const save: number = tokenBuffer.currentPosition;
    let tmp: ASTNode;

    for (const rule of this.rules) {
      tmp = new ASTNode(NodeType.TMP);
      if (rule.parse(tokenBuffer, tmp)) {
        parseSuccess = true;
        break;
      }
    }
    if (parseSuccess) {
      node.addChildren(tmp.getChildren());
    } else {
      tokenBuffer.resetCurrentPosition(save);
    }

    return parseSuccess;
  }
}

/**
 * OptionalRule represents optional rules in the recursive-descent
 * syntax analyzer ('?' in the BNF).
 */
export class OptionalRule extends SingleChildRule {
  /**
   * Construct an OptionalRule
   */
  public constructor(
    /** the optional single child rule */
    rule: Rule
  ) {
    super(rule);
  }

  /**
   * Parses the stored rule, always succeeding as the rule is optional.
   * In the event of failure of the stored rule, reset the tokenBuffer
   * to the position saved at the start of the attempted parse.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    const save: number = tokenBuffer.currentPosition;
    const tmp: ASTNode = new ASTNode(NodeType.TMP);
    parseSuccess = this.rule.parse(tokenBuffer, tmp);

    if (parseSuccess) {
      node.addChildren(tmp.getChildren());
    } else {
      tokenBuffer.resetCurrentPosition(save);
      // TODO-NG-COMPILER generate warning as parse returns true
    }

    return true;
  }
}

/**
 * ManyRule represents 'many' rules in the recursive-descent
 * syntax analyzer ('*' in the BNF).
 */
export class ManyRule extends SingleChildRule {
  /**
   * Construct a ManyRule
   */
  public constructor(
    /** the single child rule */
    rule: Rule
  ) {
    super(rule);
  }

  /**
   * Parses the stored rule as many times as possible, always succeeding
   * as the rule is optional. In the event of failure of the stored rule,
   * reset the tokenBuffer to the position saved at the start of that
   * individual attempt to parse.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    let parseSuccess: boolean = true;
    while (parseSuccess) {
      const save: number = tokenBuffer.currentPosition;
      const tmp: ASTNode = new ASTNode(NodeType.TMP);
      parseSuccess       = this.rule.parse(tokenBuffer, tmp);
      if (parseSuccess) {
        node.addChildren(tmp.getChildren());
      } else {
        tokenBuffer.resetCurrentPosition(save);
      }
    };
    return true;
  }
}

/**
 * OneOrManyRule represents one-or-many rules in the recursive-descent
 * syntax analyzer ('+' in the BNF).
 */
export class OneOrManyRule extends SingleChildRule {
  /**
   * Construct a OneOrManyRule
   */
  public constructor(
    /** the single child rule */
    rule: Rule
  ) {
    super(rule);
  }

  /**
   * Parses the stored rule as many times as possible, succeeding if the
   * rule suceeds at least once. In the event of failure of the stored rule,
   * reset the tokenBuffer to the position saved at the start of that
   * individual attempt to parse.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    let anyParseSuccess: boolean = false;
    let parseSuccess: boolean    = true;
    do {
      const save: number = tokenBuffer.currentPosition;
      const tmp: ASTNode = new ASTNode(NodeType.TMP);
      parseSuccess       = this.rule.parse(tokenBuffer, tmp);
      if (parseSuccess) {
        node.addChildren(tmp.getChildren());
        anyParseSuccess = true;
      } else {
        tokenBuffer.resetCurrentPosition(save);
      }
    } while (parseSuccess);
    return anyParseSuccess;
  }
}

/**
 * TokenRule represents a Token (terminal) in the BNF.
 */
export class TokenRule extends Rule {
  /** mapping from the TokenType to a NodeType to use in the AST */
  private static tokenToNodeMap: Map<TokenType, NodeType>;

  /**
   * Construct a TokenRule
   */
  public constructor(
    /** the TokenType to potentially match */
    private readonly tokenType: TokenType,
    /** whether to add an ASTNode if the rule succeeds */
    private readonly addNode: boolean=false
  ) {
    super();
  }

  private static tokenToNode = TOKEN_TO_NODE;

  static {
    TokenRule.tokenToNodeMap = new Map<TokenType, NodeType>();
    for (const map of TokenRule.tokenToNode) {
      TokenRule.tokenToNodeMap.set(map.tokenType, map.nodeType);
    }
  }

  /**
   * Parse the current token looking to match the stored TokenType.
   * If matched, the rule succeeds.  Moreover, if adding an ASTNode has
   * been requested (addNode), and there is a valid mapping from the
   * TokenType to a NodeType, then add the ASTNode. If there is no match,
   * reset the tokenBuffer to the position saved at the start of the method.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const token: Token = tokenBuffer.currentToken();

    if (token.isTokenType(this.tokenType)) {
      parseSuccess = true;
      tokenBuffer.popToken();
      if (this.addNode) {
        const nodeType: NodeType = TokenRule.tokenToNodeMap.get(token.tokenType);
        if (nodeType !== undefined) {
          node.addChild(new ASTNode(nodeType, token));
          // TODO-NG-COMPILER: warning if there is no valid mapping
        }
      }
    } else {
      parseSuccess = false;
    }

    return parseSuccess;
  }

  public static getNodeType(tokenType: TokenType): NodeType {
    return TokenRule.tokenToNodeMap.get(tokenType);
  }
}

/**
 * AlternateTokenRule is an optimisation of an AlternateRule where
 * the rules array would consist entirely of TokenRules (all with the
 * same addNode setting). Instead, the TokenType of the current token
 * is directly compared against a supplied array of TokenType looking
 * for the first match.
 */
export class AlternateTokenRule extends Rule {
  /** a Set of TokenType to be potentially matched */
  private readonly tokenTypes: Set<TokenType>;

  /**
   * Construct an AlternateTokenRule
   */
  public constructor(
    /** the array of TokenType to be potentially matched */
    tokenTypes: TokenType[],
    /** whether to add an ASTNode if the rule succeeds */
    private readonly addNode: boolean=false
  ) {
    super();
    // a Set is used to avoid unnecessary duplicates
    this.tokenTypes = new Set<TokenType>(tokenTypes);
    // TODO-NG-COMPILER: warning if there is a duplicate TokenType in the supplied array
  }

  /**
   * Check if the current TokenType is included in the tokenTypes Set.
   * If so, the rule succeeds. Moreover, if adding an ASTNode has
   * been requested (addNode), and there is a valid mapping from the
   * TokenType to a NodeType, then add the ASTNode. If there is no match,
   * reset the tokenBuffer to the position saved at the start of the method.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if the current token was successfully matched
   */
  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    let parseSuccess: boolean = false;
    const token: Token = tokenBuffer.currentToken();

    if (this.tokenTypes.has(token.tokenType)) {
      parseSuccess = true;
      tokenBuffer.popToken();
      if (this.addNode) {
        const nodeType: NodeType = TokenRule.getNodeType(token.tokenType);
        if (nodeType !== undefined) {
          node.addChild(new ASTNode(nodeType, token));
          // TODO-NG-COMPILER: warning if there is no valid mapping
        }
      }
    }

    return parseSuccess;
  }
}

/**
 * Attempt to match a sequence of an expected number of tokens all of the same TokenType,
 * returning the matched tokens in a supplied array.  If there is no match, reset the
 * tokenBuffer to the position saved at the start of the function.
 *
 * @param tokenBuffer the TokenBuffer to parse
 * @param tokens the matched tokens (if any) are pushed onto this array
 * @param tokenType the TokenType to potentially match
 * @param numExpected the number of expected tokens of the same type
 * @returns true if the expected sequence of tokens was successfully matched
 */
export function tokenSequence(tokenBuffer: TokenBuffer, tokens: Token[], tokenType: TokenType, numExpected: number): boolean {
  let parseSuccess: boolean = true;
  const save: number = tokenBuffer.currentPosition;
  let token: Token = tokenBuffer.currentToken();
  const tmpTokens: Token[] = [];

  for (let num=0; num<numExpected; num++) {
    if (token.isTokenType(tokenType)) {
      tokenBuffer.popToken();
      tmpTokens.push(token);
      token = tokenBuffer.currentToken();
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

/**
 * Attempt to match a sequence of an expected number of PARAMETER tokens,
 * returning the matched tokens in a supplied array.  If there is no match, reset the
 * tokenBuffer to the position saved at the start of the function.
 *
 * @param tokenBuffer the TokenBuffer to parse
 * @param parameters the matched PARAMETER tokens (if any) are pushed onto this array
 * @param numExpected the number of expected PARAMETER tokens
 * @returns true if the expected sequence of PARAMETER tokens was successfully matched
 */
export function parameterSequence(tokenBuffer: TokenBuffer, parameters: Token[], numExpected: number): boolean {
  return tokenSequence(tokenBuffer, parameters, TokenType.PARAMETER, numExpected);
}
