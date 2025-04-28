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
      let save: number = Rule.tokenBuffer.currentPosition;
      let tmp: ASTNode = new ASTNode(NodeTypes.TMP);
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

  private static tokenToNode = [
    {tokenType: TokenTypes.ANSI,             nodeType: NodeTypes.ANSI},
    {tokenType: TokenTypes.ANY,              nodeType: NodeTypes.ANY},
    {tokenType: TokenTypes.BASELAYOUT,       nodeType: NodeTypes.BASELAYOUT},
    {tokenType: TokenTypes.BEEP,             nodeType: NodeTypes.BEEP},
    {tokenType: TokenTypes.BEGIN,            nodeType: NodeTypes.BEGIN},
    {tokenType: TokenTypes.BITMAP,           nodeType: NodeTypes.BITMAP},
    {tokenType: TokenTypes.CASEDKEYS,        nodeType: NodeTypes.CASEDKEYS},
    {tokenType: TokenTypes.CONTEXT,          nodeType: NodeTypes.CONTEXT},
    {tokenType: TokenTypes.COPYRIGHT,        nodeType: NodeTypes.COPYRIGHT},
    {tokenType: TokenTypes.DISPLAYMAP,       nodeType: NodeTypes.DISPLAYMAP},
    {tokenType: TokenTypes.EQUAL,            nodeType: NodeTypes.EQUAL},
    {tokenType: TokenTypes.ETHNOLOGUECODE,   nodeType: NodeTypes.ETHNOLOGUECODE},
    {tokenType: TokenTypes.GROUP,            nodeType: NodeTypes.GROUP},
    {tokenType: TokenTypes.HOTKEY,           nodeType: NodeTypes.HOTKEY},
    {tokenType: TokenTypes.IF,               nodeType: NodeTypes.IF},
    {tokenType: TokenTypes.INCLUDECODES,     nodeType: NodeTypes.INCLUDECODES},
    {tokenType: TokenTypes.INDEX,            nodeType: NodeTypes.INDEX},
    {tokenType: TokenTypes.KEYBOARDVERSION,  nodeType: NodeTypes.KEYBOARDVERSION},
    {tokenType: TokenTypes.KEY_CODE,         nodeType: NodeTypes.KEY_CODE},
    {tokenType: TokenTypes.KMW_EMBEDCSS,     nodeType: NodeTypes.KMW_EMBEDCSS},
    {tokenType: TokenTypes.KMW_EMBEDJS,      nodeType: NodeTypes.KMW_EMBEDJS},
    {tokenType: TokenTypes.KMW_HELPFILE,     nodeType: NodeTypes.KMW_HELPFILE},
    {tokenType: TokenTypes.KMW_HELPTEXT,     nodeType: NodeTypes.KMW_HELPTEXT},
    {tokenType: TokenTypes.KMW_RTL,          nodeType: NodeTypes.KMW_RTL},
    {tokenType: TokenTypes.LANGUAGE,         nodeType: NodeTypes.LANGUAGE},
    {tokenType: TokenTypes.LAYER,            nodeType: NodeTypes.LAYER},
    {tokenType: TokenTypes.LAYOUTFILE,       nodeType: NodeTypes.LAYOUTFILE},
    {tokenType: TokenTypes.MATCH,            nodeType: NodeTypes.MATCH},
    {tokenType: TokenTypes.MESSAGE,          nodeType: NodeTypes.MESSAGE},
    {tokenType: TokenTypes.MNEMONICLAYOUT,   nodeType: NodeTypes.MNEMONICLAYOUT},
    {tokenType: TokenTypes.NAME,             nodeType: NodeTypes.NAME},
    {tokenType: TokenTypes.NEWCONTEXT,       nodeType: NodeTypes.NEWCONTEXT},
    {tokenType: TokenTypes.NEWLAYER,         nodeType: NodeTypes.NEWLAYER},
    {tokenType: TokenTypes.NOMATCH,          nodeType: NodeTypes.NOMATCH},
    {tokenType: TokenTypes.NOT_EQUAL,        nodeType: NodeTypes.NOT_EQUAL},
    {tokenType: TokenTypes.NEWLINE,          nodeType: NodeTypes.LINE},
    {tokenType: TokenTypes.OLDLAYER,         nodeType: NodeTypes.OLDLAYER},
    {tokenType: TokenTypes.OUTS,             nodeType: NodeTypes.OUTS},
    {tokenType: TokenTypes.PLATFORM,         nodeType: NodeTypes.PLATFORM},
    {tokenType: TokenTypes.POSTKEYSTROKE,    nodeType: NodeTypes.POSTKEYSTROKE},
    {tokenType: TokenTypes.READONLY,         nodeType: NodeTypes.READONLY},
    {tokenType: TokenTypes.SHIFT_CODE,       nodeType: NodeTypes.SHIFT_CODE},
    {tokenType: TokenTypes.STORE,            nodeType: NodeTypes.STORE},
    {tokenType: TokenTypes.STRING,           nodeType: NodeTypes.STRING},
    {tokenType: TokenTypes.TARGETS,          nodeType: NodeTypes.TARGETS},
    {tokenType: TokenTypes.U_CHAR,           nodeType: NodeTypes.U_CHAR},
    {tokenType: TokenTypes.UNICODE,          nodeType: NodeTypes.UNICODE},
    {tokenType: TokenTypes.USE,              nodeType: NodeTypes.USE},
    {tokenType: TokenTypes.VERSION,          nodeType: NodeTypes.VERSION},
    {tokenType: TokenTypes.VISUALKEYBOARD,   nodeType: NodeTypes.VISUALKEYBOARD},
    {tokenType: TokenTypes.WINDOWSLANGUAGES, nodeType: NodeTypes.WINDOWSLANGUAGES},
  ];

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
