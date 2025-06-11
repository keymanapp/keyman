/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-05-05
 *
 * KMC KMN Next Generation Parser (Recursive Descent/KMN Analyser)
 * 
 * System and Variable Store Rules
 */

import { TextRule } from "./kmn-analyser.js";
import { Token, TokenTypes } from "./lexer.js";
import { SingleChildRule, Rule, TokenRule, SequenceRule, AlternateTokenRule, AlternateRule } from "./recursive-descent.js";
import { OneOrManyRule  } from "./recursive-descent.js";
import { ASTNode, NodeTypes } from "./tree-construction.js";


export class SystemStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStore: Rule   = new SystemStoreRule();
    const text: Rule          = new TextRule();
    const oneOrManyText: Rule = new OneOrManyRule(text);
    this.rule = new SequenceRule([systemStore, oneOrManyText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const children: ASTNode[]  = tmp.getChildren();
      const storeNode: ASTNode   = children.splice(0, 1)[0];
      storeNode.addChildren(children);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

export class SystemStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStoreName: Rule = new SystemStoreNameRule();
    const store                 = new TokenRule(TokenTypes.STORE);
    const leftBracket           = new TokenRule(TokenTypes.LEFT_BR);
    const rightBracket          = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([
      store,
      leftBracket,
      systemStoreName,
      rightBracket,
    ]);
  }
}

export class SystemStoreNameRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenTypes.BITMAP,
      TokenTypes.CASEDKEYS,
      TokenTypes.COPYRIGHT,
      TokenTypes.DISPLAYMAP,
      TokenTypes.ETHNOLOGUECODE,
      TokenTypes.HOTKEY,
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
      TokenTypes.CAPSALWAYSOFF,
      TokenTypes.CAPSONONLY,
      TokenTypes.SHIFTFREESCAPS,
    ], true);
  }
}

export class NormalStoreAssignRule extends SingleChildRule {
  public constructor() {
    super();
    const normalStore: Rule   = new NormalStoreRule();
    const text: Rule          = new TextRule();
    const oneOrManyText: Rule = new OneOrManyRule(text);
    this.rule = new SequenceRule([normalStore, oneOrManyText]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.STORE);
      storeNode.addChildren(tmp.getChildren());
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

export class NormalStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const store: Rule              = new TokenRule(TokenTypes.STORE, true);
    const bracketedStoreName: Rule = new BracketedStoreNameRule();
    this.rule = new SequenceRule([store, bracketedStoreName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const storeNode     = tmp.getSoleChildOfType(NodeTypes.STORE);
      const storeNameNode = tmp.getSoleChildOfType(NodeTypes.STORENAME);
      storeNode.addChild(storeNameNode);
      node.addChild(storeNode);
    }
    return parseSuccess;
  }
}

export class BracketedStoreNameRule extends SingleChildRule {
  public constructor() {
    super();
    const leftBracket: Rule     = new TokenRule(TokenTypes.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const rightBracket: Rule    = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([leftBracket, normalStoreName, rightBracket]);
  }
}

export class NormalStoreNameRule extends SingleChildRule {
    public constructor() {
      super();
      const parameter: Rule = new TokenRule(TokenTypes.PARAMETER, true);
      const octal: Rule     = new TokenRule(TokenTypes.OCTAL, true);
      this.rule = new AlternateRule([parameter, octal]);
    }

    public parse(node: ASTNode): boolean {
      const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
      const parseSuccess: boolean = this.rule.parse(tmp);
      if (parseSuccess) {
        const child = tmp.getSoleChild();
        node.addToken(NodeTypes.STORENAME, child.token);
      }
      return parseSuccess;
    };
}

export class SetStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const set: Rule             = new TokenRule(TokenTypes.SET, true);
    const leftBracket: Rule     = new TokenRule(TokenTypes.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const equal: Rule           = new TokenRule(TokenTypes.EQUAL);
    const stringRule: Rule      = new TokenRule(TokenTypes.STRING, true);
    const rightBracket: Rule    = new TokenRule(TokenTypes.RIGHT_BR);

    this.rule = new SequenceRule([
      set,
      leftBracket,
      normalStoreName,
      equal,
      stringRule,
      rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const setNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.SET);
      setNode.addChildren(tmp.getChildren());
      node.addChild(setNode);
    }
    return parseSuccess;
  }
}

export class SetLayerRule extends SingleChildRule {
  public constructor() {
    super();
    const set: Rule           = new TokenRule(TokenTypes.SET, true);
    const leftBracket: Rule   = new TokenRule(TokenTypes.LEFT_BR);
    const layer: Rule         = new TokenRule(TokenTypes.LAYER, true);
    const equal: Rule         = new TokenRule(TokenTypes.EQUAL);
    const stringRule: Rule    = new TokenRule(TokenTypes.STRING, true);
    const rightBracket: Rule  = new TokenRule(TokenTypes.RIGHT_BR);

    this.rule = new SequenceRule([
      set,
      leftBracket,
      layer,
      equal,
      stringRule,
      rightBracket,
    ]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const setNode: ASTNode = tmp.removeSoleChildOfType(NodeTypes.SET);
      setNode.addChildren(tmp.getChildren());
      node.addChild(setNode);
    }
    return parseSuccess;
  }
}

export class ResetStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const reset: Rule = new TokenRule(TokenTypes.RESET, true);
    const bracketedStoreName: Rule = new BracketedStoreNameRule();
    this.rule = new SequenceRule([reset, bracketedStoreName]);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const resetNode = tmp.getSoleChildOfType(NodeTypes.RESET);
      const storeNameNode = tmp.getSoleChildOfType(NodeTypes.STORENAME);
      resetNode.addChild(storeNameNode);
      node.addChild(resetNode);
    }
    return parseSuccess;
  }
}

abstract class CapsLockStatementRule extends SingleChildRule {
  protected tokenType: TokenTypes;
  protected nodeType: NodeTypes;

  public constructor() {
    super();
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      // TODO: warning deprecated
      const token: Token = new Token(this.tokenType, '1');
      node.addToken(this.nodeType, token);
    }
    return parseSuccess;
  }
}

export class CapsAlwaysOffRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType     = TokenTypes.CAPSALWAYSOFF;
    this.nodeType      = NodeTypes.CAPSALWAYSOFF;
    const caps: Rule   = new TokenRule(TokenTypes.CAPS);
    const always: Rule = new TokenRule(TokenTypes.ALWAYS);
    const off: Rule    = new TokenRule(TokenTypes.OFF);
    this.rule = new SequenceRule([caps, always, off]);
  }
}

export class CapsOnOnlyRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType   = TokenTypes.CAPSONONLY;
    this.nodeType    = NodeTypes.CAPSONONLY;
    const caps: Rule = new TokenRule(TokenTypes.CAPS);
    const on: Rule   = new TokenRule(TokenTypes.ON);
    const only: Rule = new TokenRule(TokenTypes.ONLY);
    this.rule = new SequenceRule([caps, on, only]);
  }
}

export class ShiftFreesCapsRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType    = TokenTypes.SHIFTFREESCAPS;
    this.nodeType     = NodeTypes.SHIFTFREESCAPS;
    const shift: Rule = new TokenRule(TokenTypes.SHIFT);
    const frees: Rule = new TokenRule(TokenTypes.FREES);
    const caps: Rule  = new TokenRule(TokenTypes.CAPS);
    this.rule = new SequenceRule([shift, frees, caps]);
  }
}
