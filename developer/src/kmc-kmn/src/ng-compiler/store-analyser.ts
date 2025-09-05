/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-05-05
 *
 * KMC KMN Next Generation Parser (Recursive Descent/Store Analyser)
 *
 * System and Normal Store Rule Tests
 */

import { TokenTypes } from "./token-types.js";
import { Token } from "./lexer.js";
import { PermittedKeywordRule, TextRule } from "./kmn-analyser.js";
import { AlternateRule, AlternateTokenRule, ManyRule, OptionalRule, Rule, SingleChildRuleParseToTreeOfFirstNode, SingleChildRuleParseToTreeOfGivenNode } from "./recursive-descent.js";
import { SingleChildRule, SequenceRule, TokenRule } from "./recursive-descent.js";import { OneOrManyRule  } from "./recursive-descent.js";
import { NodeTypes } from "./node-types.js";
import { ASTNode } from "./tree-construction.js";

export class SystemStoreAssignRule extends SingleChildRuleParseToTreeOfFirstNode {
  public constructor() {
    super();
    const systemStore: Rule = new SystemStoreRule();
    const text: Rule        = new TextRule();
    const manyText: Rule    = new ManyRule(text);
    this.rule = new SequenceRule([systemStore, manyText]);
  }
}

export class SystemStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const store: Rule           = new TokenRule(TokenTypes.STORE);
    const leftBracket: Rule     = new TokenRule(TokenTypes.LEFT_BR);
    const systemStoreName: Rule = new SystemStoreNameRule();
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
      TokenTypes.OLDCHARPOSMATCHING,
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

export class NormalStoreAssignRule extends SingleChildRuleParseToTreeOfGivenNode {
  public constructor() {
    super(NodeTypes.STORE);
    const normalStore: Rule = new NormalStoreRule();
    const text: Rule        = new TextRule();
    const manyText: Rule    = new ManyRule(text);
    this.rule = new SequenceRule([normalStore, manyText]);
  }
}

export class NormalStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const store: Rule           = new TokenRule(TokenTypes.STORE, true);
    const leftBracket: Rule     = new TokenRule(TokenTypes.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const rightBracket: Rule    = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([store, leftBracket, normalStoreName, rightBracket]);
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

export class NormalStoreNameRule extends SingleChildRule {
  public constructor() {
    super();
    const normalStoreNameElement: Rule = new NormalStoreNameElementRule();
    this.rule = new OneOrManyRule(normalStoreNameElement);
  }

  public parse(node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeTypes.TMP);
    const parseSuccess: boolean = this.rule.parse(tmp);
    if (parseSuccess) {
      const children = tmp.getChildren();
      // two structures depending on number of children
      if (children.length === 1) {
        node.addNewChildWithToken(NodeTypes.STORENAME, children[0].token);
      } else {
        const normalStoreNameNode = new ASTNode(NodeTypes.STORENAME);
        normalStoreNameNode.addChildren(children);
        node.addChild(normalStoreNameNode);
      }
    }
    return parseSuccess;
  }
}

export class NormalStoreNameElementRule extends SingleChildRule {
  public constructor() {
    super();
    const parameter: Rule        = new TokenRule(TokenTypes.PARAMETER, true);
    const octal: Rule            = new TokenRule(TokenTypes.OCTAL, true);
    const permittedKeyword: Rule = new PermittedKeywordRule();
    this.rule = new AlternateRule([parameter, octal, permittedKeyword]);
  }
}

export class StoreNameRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStoreName: Rule = new SystemStoreNameRule();
    const normalStoreName: Rule = new NormalStoreNameRule();
    this.rule = new AlternateRule([systemStoreName, normalStoreName]);
  }
}

export class SetNormalStoreRule extends SingleChildRuleParseToTreeOfGivenNode {
  public constructor() {
    super(NodeTypes.SET);
    const set: Rule             = new TokenRule(TokenTypes.SET, true);
    const leftBracket: Rule     = new TokenRule(TokenTypes.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const equal: Rule           = new TokenRule(TokenTypes.EQUAL);
    const text: Rule            = new TextRule();
    const oneOrManyText: Rule   = new OneOrManyRule(text);
    const rightBracket: Rule    = new TokenRule(TokenTypes.RIGHT_BR);

    this.rule = new SequenceRule([
      set,
      leftBracket,
      normalStoreName,
      equal,
      oneOrManyText,
      rightBracket,
    ]);
  }
}

export class SetSystemStoreRule extends SingleChildRuleParseToTreeOfGivenNode {
  public constructor() {
    super(NodeTypes.SET);
    const set: Rule                   = new TokenRule(TokenTypes.SET, true);
    const leftBracket: Rule           = new TokenRule(TokenTypes.LEFT_BR);
    const systemStoreNameForSet: Rule = new SystemStoreNameForSetRule();
    const equal: Rule                 = new TokenRule(TokenTypes.EQUAL);
    const text: Rule                  = new TextRule();
    const oneOrManyText: Rule         = new OneOrManyRule(text);
    const rightBracket: Rule          = new TokenRule(TokenTypes.RIGHT_BR);

    this.rule = new SequenceRule([
      set,
      leftBracket,
      systemStoreNameForSet,
      equal,
      oneOrManyText,
      rightBracket,
    ]);
  }
}

export class SystemStoreNameForSetRule extends SingleChildRule {
  public constructor() {
    super();
    const layer: Rule           = new TokenRule(TokenTypes.LAYER, true);
    const systemStoreName: Rule = new SystemStoreNameRule();
    this.rule = new AlternateRule([systemStoreName, layer]);
  }
}

export class ResetStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const reset: Rule           = new TokenRule(TokenTypes.RESET, true);
    const leftBracket: Rule     = new TokenRule(TokenTypes.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const rightBracket: Rule    = new TokenRule(TokenTypes.RIGHT_BR);
    this.rule = new SequenceRule([reset, leftBracket, normalStoreName, rightBracket]);
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
      node.addNewChildWithToken(this.nodeType, token);
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

export class HeaderAssignRule extends SingleChildRuleParseToTreeOfFirstNode {
  public constructor() {
    super();
    const headerName: Rule  = new HeaderNameRule();
    const headerValue: Rule = new HeaderValueRule();
    this.rule = new SequenceRule([headerName, headerValue]);
  }
}

export class HeaderValueRule extends SingleChildRule {
  public constructor() {
    super();
    const text: Rule          = new TextRule();
    const oneOrManyText: Rule = new OneOrManyRule(text);
    const parameter: Rule     = new TokenRule(TokenTypes.PARAMETER, true);
    const optParameter: Rule  = new OptionalRule(parameter);
    this.rule = new AlternateRule([oneOrManyText, optParameter]);
  }
}

export class HeaderNameRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenTypes.BITMAP_HEADER,
      TokenTypes.COPYRIGHT_HEADER,
      TokenTypes.HOTKEY_HEADER,
      TokenTypes.LANGUAGE_HEADER,
      TokenTypes.LAYOUT_HEADER,
      TokenTypes.MESSAGE_HEADER,
      TokenTypes.NAME_HEADER,
      TokenTypes.VERSION_HEADER,
    ], true);
  }
}
