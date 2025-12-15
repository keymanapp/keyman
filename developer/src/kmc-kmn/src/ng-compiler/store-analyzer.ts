/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-05-05
 *
 * KMC KMN Next Generation Parser (Recursive Descent/Store Analyser)
 *
 * System and Normal Store Rule Tests
 */

import { TokenType } from "./token-type.js";
import { Token } from "./lexer.js";
import { PermittedKeywordRule, TextRule } from "./kmn-analyzer.js";
import { AlternateRule, AlternateTokenRule, ManyRule, OneOrManyRule, OptionalRule, Rule, SingleChildRuleWithASTStrategy } from "./recursive-descent.js";
import { SingleChildRule, SequenceRule, TokenRule } from "./recursive-descent.js";
import { NodeType } from "./node-type.js";
import { ASTNode } from "./tree-construction.js";
import { TokenBuffer } from "./token-buffer.js";
import { FirstNode, GivenNode, NewNodeOrTree, StackedPair } from "./ast-strategy.js";

/**
 * (BNF) systemStoreAssign: systemStore text*
 */
export class SystemStoreAssignRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new FirstNode());
    const systemStore: Rule = new SystemStoreRule();
    const text: Rule        = new TextRule();
    const manyText: Rule    = new ManyRule(text);
    this.rule = new SequenceRule([systemStore, manyText]);
  }
}

/**
 * (BNF) systemStore: STORE LEFT_BR systemStoreName RIGHT_BR
 */
export class SystemStoreRule extends SingleChildRule {
  public constructor() {
    super();
    const store: Rule           = new TokenRule(TokenType.STORE);
    const leftBracket: Rule     = new TokenRule(TokenType.LEFT_BR);
    const systemStoreName: Rule = new SystemStoreNameRule();
    const rightBracket          = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([
      store,
      leftBracket,
      systemStoreName,
      rightBracket,
    ]);
  }
}

/**
 * (BNF) see the BNF file (kmn-file.bnf)
 */
export class SystemStoreNameRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenType.BITMAP,
      TokenType.CASEDKEYS,
      TokenType.COPYRIGHT,
      TokenType.DISPLAYMAP,
      TokenType.ETHNOLOGUECODE,
      TokenType.HOTKEY,
      TokenType.INCLUDECODES,
      TokenType.KEYBOARDVERSION,
      TokenType.KMW_EMBEDCSS,
      TokenType.KMW_EMBEDJS,
      TokenType.KMW_HELPFILE,
      TokenType.KMW_HELPTEXT,
      TokenType.KMW_RTL,
      TokenType.LANGUAGE,
      TokenType.LAYOUTFILE,
      TokenType.MESSAGE,
      TokenType.MNEMONICLAYOUT,
      TokenType.NAME,
      TokenType.OLDCHARPOSMATCHING,
      TokenType.TARGETS,
      TokenType.VERSION,
      TokenType.VISUALKEYBOARD,
      TokenType.WINDOWSLANGUAGES,
      TokenType.CAPSALWAYSOFF,
      TokenType.CAPSONONLY,
      TokenType.SHIFTFREESCAPS,
    ], true);
  }
}

/**
 * (BNF) normalStoreAssign: normalStore text*
 */
export class NormalStoreAssignRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.STORE));
    const normalStore: Rule = new NormalStoreRule();
    const text: Rule        = new TextRule();
    const manyText: Rule    = new ManyRule(text);
    this.rule = new SequenceRule([normalStore, manyText]);
  }
}

/**
 * (BNF) normalStore: STORE LEFT_BR normalStoreName RIGHT_BR
 */
export class NormalStoreRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new StackedPair(NodeType.STORE, NodeType.STORENAME));
    const store: Rule           = new TokenRule(TokenType.STORE, true);
    const leftBracket: Rule     = new TokenRule(TokenType.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const rightBracket: Rule    = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([store, leftBracket, normalStoreName, rightBracket]);
  }
}

/**
 * (BNF) normalStore: STORE LEFT_BR normalStoreName RIGHT_BR
 */
export class NormalStoreNameRule extends SingleChildRuleWithASTStrategy {
  // TODO-NG-COMPILER: warning/error if normal store name consists of multiple elements
  public constructor() {
    super(new NewNodeOrTree(NodeType.STORENAME));
    const normalStoreNameElement: Rule = new NormalStoreNameElementRule();
    this.rule = new OneOrManyRule(normalStoreNameElement);
  }
}

/**
 * (BNF) normalStoreName: normalStoreNameElement+
 *
 * OCTAL and permitted keywords are included as these could be
 * valid normal store name elements
 */
export class NormalStoreNameElementRule extends SingleChildRule {
  public constructor() {
    super();
    const parameter: Rule        = new TokenRule(TokenType.PARAMETER, true);
    const octal: Rule            = new TokenRule(TokenType.OCTAL, true);
    const permittedKeyword: Rule = new PermittedKeywordRule();
    this.rule = new AlternateRule([parameter, octal, permittedKeyword]);
  }
}

/**
 * (BNF) deadkeyName: deadkeyNameElement+
 */
export class DeadkeyNameRule extends SingleChildRuleWithASTStrategy {
  // TODO-NG-COMPILER: warning/error if deadkey name consists of multiple elements
  public constructor() {
    super(new NewNodeOrTree(NodeType.DEADKEYNAME));
    const deadkeyNameElement: Rule = new DeadkeyNameElementRule();
    this.rule = new OneOrManyRule(deadkeyNameElement);
  }
}

/**
 * (BNF) deadkeyNameElement: PARAMETER|OCTAL|permittedKeyword
 *
 * OCTAL and permitted keywords are included as these could be
 * valid deadkey name elements
 */
export class DeadkeyNameElementRule extends SingleChildRule {
  public constructor() {
    super();
    const parameter: Rule        = new TokenRule(TokenType.PARAMETER, true);
    const octal: Rule            = new TokenRule(TokenType.OCTAL, true);
    const permittedKeyword: Rule = new PermittedKeywordRule();
    this.rule = new AlternateRule([parameter, octal, permittedKeyword]);
  }
}

/**
 * (BNF) storeName: systemStoreName|normalStoreName
 */
export class StoreNameRule extends SingleChildRule {
  public constructor() {
    super();
    const systemStoreName: Rule = new SystemStoreNameRule();
    const normalStoreName: Rule = new NormalStoreNameRule();
    this.rule = new AlternateRule([systemStoreName, normalStoreName]);
  }
}

/**
 * (BNF) setNormalStore: SET LEFT_BR normalStoreName EQUAL text+ RIGHT_BR
 */
export class SetNormalStoreRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.SET));
    const set: Rule             = new TokenRule(TokenType.SET, true);
    const leftBracket: Rule     = new TokenRule(TokenType.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const equal: Rule           = new TokenRule(TokenType.EQUAL);
    const text: Rule            = new TextRule();
    const oneOrManyText: Rule   = new OneOrManyRule(text);
    const rightBracket: Rule    = new TokenRule(TokenType.RIGHT_BR);

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

/**
 * (BNF) setSystemStore: SET LEFT_BR systemStoreNameForSet EQUAL text+ RIGHT_BR
 */
export class SetSystemStoreRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new GivenNode(NodeType.SET));
    const set: Rule                   = new TokenRule(TokenType.SET, true);
    const leftBracket: Rule           = new TokenRule(TokenType.LEFT_BR);
    const systemStoreNameForSet: Rule = new SystemStoreNameForSetRule();
    const equal: Rule                 = new TokenRule(TokenType.EQUAL);
    const text: Rule                  = new TextRule();
    const oneOrManyText: Rule         = new OneOrManyRule(text);
    const rightBracket: Rule          = new TokenRule(TokenType.RIGHT_BR);

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

/**
 * (BNF) systemStoreNameForSet: systemStoreName|LAYER
 */
export class SystemStoreNameForSetRule extends SingleChildRule {
  public constructor() {
    super();
    const layer: Rule           = new TokenRule(TokenType.LAYER, true);
    const systemStoreName: Rule = new SystemStoreNameRule();
    this.rule = new AlternateRule([systemStoreName, layer]);
  }
}

/**
 * (BNF) resetStore: RESET LEFT_BR normalStoreName RIGHT_BR
 */
export class ResetStoreRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new StackedPair(NodeType.RESET, NodeType.STORENAME));
    const reset: Rule           = new TokenRule(TokenType.RESET, true);
    const leftBracket: Rule     = new TokenRule(TokenType.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const rightBracket: Rule    = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([reset, leftBracket, normalStoreName, rightBracket]);
  }
}

abstract class CapsLockStatementRule extends SingleChildRule {
  protected tokenType: TokenType;
  protected nodeType: NodeType;

  public constructor() {
    super();
  }

  public parse(tokenBuffer: TokenBuffer, node: ASTNode): boolean {
    const tmp: ASTNode = new ASTNode(NodeType.TMP);
    const parseSuccess: boolean = this.rule.parse(tokenBuffer, tmp);
    if (parseSuccess) {
      // TODO-NG-COMPILER: warning/error for caps lock statements
      const token: Token = new Token(this.tokenType, '1');
      node.addNewChildWithToken(this.nodeType, token);
    }
    return parseSuccess;
  }
}

/**
 * (BNF) capsAlwaysOff: CAPS ALWAYS OFF
 */
export class CapsAlwaysOffRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType     = TokenType.CAPSALWAYSOFF;
    this.nodeType      = NodeType.CAPSALWAYSOFF;
    const caps: Rule   = new TokenRule(TokenType.CAPS);
    const always: Rule = new TokenRule(TokenType.ALWAYS);
    const off: Rule    = new TokenRule(TokenType.OFF);
    this.rule = new SequenceRule([caps, always, off]);
  }
}

/**
 * (BNF) capsOnOnly: CAPS ON ONLY
 */
export class CapsOnOnlyRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType   = TokenType.CAPSONONLY;
    this.nodeType    = NodeType.CAPSONONLY;
    const caps: Rule = new TokenRule(TokenType.CAPS);
    const on: Rule   = new TokenRule(TokenType.ON);
    const only: Rule = new TokenRule(TokenType.ONLY);
    this.rule = new SequenceRule([caps, on, only]);
  }
}

/**
 * (BNF) shiftFreesCaps: SHIFT FREES CAPS
 */
export class ShiftFreesCapsRule extends CapsLockStatementRule {
  public constructor() {
    super();
    this.tokenType    = TokenType.SHIFTFREESCAPS;
    this.nodeType     = NodeType.SHIFTFREESCAPS;
    const shift: Rule = new TokenRule(TokenType.SHIFT);
    const frees: Rule = new TokenRule(TokenType.FREES);
    const caps: Rule  = new TokenRule(TokenType.CAPS);
    this.rule = new SequenceRule([shift, frees, caps]);
  }
}

/**
 * (BNF) headerAssign: headerName headerValue
 */
export class HeaderAssignRule extends SingleChildRuleWithASTStrategy {
  public constructor() {
    super(new FirstNode());
    const headerName: Rule  = new HeaderNameRule();
    const headerValue: Rule = new HeaderValueRule();
    this.rule = new SequenceRule([headerName, headerValue]);
  }
}

/**
 * (BNF) headerValue: text+|PARAMETER?
 */
export class HeaderValueRule extends SingleChildRule {
  public constructor() {
    super();
    const text: Rule          = new TextRule();
    const oneOrManyText: Rule = new OneOrManyRule(text);
    const parameter: Rule     = new TokenRule(TokenType.PARAMETER, true);
    const optParameter: Rule  = new OptionalRule(parameter);
    this.rule = new AlternateRule([oneOrManyText, optParameter]);
  }
}

/**
 * (BNF) see the BNF file (kmn-file.bnf)
 */
export class HeaderNameRule extends AlternateTokenRule {
  public constructor() {
    super([
      TokenType.BITMAP_HEADER,
      TokenType.COPYRIGHT_HEADER,
      TokenType.HOTKEY_HEADER,
      TokenType.LANGUAGE_HEADER,
      TokenType.LAYOUT_HEADER,
      TokenType.MESSAGE_HEADER,
      TokenType.NAME_HEADER,
      TokenType.VERSION_HEADER,
    ], true);
  }
}
