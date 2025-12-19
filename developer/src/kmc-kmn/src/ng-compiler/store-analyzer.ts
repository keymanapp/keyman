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
import { AlternateRule, AlternateTokenRule, ManyRule, OneOrManyRule, OptionalRule, Rule, SingleChildRuleWithASTRebuild } from "./recursive-descent.js";
import { SingleChildRule, SequenceRule, TokenRule } from "./recursive-descent.js";
import { NodeType } from "./node-type.js";
import { ASTNode } from "./tree-construction.js";
import { TokenBuffer } from "./token-buffer.js";
import { FirstNode, GivenNode, NewNodeOrTree, StackedPair } from "./ast-rebuild.js";

/**
 * (BNF) systemStoreAssign: systemStore text*
 *
 * https://help.keyman.com/developer/language/reference/store
 *
 * Uses a FirstNode to rebuild the tree to be rooted at the first node found
 */
export class SystemStoreAssignRule extends SingleChildRuleWithASTRebuild {
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
 *
 * https://help.keyman.com/developer/language/reference/store
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
  // TODO-NG-COMPILER: warning/error for OLDCHARPOSMATCHING
  public constructor() {
    super([
      TokenType.BITMAP,             // https://help.keyman.com/developer/language/reference/bitmap
      TokenType.CASEDKEYS,          // https://help.keyman.com/developer/language/reference/casedkeys
      TokenType.COPYRIGHT,          // https://help.keyman.com/developer/language/reference/copyright
      TokenType.DISPLAYMAP,         // https://help.keyman.com/developer/language/reference/displaymap
      TokenType.ETHNOLOGUECODE,     // https://help.keyman.com/developer/language/reference/ethnologuecode
      TokenType.HOTKEY,             // https://help.keyman.com/developer/language/reference/hotkey
      TokenType.INCLUDECODES,       // https://help.keyman.com/developer/language/reference/includecodes
      TokenType.KEYBOARDVERSION,    // https://help.keyman.com/developer/language/reference/keyboardversion
      TokenType.KMW_EMBEDCSS,       // https://help.keyman.com/developer/language/reference/kmw_embedcss
      TokenType.KMW_EMBEDJS,        // https://help.keyman.com/developer/language/reference/kmw_embedjs
      TokenType.KMW_HELPFILE,       // https://help.keyman.com/developer/language/reference/kmw_helpfile
      TokenType.KMW_HELPTEXT,       // https://help.keyman.com/developer/language/reference/kmw_helptext
      TokenType.KMW_RTL,            // https://help.keyman.com/developer/language/reference/kmw_rtl
      TokenType.LANGUAGE,           // https://help.keyman.com/developer/language/reference/language
      TokenType.LAYOUTFILE,         // https://help.keyman.com/developer/language/reference/layoutfile
      TokenType.MESSAGE,            // https://help.keyman.com/developer/language/reference/message
      TokenType.MNEMONICLAYOUT,     // https://help.keyman.com/developer/language/reference/mnemoniclayout
      TokenType.NAME,               // https://help.keyman.com/developer/language/reference/name
      TokenType.OLDCHARPOSMATCHING, // https://help.keyman.com/developer/language/reference/oldcharposmatching
      TokenType.TARGETS,            // https://help.keyman.com/developer/language/reference/targets
      TokenType.VERSION,            // https://help.keyman.com/developer/language/reference/version
      TokenType.VISUALKEYBOARD,     // https://help.keyman.com/developer/language/reference/visualkeyboard
      TokenType.WINDOWSLANGUAGES,   // https://help.keyman.com/developer/language/reference/windowslanguages
      TokenType.CAPSALWAYSOFF,      // https://help.keyman.com/developer/language/reference/caps
      TokenType.CAPSONONLY,         // https://help.keyman.com/developer/language/reference/caps
      TokenType.SHIFTFREESCAPS,     // https://help.keyman.com/developer/language/reference/caps
    ], true);
  }
}

/**
 * (BNF) normalStoreAssign: normalStore text*
 *
 * https://help.keyman.com/developer/language/reference/store
 *
 * Uses a GivenNode to rebuild the tree to be rooted at the STORE node
 */
export class NormalStoreAssignRule extends SingleChildRuleWithASTRebuild {
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
 *
 *  https://help.keyman.com/developer/language/reference/store
 *
 * Uses a StackedPair to rebuild the tree as STORE parent and STORENAME child nodes
 */
export class NormalStoreRule extends SingleChildRuleWithASTRebuild {
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
 * (BNF) normalStoreName: normalStoreNameElement+
 *
 * Uses a NewNodeOrTree to add either a single new STORENAME
 * node or build a tree rooted at a STORENAME node, depending
 * on the number of children found (one or more)
 */
export class NormalStoreNameRule extends SingleChildRuleWithASTRebuild {
  // TODO-NG-COMPILER: warning/error if normal store name consists of multiple elements
  public constructor() {
    super(new NewNodeOrTree(NodeType.STORENAME));
    const normalStoreNameElement: Rule = new NormalStoreNameElementRule();
    this.rule = new OneOrManyRule(normalStoreNameElement);
  }
}

/**
 * (BNF) normalStoreNameElement: PARAMETER|OCTAL|permittedKeyword
 *
 * https://help.keyman.com/developer/language/guide/strings
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
 *
 * Uses a NewNodeOrTree to add either a single new DEADKEYNAME
 * node or build a tree rooted at a DEADKEYNAME node, depending
 * on the number of children found (one or more)
 */
export class DeadkeyNameRule extends SingleChildRuleWithASTRebuild {
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
 * https://help.keyman.com/developer/language/guide/strings
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
 *
 * https://help.keyman.com/developer/language/reference/set
 *
 * Uses a GivenNode to rebuild the tree to be rooted at the SET node
 */
export class SetNormalStoreRule extends SingleChildRuleWithASTRebuild {
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
 *
 * https://help.keyman.com/developer/language/reference/set
 *
 * Uses a GivenNode to rebuild the tree to be rooted at the SET node
 */
export class SetSystemStoreRule extends SingleChildRuleWithASTRebuild {
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
 *
 * https://help.keyman.com/developer/language/reference/set
 * https://help.keyman.com/developer/language/reference/layer
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
 *
 * https://help.keyman.com/developer/language/reference/reset
 *
 * Uses a StackedPair to rebuild the tree as RESET parent and STORENAME child nodes
 */
export class ResetStoreRule extends SingleChildRuleWithASTRebuild {
  public constructor() {
    super(new StackedPair(NodeType.RESET, NodeType.STORENAME));
    const reset: Rule           = new TokenRule(TokenType.RESET, true);
    const leftBracket: Rule     = new TokenRule(TokenType.LEFT_BR);
    const normalStoreName: Rule = new NormalStoreNameRule();
    const rightBracket: Rule    = new TokenRule(TokenType.RIGHT_BR);
    this.rule = new SequenceRule([reset, leftBracket, normalStoreName, rightBracket]);
  }
}

/**
 * An abstract base class for rules that match multiword caps lock
 * headers and map them to more modern caps lock system store commands.
 *
 * https://help.keyman.com/developer/language/reference/_keywordsbytype
 * https://help.keyman.com/developer/language/reference/caps
 */
abstract class AbstractCapsLockStatementRule extends SingleChildRule {
  public constructor(
    /** type of token that will be created */
    protected tokenType: TokenType,
    /** type of node that will be added to the tree */
    protected nodeType: NodeType,
  ) {
    super();
  }

  /**
   * Parse a CapsLockStatementRule. A child class needs to identify
   * the nodeType and tokenType that will be added to the AST if
   * the parse is successful.
   *
   * @param tokenBuffer the TokenBuffer to parse
   * @param node where to build the AST
   * @returns true if this rule was successfully parsed
   */
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
 *
 * https://help.keyman.com/developer/language/reference/_keywordsbytype
 * https://help.keyman.com/developer/language/reference/caps
 */
export class CapsAlwaysOffRule extends AbstractCapsLockStatementRule {
  public constructor() {
    super(TokenType.CAPSALWAYSOFF, NodeType.CAPSALWAYSOFF);
    const caps: Rule   = new TokenRule(TokenType.CAPS);
    const always: Rule = new TokenRule(TokenType.ALWAYS);
    const off: Rule    = new TokenRule(TokenType.OFF);
    this.rule = new SequenceRule([caps, always, off]);
  }
}

/**
 * (BNF) capsOnOnly: CAPS ON ONLY
 *
 * https://help.keyman.com/developer/language/reference/_keywordsbytype
 * https://help.keyman.com/developer/language/reference/caps
 */
export class CapsOnOnlyRule extends AbstractCapsLockStatementRule {
  public constructor() {
    super(TokenType.CAPSONONLY, NodeType.CAPSONONLY);
    const caps: Rule = new TokenRule(TokenType.CAPS);
    const on: Rule   = new TokenRule(TokenType.ON);
    const only: Rule = new TokenRule(TokenType.ONLY);
    this.rule = new SequenceRule([caps, on, only]);
  }
}

/**
 * (BNF) shiftFreesCaps: SHIFT FREES CAPS
 *
 * https://help.keyman.com/developer/language/reference/_keywordsbytype
 * https://help.keyman.com/developer/language/reference/caps
 */
export class ShiftFreesCapsRule extends AbstractCapsLockStatementRule {
  public constructor() {
    super(TokenType.SHIFTFREESCAPS, NodeType.SHIFTFREESCAPS);
    const shift: Rule = new TokenRule(TokenType.SHIFT);
    const frees: Rule = new TokenRule(TokenType.FREES);
    const caps: Rule  = new TokenRule(TokenType.CAPS);
    this.rule = new SequenceRule([shift, frees, caps]);
  }
}

/**
 * (BNF) headerAssign: headerName headerValue
 *
 * Uses a FirstNode to rebuild the tree to be rooted at the first node found
 */
export class HeaderAssignRule extends SingleChildRuleWithASTRebuild {
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
 *
 * https://help.keyman.com/developer/language/reference/_keywordsbytype
 */
export class HeaderNameRule extends AlternateTokenRule {
  // TODO-NG-COMPILER: warning/error for header statements
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
