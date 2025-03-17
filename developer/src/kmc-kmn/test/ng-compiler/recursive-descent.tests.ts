/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Kleene Operator Rules)
 */

import 'mocha';
import { assert } from 'chai';
import { OptionalRule, Rule } from '../../src/ng-compiler/recursive-descent.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';
import { Token, TokenTypes } from '../../src/ng-compiler/lexer.js';

// nomatch > layer('default')
const LIST: Token[] = [
  new Token(TokenTypes.TT_NOMATCH, 'nomatch'),
  new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 8),
  new Token(TokenTypes.TT_CHEVRON, '>', 1, 9),
  new Token(TokenTypes.TT_WHITESPACE, ' ', 1, 10),
  new Token(TokenTypes.TT_LAYER, 'layer', 1, 11),
  new Token(TokenTypes.TT_LEFT_BR, '(', 1, 16),
  new Token(TokenTypes.TT_STRING, '\'default\'', 1, 17),
  new Token(TokenTypes.TT_RIGHT_BR, ')', 1, 26),
];

class TrueRule extends Rule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
  }

  public parse(node: ASTNode): boolean {
    this.tokenBuffer.popToken();
    node.addChild(new ASTNode(NodeTypes.TMP));
    return true;
  }
}

class FalseRule extends Rule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
  }

  public parse(node: ASTNode): boolean {
    this.tokenBuffer.popToken();
    return false;
  }
}

describe("Recursive Descent Tests", () => {
  describe("OptionalRule Tests", () => {
    it("can construct an OptionalRule", () => {
      const tokenBuffer    = new TokenBuffer(LIST);
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new OptionalRule(tokenBuffer, child);
      assert.isNotNull(optional);
    });
    it("can parse with a successful child Rule", () => {
      const tokenBuffer     = new TokenBuffer(LIST);
      const child: Rule     = new TrueRule(tokenBuffer);
      const optional: Rule  = new OptionalRule(tokenBuffer, child);
      const root: ASTNode   = new ASTNode(NodeTypes.TMP);
      assert.isTrue(optional.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule", () => {
      const tokenBuffer     = new TokenBuffer(LIST);
      const child: Rule     = new FalseRule(tokenBuffer);
      const optional: Rule  = new OptionalRule(tokenBuffer, child);
      const root: ASTNode   = new ASTNode(NodeTypes.TMP);
      assert.isTrue(optional.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
});