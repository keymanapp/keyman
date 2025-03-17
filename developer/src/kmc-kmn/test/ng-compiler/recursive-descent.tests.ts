/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2025-02-25
 *
 * Tests for KMC KMN Next Generation Parser (Recursive Descent/Kleene Operator Rules)
 */

import 'mocha';
import { assert } from 'chai';
import { ManyRule, OneOrManyRule, OptionalRule, Rule } from '../../src/ng-compiler/recursive-descent.js';
import { TokenBuffer } from '../../src/ng-compiler/token-buffer.js';
import { ASTNode, NodeTypes } from '../../src/ng-compiler/tree-construction.js';
import { Token, TokenTypes } from '../../src/ng-compiler/lexer.js';

const LIST_OF_ONE: Token[] = [
  new Token(TokenTypes.TT_STRING, ''),
];

class TrueRule extends Rule {
  public constructor(tokenBuffer: TokenBuffer) {
    super(tokenBuffer);
  }

  public parse(node: ASTNode): boolean {
    if (this.tokenBuffer.nextToken().isTokenType(TokenTypes.TT_EOF))
      return false;
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

let tokenBuffer: TokenBuffer = null;
let root: ASTNode            = null;

describe("Recursive Descent Tests", () => {
  beforeEach(() => {
    tokenBuffer = new TokenBuffer(LIST_OF_ONE);
    root        = new ASTNode(NodeTypes.TMP);
  });
  describe("OptionalRule Tests", () => {
    it("can construct an OptionalRule", () => {
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new OptionalRule(tokenBuffer, child);
      assert.isNotNull(optional);
    });
    it("can parse with a successful child Rule", () => {
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new OptionalRule(tokenBuffer, child);
      assert.isTrue(optional.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with an unsuccessful child Rule", () => {
      const child: Rule     = new FalseRule(tokenBuffer);
      const optional: Rule  = new OptionalRule(tokenBuffer, child);
      assert.isTrue(optional.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("ManyRule Tests", () => {
    it("can construct a ManyRule", () => {
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new ManyRule(tokenBuffer, child);
      assert.isNotNull(optional);
    });
    it("can parse with a successful child Rule (single Token)", () => {
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new ManyRule(tokenBuffer, child);
      assert.isTrue(optional.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three Tokens)", () => {
      tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
      ]);
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new ManyRule(tokenBuffer, child);
      assert.isTrue(optional.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can parse with a unsuccessful child Rule", () => {
      const child: Rule    = new FalseRule(tokenBuffer);
      const optional: Rule = new ManyRule(tokenBuffer, child);
      assert.isTrue(optional.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
  describe("OneOrManyRule Tests", () => {
    it("can construct a OneOrManyRule", () => {
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new OneOrManyRule(tokenBuffer, child);
      assert.isNotNull(optional);
    });
    it("can parse with a successful child Rule (single Token)", () => {
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new OneOrManyRule(tokenBuffer, child);
      assert.isTrue(optional.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 1);
      assert.equal(tokenBuffer.currentPosition, 1);
    });
    it("can parse with a successful child Rule (three Tokens)", () => {
      tokenBuffer = new TokenBuffer([
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
        new Token(TokenTypes.TT_STRING, ''),
      ]);
      const child: Rule    = new TrueRule(tokenBuffer);
      const optional: Rule = new OneOrManyRule(tokenBuffer, child);
      assert.isTrue(optional.parse(root));
      assert.isTrue(root.hasChild());
      assert.isTrue(root.hasChildOfType(NodeTypes.TMP));
      assert.equal(root.getChildren().length, 3);
      assert.equal(tokenBuffer.currentPosition, 3);
    });
    it("can parse with a unsuccessful child Rule", () => {
      const child: Rule    = new FalseRule(tokenBuffer);
      const optional: Rule = new OneOrManyRule(tokenBuffer, child);
      assert.isFalse(optional.parse(root));
      assert.isFalse(root.hasChild());
      assert.equal(tokenBuffer.currentPosition, 0);
    });
  });
});