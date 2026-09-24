/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-24
 *
 * KMC KMN Next Generation Semantic Model Builder
 */

import { ASTNode } from '../../src/ng-compiler/tree-construction.js';

/**
 * The Next Generation Semantic Model Builder for the Keyman Keyboard Language.
 *
 * The Semantic Model Builder builds an in-memory model from the supplied Abstract Syntax Tree.
 */
export class SemanticModelBuilder {
  /**
   * Construct a SemanticModelBuilder
   */
  public constructor(
    /** the abstract syntax tree (AST) to build from */
    private readonly root: ASTNode
  ) {
  }

  /**
   * Uses the AST to build an in-memory model of a KMN file.
   */
  public build(): ASTNode {
    return this.root;
  }
}