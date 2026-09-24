/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-24
 *
 * Tests for KMC KMN Next Generation Semantic Model Builder
 */

import 'mocha';
import { assert } from 'chai';
import { ASTNode } from '../../src/ng-compiler/tree-construction.js';
import { SemanticModelBuilder } from '../../src/ng-compiler/sm-builder.js';

let root: ASTNode = null;

describe("Semantic Model Builder Tests", () => {
  beforeEach(() => {
    root = new ASTNode();
  });
  it("can construct a SemanticModelBuilder", () => {
    const builder: SemanticModelBuilder = new SemanticModelBuilder(root);
    assert.isNotNull(builder);
  });
});