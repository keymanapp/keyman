/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-24
 *
 * KMC KMN Next Generation Semantic Model Builder (KmxBuilder)
 */

import { ASTNode } from './tree-construction.js';
import { DwFileVersion } from './kmx-enums.js';
import { NodeType } from './node-type.js';

/**
 * The Next Generation Semantic Model Builder for the Keyman Keyboard Language.
 *
 * The KMX Builder builds an in-memory model from the supplied Abstract Syntax Tree (AST).
 */
export class KmxBuilder {
  /** the KmxModel to be built */
  private model = new KmxModel();

  /**
   * Construct a KmxBuilder
   */
  public constructor(
    /** the abstract syntax tree (AST) to build from */
    private readonly root: ASTNode
  ) {
  }

  /**
   * Uses the AST to build an in-memory model of a KMN file.
   */
  public build(): KmxModel {
    this.buildHeader();
    return this.model;
  }

  private buildHeader() {
    if (this.root.hasSoleChildOfType(NodeType.STORES)) {
      this.model.dwFileVersion = DwFileVersion.VERSION_190;
    }
  }
}

/*
 * KmxModel is an in-memory model of a KMX file.
 */
export class KmxModel {
  dwFileVersion: DwFileVersion;
}
