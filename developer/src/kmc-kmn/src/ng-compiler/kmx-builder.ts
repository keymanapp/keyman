/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-24
 *
 * KMC KMN Next Generation Semantic Model Builder (KmxBuilder)
 */

import { ASTNode } from './tree-construction.js';
import { DwFileVersion, DwSystemID, DW_FILE_VERSION_MAP } from './kmx-constants.js';
import { NodeType } from './node-type.js';

/**
 * The Next Generation Semantic Model Builder for the Keyman Keyboard Language.
 *
 * The KMX Builder builds an in-memory model from the supplied Abstract Syntax Tree (AST).
 */
export class KmxBuilder {
  /** the KmxModel to be built */
  private readonly model = new KmxModel();

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
    // TODO-NG-COMPILER: errors for invalid AST structure
    // add deprecated COMP_STOREs that are always preset
    this.model.compStore.push(new CompStore(DwSystemID.TSS_CUSTOMKEYMANEDITION, '0'));
    this.model.compStore.push(new CompStore(DwSystemID.TSS_CUSTOMKEYMANEDITIONNAME, 'Keyman'));

    // dwFileVersion
    const versionNode = this.root.getDescendents(NodeType.VERSION)[0];
    // TODO-NG-COMPILER: error for invalid version
    const versionText = versionNode?.getText() ?? '';
    const valueText   = versionNode?.getTextOfType(NodeType.STRING) ?? '';
    this.model.dwFileVersion = this.versionToDwFileVersion(valueText);
    this.model.compStore.push(new CompStore(DwSystemID.TSS_VERSION, versionText));
  }

  private versionToDwFileVersion(text: String): DwFileVersion {
    text = text.replace(/^['|"]|['"]$/gm, '');
    return DW_FILE_VERSION_MAP.get(Number(text));
  }
}

/*
 * KmxModel is an in-memory model of a KMX file.
 */
export class KmxModel {
  dwFileVersion: DwFileVersion;
  compStore: CompStore[] = [];
}

export class CompStore {
  public dpName: Number;
  constructor(
    public readonly dwSystemID: DwSystemID,
    public readonly dpString: String
  ) {
    this.dpName = 0;
  }
}
