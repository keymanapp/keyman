/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-24
 *
 * KMC KMN Next Generation Semantic Model Builder (KmxBuilder)
 */

import { ASTNode } from './tree-construction.js';
import { DwFileVersion, DwSystemID } from './kmx-enums.js';
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
    // TODO-NG-COMPILER: errors for invalid AST structure
    // add deprecated COMP_STOREs that are always preset
    this.model.compStore.push(new CompStore(DwSystemID.TSS_CUSTOMKEYMANEDITION, '0'));
    this.model.compStore.push(new CompStore(DwSystemID.TSS_CUSTOMKEYMANEDITIONNAME, 'Keyman'));

    // dwFileVersion
    const versionNode = this.root.getDescendents(NodeType.VERSION)[0];
    // TODO-NG-COMPILER: error for invalid version
    const versionText = versionNode.getText();
    this.model.dwFileVersion = this.versionToDwFileVersion(versionText);
    this.model.compStore.push(new CompStore(DwSystemID.TSS_VERSION, versionText));
  }

  private versionToDwFileVersion(text: String): DwFileVersion {
    const versionMap = new Map<Number, DwFileVersion>([
      [3.0,  DwFileVersion.VERSION_30],
      [3.1,  DwFileVersion.VERSION_31],
      [3.2,  DwFileVersion.VERSION_32],
      [4.0,  DwFileVersion.VERSION_40],
      [5.0,  DwFileVersion.VERSION_50],
      [5.1,  DwFileVersion.VERSION_501],
      [6.0,  DwFileVersion.VERSION_60],
      [7.0,  DwFileVersion.VERSION_70],
      [8.0,  DwFileVersion.VERSION_80],
      [9.0,  DwFileVersion.VERSION_90],
      [10.0, DwFileVersion.VERSION_100],
      [11.0, DwFileVersion.VERSION_100],
      [12.0, DwFileVersion.VERSION_100],
      [13.0, DwFileVersion.VERSION_100],
      [14.0, DwFileVersion.VERSION_140],
      [15.0, DwFileVersion.VERSION_150],
      [16.0, DwFileVersion.VERSION_160],
      [17.0, DwFileVersion.VERSION_170],
      [18.0, DwFileVersion.VERSION_170],
      [19.0, DwFileVersion.VERSION_190],
    ]);
    return versionMap.get(Number(text));
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
