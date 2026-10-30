/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Dr Mark C. Sinclair on 2026-09-24
 *
 * KMC KMN Next Generation Semantic Model Builder (KmxBuilder)
 */

import { ASTNode } from './tree-construction.js';
import { DwFileVersion, DwSystemID, DW_FILE_VERSION_MAP, KMX_FILEID_COMPILED, DwFlags } from './kmx-constants.js';
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
    const versionNode        = this.root.getDescendents(NodeType.VERSION)[0];
    // TODO-NG-COMPILER: error for invalid or missing version
    const versionName        = versionNode?.getText() ?? '';
    const versionString      = versionNode?.getTextOfType(NodeType.STRING) ?? '';
    this.model.dwFileVersion = this.versionToDwFileVersion(versionString);
    this.model.compStore.push(new CompStore(DwSystemID.TSS_VERSION, versionString, versionName));

    //TODO-NG-COMPILER: consider including deprecated variables: dwCheckSum, KeyboardID, IsRegistered, version, StartGroup_ANSI

    // dwFlags
    //TODO-NG-COMPILER: include code to handle SHIFTFREESCAPS_HEADER, CAPSONONLY_HEADER and CAPSALWAYSOFF_HEADER
    const shiftFreesCapsNode = this.root.getDescendents(NodeType.SHIFTFREESCAPS)[0];
    if (shiftFreesCapsNode) {
      const shiftFreesCapsName   = shiftFreesCapsNode.getText() ?? '';
      const shiftFreesCapsString = shiftFreesCapsNode.getTextOfType(NodeType.STRING) ?? '';
      if (this.stripQuoteMarks(shiftFreesCapsString) === '1') {
        this.model.dwFlags |= DwFlags.KF_SHIFTFREESCAPS;
      }
      this.model.compStore.push(new CompStore(DwSystemID.TSS_SHIFTFREESCAPS, shiftFreesCapsString, shiftFreesCapsName));
    }
    const capsOnOnlyNode = this.root.getDescendents(NodeType.CAPSONONLY)[0];
    if (capsOnOnlyNode) {
      const capsOnOnlyName   = capsOnOnlyNode.getText() ?? '';
      const capsOnOnlyString = capsOnOnlyNode.getTextOfType(NodeType.STRING) ?? '';
      if (this.stripQuoteMarks(capsOnOnlyString) === '1') {
        this.model.dwFlags |= DwFlags.KF_CAPSONONLY;
      }
      this.model.compStore.push(new CompStore(DwSystemID.TSS_CAPSONONLY, capsOnOnlyString, capsOnOnlyName));
    }
    const capsAlwaysOffNode = this.root.getDescendents(NodeType.CAPSALWAYSOFF)[0];
    if (capsAlwaysOffNode) {
      const capsAlwaysOffName   = capsAlwaysOffNode.getText() ?? '';
      const capsAlwaysOffString = capsAlwaysOffNode.getTextOfType(NodeType.STRING) ?? '';
      if (this.stripQuoteMarks(capsAlwaysOffString) === '1') {
        this.model.dwFlags |= DwFlags.KF_CAPSALWAYSOFF;
      }
      this.model.compStore.push(new CompStore(DwSystemID.TSS_CAPSALWAYSOFF, capsAlwaysOffString, capsAlwaysOffName));
    }

    // dwHotKey
    //TODO-NG-COMPILER: include code to handle HOTKEY_HEADER
    //TODO-NG-COMPILER: code to handle anything but a single virtual key argument
    const hotKeyNode = this.root.getDescendents(NodeType.HOTKEY)[0];
    if (hotKeyNode) {
      const hotKeyName = hotKeyNode.getText() ?? '';
      const source     = hotKeyNode.getTextOfType(NodeType.SOURCE);
      this.model.compStore.push(new CompStore(DwSystemID.TSS_HOTKEY, source, hotKeyName));
    }
  }

  private stripQuoteMarks(text: string): string {
    return text.replace(/^['"]|['"]$/gm, '');
  }

  private versionToDwFileVersion(text: string): DwFileVersion {
    text = this.stripQuoteMarks(text);
    return DW_FILE_VERSION_MAP.get(Number(text));
  }
}

/*
 * KmxModel is an in-memory model of a KMX file.
 */
export class KmxModel {
  dwIdentifier = KMX_FILEID_COMPILED;
  dwFileVersion: DwFileVersion;
  dwCheckSum   = 0x0; // deprecated
  KeyboardID   = 0x0; // deprecated
  IsRegistered = 0x1; // deprecated
  version      = 0x0; // deprecated
  cxStoreArray: number;
  cxGroupArray: number;
  dpStoreArray: number;
  dpGroupArray: number;
  StartGroup_ANSI    = 0xFFFFFFFF; // deprecated
  StartGroup_Unicode = 0xFFFFFFFF;
  dwFlags  = 0x0;
  dwHotKey = 0x0;
  dpBitmapOffset: number;
  dwBitmapSize: number;
  compStore: CompStore[] = [];
}

export class CompStore {
  constructor(
    public readonly dwSystemID: DwSystemID,
    public readonly dpString: string,
    public readonly dpName=''
  ) {}
}
