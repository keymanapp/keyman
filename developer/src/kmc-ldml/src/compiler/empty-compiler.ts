import { SectionIdent } from '@keymanapp/ldml-keyboard-constants';
import { SectionCompiler } from "./section-compiler.js";
import { LDMLKeyboard, KMXPlus, CompilerCallbacks } from "@keymanapp/common-types";


/**
 * Compiler for typrs that don't actually consume input XML
 */
export abstract class EmptyCompiler extends SectionCompiler {
  private _id: SectionIdent;
  constructor(id: SectionIdent, source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
    this._id = id;
  }

  public get id(): SectionIdent {
    return this._id;
  }
  public get dependencies(): Set<SectionIdent> {
    return new Set(); // no dependencies
  }
}

export class StrsCompiler extends EmptyCompiler {
  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super('strs', source, callbacks);
  }
  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    return new KMXPlus.Strs();
  }
}

export class ElemCompiler extends EmptyCompiler {
  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super('elem', source, callbacks);
  }
  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    return new KMXPlus.Elem(sections.strs);
  }
  public get dependencies(): Set<SectionIdent> {
    const strsOnly = new Set(<SectionIdent[]>['strs']);
    return strsOnly;
  }
}

export class ListCompiler extends EmptyCompiler {
  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super('list', source, callbacks);
  }
  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    return new KMXPlus.List(sections.strs);
  }
  public get dependencies(): Set<SectionIdent> {
    const strsOnly = new Set(<SectionIdent[]>['strs']);
    return strsOnly;
  }
}
