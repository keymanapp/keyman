import { SectionIdent, constants } from '@keymanapp/ldml-keyboard-constants';
import { SectionCompiler } from "./section-compiler.js";
import { LDMLKeyboard, KMXPlus, CompilerCallbacks } from "@keymanapp/common-types";
import { VarsCompiler } from './vars.js';

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
    super(constants.section.strs, source, callbacks);
  }
  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    return new KMXPlus.Strs();
  }
}

export class ElemCompiler extends EmptyCompiler {
  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(constants.section.elem, source, callbacks);
  }
  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    return new KMXPlus.Elem(sections);
  }
  public get dependencies(): Set<SectionIdent> {
    const strsOnly = new Set(<SectionIdent[]>[constants.section.strs]);
    return strsOnly;
  }
}

export class ListCompiler extends EmptyCompiler {
  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(constants.section.list, source, callbacks);
  }
  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    return new KMXPlus.List(sections.strs);
  }
  public get dependencies(): Set<SectionIdent> {
    const strsOnly = new Set(<SectionIdent[]>[constants.section.strs]);
    return strsOnly;
  }
}

export class UsetCompiler extends EmptyCompiler {
  constructor(source: LDMLKeyboard.LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(constants.section.uset, source, callbacks);
  }
  public compile(sections: KMXPlus.DependencySections): KMXPlus.Section {
    return new KMXPlus.Uset();
  }
  public get dependencies(): Set<SectionIdent> {
    const strsOnly = new Set(<SectionIdent[]>[constants.section.strs]);
    return strsOnly;
  }
}

/**
 * For test use. The top compilers.
 */
export const BASIC_DEPENDENCIES = [ StrsCompiler, ListCompiler, ElemCompiler, VarsCompiler ];
