import { SectionIdent, constants } from '@keymanapp/ldml-keyboard-constants';
import { SectionCompiler } from "./section-compiler.js";
import { util, LDMLKeyboard, KMXPlus, CompilerCallbacks, MarkerParser } from "@keymanapp/common-types";
import { VarsCompiler } from './vars.js';
import { LdmlCompilerMessages } from './ldml-compiler-messages.js';

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
  public postValidate(section?: KMXPlus.Section): boolean {
    const strs = <KMXPlus.Strs>section;

    if (strs) {
      const badStringAnalyzer = new util.BadStringAnalyzer();
      const CONTAINS_MARKER_REGEX = new RegExp(MarkerParser.ANY_MARKER_MATCH);
      for (let s of strs.allProcessedStrings.values()) {
        // replace all \\uXXXX with the actual code point.
        // this lets us analyze whether there are PUA, unassigned, etc.
        // the results might not be valid regex of course.
        if (util.CONTAINS_QUAD_ESCAPE.test(s)) {
          s = util.unescapeQuadString(s);
        }
        // skip marker strings
        if (CONTAINS_MARKER_REGEX.test(s)) {
          // it had a marker, take out all marker strings, as the sentinel is illegal
          // need a new regex to match
          const REPLACE_MARKER_REGEX = new RegExp(MarkerParser.ANY_MARKER_MATCH, 'g');
          s = s.replaceAll(REPLACE_MARKER_REGEX, ''); // remove markers.
        }
        badStringAnalyzer.add(s);
      }
      const m = badStringAnalyzer.analyze();
      if (m?.size > 0) {
        const puas = m.get(util.BadStringType.pua);
        const unassigneds = m.get(util.BadStringType.unassigned);
        const illegals = m.get(util.BadStringType.illegal);
        if (puas) {
          const [count, lowestCh] = [puas.size, Array.from(puas.values()).sort((a, b) => a - b)[0]];
          this.callbacks.reportMessage(LdmlCompilerMessages.Hint_PUACharacters({ count, lowestCh }))
        }
        if (unassigneds) {
          const [count, lowestCh] = [unassigneds.size, Array.from(unassigneds.values()).sort((a, b) => a - b)[0]];
          this.callbacks.reportMessage(LdmlCompilerMessages.Warn_UnassignedCharacters({ count, lowestCh }))
        }
        if (illegals) {
          // do this last, because we will return false.
          const [count, lowestCh] = [illegals.size, Array.from(illegals.values()).sort((a, b) => a - b)[0]];
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_IllegalCharacters({ count, lowestCh }))
          return false;
        }
      }
    }
    return true;
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
