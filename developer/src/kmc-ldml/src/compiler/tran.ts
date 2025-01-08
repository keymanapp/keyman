import { constants, SectionIdent } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LdmlKeyboardTypes, util } from '@keymanapp/common-types';
import { CompilerCallbacks, LDMLKeyboard } from "@keymanapp/developer-utils";
import { SectionCompiler } from "./section-compiler.js";

import Bksp = KMXPlus.Bksp;
import DependencySections = KMXPlus.DependencySections;
import Tran = KMXPlus.Tran;
import TranGroup = KMXPlus.TranGroup;
import TranReorder = KMXPlus.TranReorder;
import TranTransform = KMXPlus.TranTransform;
import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
import LKTransformGroup = LDMLKeyboard.LKTransformGroup;
import LKReorder = LDMLKeyboard.LKReorder;
import LKTransform = LDMLKeyboard.LKTransform;
import LKTransforms = LDMLKeyboard.LKTransforms;
import { verifyValidAndUnique } from "../util/util.js";
import { LdmlCompilerMessages } from "./ldml-compiler-messages.js";
import { Substitutions, SubstitutionUse } from "./substitution-tracker.js";

type TransformCompilerType = 'simple' | 'backspace';

export abstract class TransformCompiler<T extends TransformCompilerType, TranBase extends Tran> extends SectionCompiler {

  static validateSubstitutions(keyboard: LDMLKeyboard.LKKeyboard, st: Substitutions): boolean {
    keyboard?.transforms?.forEach(transforms =>
      transforms.transformGroup.forEach(transformGroup => {
        transformGroup.transform?.forEach(({ to, from }) => {
          st.addSetAndStringSubtitution(SubstitutionUse.consume, from);
          st.addSetAndStringSubtitution(SubstitutionUse.emit, to);
          const mapFrom = LdmlKeyboardTypes.VariableParser.CAPTURE_SET_REFERENCE.exec(from);
          const mapTo = LdmlKeyboardTypes.VariableParser.MAPPED_SET_REFERENCE.exec(to || '');
          if (mapFrom) {
            // add the 'from' as a match
            st.set.add(SubstitutionUse.consume, [mapFrom[1]]);
          }
          if (mapTo) {
            // add the 'from' as a match
            st.set.add(SubstitutionUse.emit, [mapTo[1]]);
          }
        });
        transformGroup.reorder?.forEach(({ before }) => {
          st.addStringSubstitution(SubstitutionUse.consume, before);
        });
      }));
    return true;
  }

  protected type: T;

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
  }

  public validate(): boolean {
    const reportMessage = this.callbacks.reportMessage.bind(this.callbacks);

    let valid = true;
    const transforms = this?.keyboard3?.transforms;
    if (transforms) {
      const types : string[] = transforms.map(({type}) => type);
      if (!verifyValidAndUnique(types,
        types => reportMessage(LdmlCompilerMessages.Error_DuplicateTransformsType({ types })),
        new Set(['simple', 'backspace']),
        types => reportMessage(LdmlCompilerMessages.Error_InvalidTransformsType({ types })))) {
        valid = false;
      }

      // check for mixed groups
      let mixed = false;
      let empty = false;
      transforms.forEach(({transformGroup}) => transformGroup.forEach((transformGroup) => {
        if (transformGroup.reorder?.length && transformGroup.transform?.length) {
          mixed = true;
        }
        if (!transformGroup.reorder?.length && !transformGroup.transform?.length) {
          empty = true;
        }
      }));
      if (mixed) {
        valid = false;
        reportMessage(LdmlCompilerMessages.Error_MixedTransformGroup()); // report this once
      }
      if (empty) {
        valid = false;
        reportMessage(LdmlCompilerMessages.Error_EmptyTransformGroup()); // report this once
      }

      // TODO-LDML: linting here should check for identical from, but this involves a double-parse which is ugly
      // TODO-LDML: unicodesets means that either we fully parse them and verify conflicting rules or the linting is imperfect
    }
    return valid;
  }

  /* c8 ignore next 4 */
  /** allocate a new TranBase subclass */
  protected newTran(): TranBase {
    throw Error(`Internal Error: newTran() not implemented`);
  }

  private compileTransforms(sections: DependencySections, transforms: LKTransforms): TranBase {
    let result = this.newTran();

    if (transforms?.transformGroup) {
      for (let transformGroup of transforms.transformGroup) {
        const tg = this.compileTransformGroup(sections, transformGroup);
        if (!tg) return null; //error
        result.groups.push(tg);
      }
    }
    return result;
  }

  private compileTransformGroup(sections: DependencySections, transformGroup: LKTransformGroup): TranGroup {
    if (transformGroup.reorder.length && transformGroup.transform.length) {
      /* c8 ignore next 2 */
      // should have been caught by validate
      throw Error(`Internal error: transformGroup has both reorder and transform elements.`);
    } else if (transformGroup.reorder.length) {
      return this.compileReorderTranGroup(sections, transformGroup.reorder);
    } else if (transformGroup.transform.length) {
      return this.compileTransformTranGroup(sections, transformGroup.transform);
    } else {
      /* c8 ignore next */
      throw Error(`Internal error: transformGroup has neither reorder nor transform elements.`);
    }
  }

  private compileTransformTranGroup(sections: DependencySections, transforms: LKTransform[]): TranGroup {
    const result : TranGroup = {
      type: constants.tran_group_type_transform,
      transforms: transforms.map(transform => this.compileTransform(sections, transform)),
      reorders: [],
    }
    if (result.transforms.includes(null)) return null;
    return result;
  }

  private compileTransform(sections: DependencySections, transform: LKTransform) : TranTransform {
    let result = new TranTransform();
    let cookedFrom = transform.from;

    // check for incorrect \uXXXX escapes. Do this before substituting markers or sets.
    cookedFrom = this.checkEscapes(cookedFrom); // check for \uXXXX escapes before normalizing

    cookedFrom = sections.vars.substituteStrings(cookedFrom, sections, true);
    const mapFrom = LdmlKeyboardTypes.VariableParser.CAPTURE_SET_REFERENCE.exec(cookedFrom);
    const mapTo = LdmlKeyboardTypes.VariableParser.MAPPED_SET_REFERENCE.exec(transform.to || '');
    if (mapFrom && mapTo) { // TODO-LDML: error cases
      result.mapFrom = sections.strs.allocString(mapFrom[1]); // var name
      result.mapTo = sections.strs.allocString(mapTo[1]); // var name
    } else {
      result.mapFrom = sections.strs.allocString('');
      result.mapTo = sections.strs.allocString('');

      // validate 'to' here
      if (!this.isValidTo(transform.to || '')) {
        return null;
      }
    }

    if (cookedFrom === null) return null; // error

    // the set substution will not produce raw markers `\m{...}` but they will already be in sentinel form.
    cookedFrom = sections.vars.substituteSetRegex(cookedFrom, sections);

    // add in markers. idempotent if no markers.
    cookedFrom = sections.vars.substituteMarkerString(cookedFrom, true);

    // unescape from \u{} form to plain, or in some cases \uXXXX / \UXXXXXXXX for core
    cookedFrom = util.unescapeStringToRegex(cookedFrom);

    // check for denormalized ranges
    cookedFrom = this.checkRanges(cookedFrom); // check before normalizing

    if (!sections?.meta?.normalizationDisabled) {
      // nfd here.
      cookedFrom = LdmlKeyboardTypes.MarkerParser.nfd_markers(cookedFrom, true);
    }

    // perform regex validation
    if (!this.isValidRegex(cookedFrom, transform.from)) {
      return null;
    }

    // cookedFrom is cooked above, since there's some special treatment
    result.from = sections.strs.allocString(cookedFrom, {
      unescape: false,
    }, sections);
    // 'to' is handled via allocString
    result.to = sections.strs.allocString(transform.to, {
      stringVariables: true,
      markers: true,
      unescape: true,
      nfd: true,
    }, sections);
    return result;
  }

  /**
   * Validate the 'to' string.
   * We have already checked that it's not a mapTo,
   * so there should not be any illegal substitutions.
   */
  private isValidTo(to: string) : boolean {
    if (/(?<!\\)(?:\\\\)*\$\[/.test(to)) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_IllegalTransformToUset({ to }));
      return false;
    }
    return true;
  }

  /**
   * Validate the final regex
   * @param cookedFrom the regex to use, missing the trailing '$'
   * @param from the original from - for error reporting
   * @returns true if OK
   */
  private isValidRegex(cookedFrom: string, from: string) : boolean {
    // check for any unescaped dollar sign here
    if (/(?<!\\)(?:\\\\)*\$/.test(cookedFrom)) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_IllegalTransformDollarsign({ from }));
      return false;
    }
    if (/(?<!\\)(?:\\\\)*\*/.test(cookedFrom)) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_IllegalTransformAsterisk({ from }));
      return false;
    }
    if (/(?<!\\)(?:\\\\)*\+/.test(cookedFrom)) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_IllegalTransformPlus({ from }));
      return false;
    }
    // Verify that the regex is syntactically valid
    try {
      const rg = new RegExp(cookedFrom + '$', 'ug');
      // Tests against the regex:

      // does it match an empty string?
      if (rg.test('')) {
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_TransformFromMatchesNothing({ from }));
        return false;
      }
    } catch (e) {
      // We're exposing the internal regex error message here.
      // In the future, CLDR plans to expose the EBNF for the transform,
      // at which point we would have more precise validation prior to getting to this point.
      this.callbacks.reportMessage(LdmlCompilerMessages.Error_UnparseableTransformFrom({ from, message: e.message }));
      return false;
    }
    return true;
  }

  private compileReorderTranGroup(sections: DependencySections, reorders: LKReorder[]): TranGroup {
    const result : TranGroup = {
      type: constants.tran_group_type_reorder,
      transforms: [],
      reorders: reorders.map(reorder => this.compileReorder(sections, reorder)),
    }
    if (result.reorders.includes(null)) return null; // if any of the reorders returned null, fail the entire group.
    return result;
  }

  private compileReorder(sections: DependencySections, reorder: LKReorder): TranReorder {
    let result = new TranReorder();
    if (reorder.from && this.checkEscapes(reorder.from) === null) {
      return null; // error'ed
    }
    if (reorder.before && this.checkEscapes(reorder.before) === null) {
      return null; // error'ed
    }
    result.elements = sections.elem.allocElementString(sections, reorder.from, reorder.order, reorder.tertiary, reorder.tertiaryBase, reorder.preBase);
    result.before = sections.elem.allocElementString(sections, reorder.before);
    if (!result.elements || !result.before) {
      return null; // already error'ed
    } else {
      return result;
    }
  }

  public compile(sections: DependencySections): TranBase {
    for(let t of this.keyboard3.transforms) {
      if(t.type == this.type) {
        // compile only the transforms of the correct type
        return this.compileTransforms(sections, t);
      }
    }
    return this.newTran(); // empty: nothing of this type found.
  }
  public get dependencies(): Set<SectionIdent> {
    const defaults = new Set(<SectionIdent[]>[
      constants.section.elem,
      constants.section.list,
      constants.section.meta,
      constants.section.strs,
      constants.section.uset,
      constants.section.vars,
    ]);
    defaults.delete(this.id);
    return defaults;
  }

  /**
   * Analyze reorders and regexes for \uXXXX escapes.
   * The LDML spec requires \u{XXXX} format.
   * @param cookedFrom the original string
   * @returns the original string, or null if an error was reported
   */
  private checkEscapes(cookedFrom: string): string | null {
    if (!cookedFrom) return cookedFrom;

    // should not follow marker prefix, nor marker prefix with range
    const anyQuad = /(?<!\\uffff\\u0008(?:\[[0-9a-fA-F\\u-]*)?)\\u([0-9a-fA-F]{4})/g;

    for (const [, sub] of cookedFrom.matchAll(anyQuad)) {
      const s = util.unescapeOne(sub);
      if (s !== '\uffff' && s !== '\u0008') { // markers
        this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidQuadEscape({ cp: s.codePointAt(0) }));
        return null; // exit on the first error
      }
    }
    return cookedFrom;
  }

  /**
   * Analyze character classes such as '[a-z]' for denormalized characters.
   * Escapes non-NFD characters as hex escapes.
   * @param cookedFrom input regex string
   * @returns updated 'from' string
   */
  private checkRanges(cookedFrom: string): string {
    if (!cookedFrom) return cookedFrom;

    // extract all of the potential ranges - but don't match any-markers!
    const anyRange = /(?<!\\uffff\\u0008)\[([^\]]+)\]/g;
    const ranges = cookedFrom.matchAll(anyRange);

    if (!ranges) return cookedFrom;

    // extract inner members of a range (inside the [])
    const rangeRegex = /(\\u\{[0-9a-fA-F]\}{1,6}|.)-(\\u\{[0-9a-fA-F]\}{1,6}|.)|./g;

    const rangeExplicit = new util.NFDAnalyzer();
    const rangeImplicit = new util.NFDAnalyzer();

    /** process an explicit entry */
    function processExplicit(s: string) {
      if (s.startsWith('\\u{')) {
        s = util.unescapeString(s);
      }
      rangeExplicit.add(s);
      return s;
    }

    for (const [, sub] of ranges) {
      const subRanges = sub.matchAll(rangeRegex);
      for (const [all, start, end] of subRanges) {
        if (!start && !end) {
          // explicit single char
          processExplicit(all); // matched one char
        } else {
          // start-end range - get explicit start and end chars
          const s = processExplicit(start);
          const sch = s.codePointAt(0);
          const e = processExplicit(end);
          const ech = e.codePointAt(0);
          // now, process the inner chars, not including explicit
          for (let n = sch; n < ech; n++) {
            // add inner text
            rangeImplicit.add(String.fromCodePoint(n));
          }
        }
      }
    }

    // analyze ranges
    let needCooking = false;
    const explicitSet = rangeExplicit.analyze()?.get(util.BadStringType.denormalized);
    if (explicitSet) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Warn_CharClassExplicitDenorm({ lowestCh: explicitSet.values().next().value }));
      needCooking = true;
    } else {
      // don't analyze the implicit set of THIS range, if explicit is already problematic
      const implicitSet = rangeImplicit.analyze()?.get(util.BadStringType.denormalized);
      if (implicitSet) {
        this.callbacks.reportMessage(LdmlCompilerMessages.Hint_CharClassImplicitDenorm({ lowestCh: implicitSet.values().next().value }));
        needCooking = true;
      }
    }

    // do we need to fixup the ranges?
    // don't do this unless we flagged issues above
    if (needCooking) {
      // if we get here, there are some ranges with troublesome chars.
      // we work around this by escaping all chars
      function cookOne(s: string) {
        if (s === '^') {
          return s; // syntax
        } else if (s.startsWith('\\u{') || s.startsWith('\\u')) {
          return s; // already escaped
        } else {
          return util.escapeRegexChar(s);
        }
      }
      return cookedFrom.replaceAll(anyRange, (ignored1: any, sub: string) => {
        return '[' + sub.replaceAll(rangeRegex, (all: any, start: string, end: string) => {
          if (!start && !end) {
            // explicit single char
            return cookOne(all); // matched one char
          } else {
            return cookOne(start) + '-' + cookOne(end);
          }
        }) + ']';
      });
    }
    return cookedFrom; // no change
  }
}

export class TranCompiler extends TransformCompiler<'simple', Tran /*, TranItem*/> {
  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
    this.type = 'simple';
  }
  protected newTran(): Tran {
    return new Tran();
  }
  public get id() {
    return constants.section.tran;
  }
};

export class BkspCompiler extends TransformCompiler<'backspace', Bksp /*, BkspItem*/> {
  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
    this.type = 'backspace';
  }
  protected newTran(): Bksp {
    return new Bksp();
  }
  public get id() {
    return constants.section.bksp;
  }
};
