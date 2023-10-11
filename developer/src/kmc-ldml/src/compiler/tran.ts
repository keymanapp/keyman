import { constants, SectionIdent } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, CompilerCallbacks, VariableParser, MarkerParser } from '@keymanapp/common-types';
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
import { CompilerMessages } from "./messages.js";
import { MarkerTracker, MarkerUse } from "./marker-tracker.js";

type TransformCompilerType = 'simple' | 'backspace';

export class TransformCompiler<T extends TransformCompilerType, TranBase extends Tran> extends SectionCompiler {

  static validateMarkers(keyboard: LDMLKeyboard.LKKeyboard, mt : MarkerTracker): boolean {
    keyboard?.transforms?.forEach(transforms =>
      transforms.transformGroup.forEach(transformGroup => {
        transformGroup.transform?.forEach(({ to, from }) => {
          mt.add(MarkerUse.emit, MarkerParser.allReferences(to));
          mt.add(MarkerUse.consume, MarkerParser.allReferences(from));
        })}));
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
        types => reportMessage(CompilerMessages.Error_DuplicateTransformsType({ types })),
        new Set(['simple', 'backspace']),
        types => reportMessage(CompilerMessages.Error_InvalidTransformsType({ types })))) {
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
        reportMessage(CompilerMessages.Error_MixedTransformGroup()); // report this once
      }
      if (empty) {
        valid = false;
        reportMessage(CompilerMessages.Error_EmptyTransformGroup()); // report this once
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
        result.groups.push(this.compileTransformGroup(sections, transformGroup));
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
    return result;
  }

  private compileTransform(sections: DependencySections, transform: LKTransform) : TranTransform {
    let result = new TranTransform();
    let cookedFrom = transform.from;

    cookedFrom = sections.vars.substituteStrings(cookedFrom, sections);
    // TODO: handle 'map' case
    const mapFrom = VariableParser.CAPTURE_SET_REFERENCE.exec(cookedFrom);
    const mapTo = VariableParser.MAPPED_SET_REFERENCE.exec(transform.to || '');
    if (mapFrom && mapTo) { // TODO-LDML: error cases
      result.mapFrom = sections.strs.allocString(mapFrom[1]); // var name
      result.mapTo = sections.strs.allocString(mapTo[1]); // var name
    } else {
      result.mapFrom = sections.strs.allocString(''); // TODO-LDML
      result.mapTo = sections.strs.allocString(''); // TODO-LDML
    }
    cookedFrom = sections.vars.substituteSetRegex(cookedFrom, sections);

    // add in markers. idempotent if no markers.
    cookedFrom = sections.vars.substituteMarkerString(cookedFrom, true);

    // cookedFrom is cooked above, since there's some special treatment
    result.from = sections.strs.allocString(cookedFrom, {
      unescape: true
    }, sections);
    // 'to' is handled via allocString
    result.to = sections.strs.allocString(transform.to, {
      stringVariables: true,
      markers: true,
      unescape: true,
    }, sections);
    return result;
  }

  private compileReorderTranGroup(sections: DependencySections, reorders: LKReorder[]): TranGroup {
    const result : TranGroup = {
      type: constants.tran_group_type_reorder,
      transforms: [],
      reorders: reorders.map(reorder => this.compileReorder(sections, reorder)),
    }
    return result;
  }

  private compileReorder(sections: DependencySections, reorder: LKReorder): TranReorder {
    let result = new TranReorder();
    result.elements = sections.elem.allocElementString(sections, reorder.from, reorder.order, reorder.tertiary, reorder.tertiaryBase, reorder.preBase);
    result.before = sections.elem.allocElementString(sections, reorder.before);
    return result;
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
      constants.section.strs,
      constants.section.list,
      constants.section.elem,
      constants.section.vars,
      constants.section.uset,
    ]);
    defaults.delete(this.id);
    return defaults;
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
