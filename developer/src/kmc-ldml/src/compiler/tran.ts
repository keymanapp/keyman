import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, CompilerCallbacks } from '@keymanapp/common-types';
import { SectionCompiler } from "./section-compiler.js";

import Bksp = KMXPlus.Bksp;
import GlobalSections = KMXPlus.GlobalSections;
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

type TransformCompilerType = 'simple' | 'backspace';

class TransformCompiler<T extends TransformCompilerType, TranBase extends Tran> extends SectionCompiler {

  protected type: T;

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
  }

  public validate(): boolean {
    const reportMessage = this.callbacks.reportMessage.bind(this.callbacks);

    let valid = true;
    const transforms = this?.keyboard?.transforms;
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

  protected newTran(): TranBase {
    // needs to be overridden in the subclass
    return null;
  }

  private compileTransforms(sections: GlobalSections, transforms: LKTransforms): TranBase {
    let result = this.newTran();

    if (transforms?.transformGroup) {
      for (let transformGroup of transforms.transformGroup) {
        result.groups.push(this.compileTransformGroup(sections, transformGroup));
      }
    }
    return result;
  }

  private compileTransformGroup(sections: GlobalSections, transformGroup: LKTransformGroup): TranGroup {
    if (transformGroup.reorder.length && transformGroup.transform.length) {
      // should have been caught by validate
      throw Error(`Internal error: transformGroup has both reorder and transform elements.`);
    } else if (transformGroup.reorder.length) {
      return this.compileReorderTranGroup(sections, transformGroup.reorder);
    } else if (transformGroup.transform.length) {
      return this.compileTransformTranGroup(sections, transformGroup.transform);
    } else {
      throw Error(`Internal error: transformGroup has neither reorder nor transform elements.`);
    }
  }

  private compileTransformTranGroup(sections: GlobalSections, transforms: LKTransform[]): TranGroup {
    const result : TranGroup = {
      type: constants.tran_group_type_transform,
      transforms: transforms.map(transform => this.compileTransform(sections, transform)),
      reorders: [],
    }
    return result;
  }

  private compileTransform(sections: GlobalSections, transform: LKTransform) : TranTransform {
    let result = new TranTransform();
    let cookedFrom = transform.from;
    let cookedTo = transform.to;

    // TODO-LDML: cook it

    result.from = sections.elem.allocElementString(sections.strs, cookedFrom);
    result.to = sections.strs.allocAndUnescapeString(cookedTo);

    result.mapFrom = null; // TODO-LDML
    result.mapTo = null; // TODO-LDML

    return result;
  }

  private compileReorderTranGroup(sections: GlobalSections, reorders: LKReorder[]): TranGroup {
    const result : TranGroup = {
      type: constants.tran_group_type_reorder,
      transforms: [],
      reorders: reorders.map(reorder => this.compileReorder(sections, reorder)),
    }
    return result;
  }

  private compileReorder(sections: GlobalSections, reorder: LKReorder): TranReorder {
    let result = new TranReorder();
    result.elements = sections.elem.allocElementString(sections.strs, reorder.from, reorder.order, reorder.tertiary, reorder.tertiaryBase, reorder.preBase);
    result.before = sections.elem.allocElementString(sections.strs, reorder.before);
    return result;
  }

  public compile(sections: GlobalSections): TranBase {
    for(let t of this.keyboard.transforms) {
      if(t.type == this.type) {
        // compile only the transforms of the correct type
        return this.compileTransforms(sections, t);
      }
    }
    return this.newTran(); // empty: nothing of this type found.
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
