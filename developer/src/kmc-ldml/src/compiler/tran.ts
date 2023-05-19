import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus, LDMLKeyboard, CompilerCallbacks } from '@keymanapp/common-types';
import { SectionCompiler } from "./section-compiler.js";

import Bksp = KMXPlus.Bksp;
// import BkspItem = KMXPlus.BkspItem;
import GlobalSections = KMXPlus.GlobalSections;
import Tran = KMXPlus.Tran;
// import TranItem = KMXPlus.TranItem;
// import TranItemFlags = KMXPlus.TranItemFlags;
import LDMLKeyboardXMLSourceFile = LDMLKeyboard.LDMLKeyboardXMLSourceFile;
// import LKTransform = LDMLKeyboard.LKTransform;
import LKTransforms = LDMLKeyboard.LKTransforms;
import { verifyValidAndUnique } from "../util/util.js";
import { CompilerMessages } from "./messages.js";

type TransformCompilerType = 'simple' | 'backspace';

class TransformCompiler<T extends TransformCompilerType, TranBase extends Tran/*, TranItemBase extends TranItem*/> extends SectionCompiler {

  protected type: T;

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
  }

  public validate(): boolean {
    const reportMessage = this.callbacks.reportMessage.bind(this.callbacks);

    let valid = true;
    // TODO-LDML: linting here should check for identical from, but this involves a double-parse which is ugly
    // TODO-LDML: unicodesets means that either we fully parse them and verify conflicting rules or the linting is imperfect
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
      transforms.forEach(({transformGroup}) => transformGroup.forEach((transformGroup) => {
        if (transformGroup.reorder?.length && transformGroup.transform?.length) {
          mixed = true;
        }
      }));
      if (mixed) {
        valid = false;
        reportMessage(CompilerMessages.Error_MixedTransformGroup()); // report this once
      }
    }
    return valid;
  }

  // protected newTranItem(): TranItemBase {
  //   return null;
  // }

  protected newTran(): TranBase {
    return null;
  }

  // private compileTransform(sections: GlobalSections, transform: LKTransform) : any {
  //   let result = null;
  //   // let result = this.newTranItem();
  //   // result.from = sections.elem.allocElementString(sections.strs, transform.from);
  //   // result.to = sections.strs.allocAndUnescapeString(transform.to);
  //   // result.before = sections.elem.allocElementString(sections.strs, transform.before);
  //   // result.flags = transform.error == 'fail' ? TranItemFlags.error : TranItemFlags.none;
  //   return result;
  // }

  private compileTransforms(sections: GlobalSections, transforms: LKTransforms): any {
    let result = this.newTran();

    // if(transforms?.transform) {
    //   for(let transform of transforms.transform) {
    //     result.items.push(this.compileTransform(sections, transform));
    //   }
    // }

    return result;
  }

  public compile(sections: GlobalSections): TranBase {
    for(let t of this.keyboard.transforms) {
      if(t.type == this.type) {
        return this.compileTransforms(sections, t);
      }
    }
    return this.newTran();
  }
}

export class TranCompiler extends TransformCompiler<'simple', Tran /*, TranItem*/> {
  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
    this.type = 'simple';
  }
  // protected newTranItem(): TranItem {
  //   return new TranItem();
  // }
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

  // protected newTranItem(): BkspItem {
  //   return new BkspItem();
  // }
  protected newTran(): Bksp {
    return new Bksp();
  }
  public get id() {
    return constants.section.bksp;
  }
};
