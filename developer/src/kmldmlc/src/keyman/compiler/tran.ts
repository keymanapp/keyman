import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Finl, FinlItem, Tran, TranItem, TranItemFlags } from "../kmx/kmx-plus";
import { ElementString } from "../kmx/element-string";
import LDMLKeyboardXMLSourceFile, { LKTransform, LKTransforms } from "../ldml-keyboard/ldml-keyboard-xml";
import CompilerCallbacks from "./callbacks";
import { SectionCompiler } from "./section-compiler";

type TransformCompilerType = 'simple' | 'final';

class TransformCompiler<T extends TransformCompilerType, TranBase extends Tran, TranItemBase extends TranItem> extends SectionCompiler {

  protected type: T;

  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
  }

  public validate(): boolean {
    let valid = true;
    // TODO-LDML: linting here should check for identical before+from, but this involves a double-parse which is ugly
    // TODO-LDML: unicodesets means that either we fully parse them and verify conflicting rules or the linting is imperfect
    return valid;
  }

  protected newTranItem(): TranItemBase {
    return null;
  }

  protected newTran(): TranBase {
    return null;
  }

  private compileTransform(transform: LKTransform): TranItemBase {
    let result = this.newTranItem();
    result.from = new ElementString(transform.from);
    result.to = transform.to;
    result.before = new ElementString(transform.before);
    result.flags = transform.error == 'fail' ? TranItemFlags.error : TranItemFlags.none;
    return result;
  }

  private compileTransforms(transforms: LKTransforms): TranBase {
    let result = this.newTran();

    if(transforms?.transform) {
      for(let transform of transforms.transform) {
        result.items.push(this.compileTransform(transform));
      }
    }

    return result;
  }

  public compile(): TranBase {
    for(let t of this.keyboard.transforms) {
      if(t.type == this.type) {
        return this.compileTransforms(t);
      }
    }
    return this.newTran();
  }
}

export class TranCompiler extends TransformCompiler<'simple', Tran, TranItem> {
  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
    this.type = 'simple';
  }
  protected newTranItem(): TranItem {
    return new TranItem();
  }
  protected newTran(): Tran {
    return new Tran();
  }
  public get id() {
    return constants.section.tran;
  }
};

export class FinlCompiler extends TransformCompiler<'final', Finl, FinlItem> {
  constructor(source: LDMLKeyboardXMLSourceFile, callbacks: CompilerCallbacks) {
    super(source, callbacks);
    this.type = 'final';
  }
  protected newTranItem(): FinlItem {
    return new FinlItem();
  }
  protected newTran(): Finl {
    return new Finl();
  }
  public get id() {
    return constants.section.finl;
  }
};
