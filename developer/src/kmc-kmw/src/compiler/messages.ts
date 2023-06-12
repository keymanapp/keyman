import { KmnCompilerMessages } from "@keymanapp/kmc-kmn";
import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageSpec as m } from "@keymanapp/common-types";

const Namespace = CompilerErrorNamespace.KmwCompiler;
// const SevInfo = CompilerErrorSeverity.Info | Namespace;
// const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
// const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

export class KmwCompilerMessages extends KmnCompilerMessages {
  // Note: for legacy reasons, KMWCompilerMessages extends from
  // KMNCompilerMessages as they share the same error codes. This can be a
  // little confusing because kmcmplib still builds its own error message
  // strings, not kmc-kmn, whereas the kmc-kmw messages are defined here.
  // However, as the kmc-kmw module may be going away at some point, it's
  // probably not worth the splitting of all KMW-specific error messages out of
  // the KmnCompilerMessages space.

  static Error_InvalidBegin = () => m(this.ERROR_InvalidBegin,
    `A "begin unicode" statement is required to compile a KeymanWeb keyboard`);
  static Error_TouchLayoutFileInvalid = () => m(this.ERROR_InvalidTouchLayoutFile,
    `Touch layout file is not valid`);
  static Warn_DontMixChiralAndNonChiralModifiers = () => m(this.WARN_DontMixChiralAndNonChiralModifiers,
    `This keyboard contains Ctrl,Alt and LCtrl,LAlt,RCtrl,RAlt sets of modifiers. Use only one or the other set for web target.`);
  static Warn_OptionStoreNameInvalid = (o:{name:string}) => m(this.WARN_OptionStoreNameInvalid,
    `The option store ${o.name} should be named with characters in the range A-Z, a-z, 0-9 and _ only.`);
  static Error_VirtualCharacterKeysNotSupportedInKeymanWeb = () => m(this.ERROR_VirtualCharacterKeysNotSupportedInKeymanWeb,
    `Virtual character keys not currently supported in KeymanWeb`);
  static Error_VirtualKeysNotValidForMnemonicLayouts = () => m(this.ERROR_VirtualKeysNotValidForMnemonicLayouts,
    `Virtual keys are not valid for mnemonic layouts`);
  static Warn_ExtendedShiftFlagsNotSupportedInKeymanWeb = (o:{flags:string}) => m(this.WARN_ExtendedShiftFlagsNotSupportedInKeymanWeb,
    `Extended shift flags ${o.flags} are not supported in KeymanWeb`);
  static Hint_UnreachableKeyCode = (o:{key:string}) => m(this.HINT_UnreachableKeyCode,
    `The rule will never be matched for key ${o.key} because its key code is never fired.`);
  static Error_NotSupportedInKeymanWebStore = (o:{code:string,store:string}) => m(this.ERROR_NotSupportedInKeymanWebStore,
    `${o.code} is not currently supported in store '${o.store}' when used by any or index`);
  static Error_NotSupportedInKeymanWebContext = (o:{code:String}) => m(this.ERROR_NotSupportedInKeymanWebContext,
    `Statement ${o.code} is not currently supported in context`);
  static Error_NotSupportedInKeymanWebOutput = (o:{code:string}) => m(this.ERROR_NotSupportedInKeymanWebOutput,
    `Statement ${o.code} is not currently supported in output`);

  // Following messages are kmc-kmw only, so use KmwCompiler error namespace
  static Error_NotAnyRequiresVersion14 = () => m(this.ERROR_NotAnyRequiresVersion14,
    `Statement notany in context() match requires version 14.0+ of KeymanWeb`);
  static ERROR_NotAnyRequiresVersion14 = SevError | 0x0001;
};
