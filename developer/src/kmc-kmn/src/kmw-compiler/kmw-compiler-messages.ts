import { KmnCompilerMessages } from "../compiler/kmn-compiler-messages.js";
import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerEvent, CompilerMessageDef as def, CompilerMessageSpec } from "@keymanapp/common-types";
import { kmnfile } from "./compiler-globals.js";

const Namespace = CompilerErrorNamespace.KmwCompiler;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
// const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

const m = (code: number, message: string, o?: {filename?: string, line?: number}) : CompilerEvent => ({
  ...CompilerMessageSpec(code, message),
  filename: o?.filename ?? kmnfile,
  line: o?.line,
});

/**
 * @internal
 * Error messages reported by the KeymanWeb .kmn compiler.
 */
export class KmwCompilerMessages extends KmnCompilerMessages {
  // Note: for legacy reasons, KMWCompilerMessages extends from
  // KMNCompilerMessages as they share the same error codes. This can be a
  // little confusing because kmcmplib still builds its own error message
  // strings, not kmc-kmn, whereas the kmw messages are defined here. However,
  // as the kmw module may be going away at some point, it's probably not worth
  // the splitting of all KMW-specific error messages out of the
  // KmnCompilerMessages space.

  // Following messages are kmw-compiler only, so use KmwCompiler error namespace

  // 0x0001 - Reserved for removed identifier ERROR_NotAnyRequiresVersion14 =
  //          SevError | 0x0001, added in 17.0, removed in 18.0

  static ERROR_TouchLayoutIdentifierRequires15 = SevError | 0x0002;
  static Error_TouchLayoutIdentifierRequires15 = (o:{keyId:string, platformName:string, layerId:string}) => m(this.ERROR_TouchLayoutIdentifierRequires15,
    `Key "${def(o.keyId)}" on "${def(o.platformName)}", layer "${def(o.layerId)}" has a multi-part identifier which requires version 15.0 or newer.`);

  static ERROR_InvalidTouchLayoutFileFormat = SevError | 0x0003;
  static Error_InvalidTouchLayoutFileFormat = (o:{msg: string}) => m(this.ERROR_InvalidTouchLayoutFileFormat,
    `Invalid touch layout file: ${def(o.msg)}`);

  static ERROR_TouchLayoutFileDoesNotExist = SevError | 0x0004;
  static Error_TouchLayoutFileDoesNotExist = (o:{filename:string}) => m(this.ERROR_TouchLayoutFileDoesNotExist,
    `Touch layout file ${def(o.filename)} does not exist`);

  static HINT_TouchLayoutUsesUnsupportedGesturesDownlevel = SevHint | 0x0005;
  static Hint_TouchLayoutUsesUnsupportedGesturesDownlevel = (o:{keyId:string}) => m(this.HINT_TouchLayoutUsesUnsupportedGesturesDownlevel,
    `The touch layout uses a flick or multi-tap gesture on key ${def(o.keyId)}, which is only available on version 17.0+ of Keyman`);

  static INFO_MinimumWebEngineVersion = SevInfo | 0x0006;
  static Info_MinimumWebEngineVersion = (o:{version:string}) => m(
    this.INFO_MinimumWebEngineVersion,
    `The compiler has assigned a minimum web engine version of ${o.version} based on features used in this keyboard`
  );
};
