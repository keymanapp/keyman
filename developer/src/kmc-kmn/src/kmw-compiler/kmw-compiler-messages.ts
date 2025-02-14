import { keyAddress, KmnCompilerMessages } from "../compiler/kmn-compiler-messages.js";
import { CompilerErrorNamespace, CompilerErrorSeverity, CompilerMessageDef as def, CompilerMessageSpec as m } from "@keymanapp/developer-utils";
import { KeyAddress } from "./validate-layout-file.js";

const Namespace = CompilerErrorNamespace.KmwCompiler;
const SevInfo = CompilerErrorSeverity.Info | Namespace;
const SevHint = CompilerErrorSeverity.Hint | Namespace;
// const SevWarn = CompilerErrorSeverity.Warn | Namespace;
const SevError = CompilerErrorSeverity.Error | Namespace;
// const SevFatal = CompilerErrorSeverity.Fatal | Namespace;

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

  //------------------------------------------------------------------------------|
  // max length of detail message lines (checked by verifyCompilerMessagesObject) |
  //------------------------------------------------------------------------------|

  static ERROR_TouchLayoutIdentifierRequires15 = SevError | 0x0002;
  static Error_TouchLayoutIdentifierRequires15 = (o:{keyId:string, platformName:string, layerId:string, address:KeyAddress}) => m(
    this.ERROR_TouchLayoutIdentifierRequires15,
    `Key "${def(o.keyId)}" on "${def(o.platformName)}", layer "${def(o.layerId)}" (${keyAddress(o.address)}) has a `+
    `multi-part identifier which requires version 15.0 or newer`, `
    The Unicode key format \`U_xxxx_yyyy\` is supported in Keyman 15.0 and later
    versions.

    For example, \`U_0041_0300\` means the key will by default emit
    \`U+0041 U+0300\` (À); earlier versions allow only a single Unicode value in
    the identifier, e.g. \`U_0300\`.
  `);

  static ERROR_InvalidTouchLayoutFileFormat = SevError | 0x0003;
  static Error_InvalidTouchLayoutFileFormat = (o:{msg: string}) => m(
    this.ERROR_InvalidTouchLayoutFileFormat,
    `Invalid touch layout file: ${def(o.msg)}`, `
    The referenced .keyman-touch-layout file contained invalid JSON content. The
    touch layout file format is documented at
    https://help.keyman.com/developer/current-version/reference/file-types/keyman-touch-layout.
  `);

  static ERROR_TouchLayoutFileDoesNotExist = SevError | 0x0004;
  static Error_TouchLayoutFileDoesNotExist = (o:{filename:string}) => m(
    this.ERROR_TouchLayoutFileDoesNotExist,
    `Touch layout file ${def(o.filename)} does not exist`, `
    The compiler was unable to load the referenced .keyman-touch-layout file.
    Verify that the referenced file does exist and is accessible to the compiler.
  `);

  static HINT_TouchLayoutUsesUnsupportedGesturesDownlevel = SevHint | 0x0005;
  static Hint_TouchLayoutUsesUnsupportedGesturesDownlevel = (o:{keyId:string}) => m(
    this.HINT_TouchLayoutUsesUnsupportedGesturesDownlevel,
    `The touch layout uses a flick or multi-tap gesture on key ${def(o.keyId)}, which `+
    `is only available on version 17.0+ of Keyman`, `
    Flick and multi-tap gesture support was added to Keyman mobile platforms in
    version 17.0. Keyboards which include these gestures can still work in earlier
    versions of Keyman, but any flick and multi-tap gestures will not be available,
    which may make some characters inaccesssible.
  `);

  static INFO_MinimumWebEngineVersion = SevInfo | 0x0006;
  static Info_MinimumWebEngineVersion = (o:{version:string}) => m(
    this.INFO_MinimumWebEngineVersion,
    `The compiler has assigned a minimum web engine version of ${o.version} based on `+
    `features used in this keyboard`, `
    If the [\`&version\` store](https://help.keyman.com/developer/language/reference/version)
    is not present in the keyboard source, the compiler attempts to assign the
    lowest possible Keyman version that can support the features found in the
    keyboard. Details on the history of language features and supported versions
    can be found at https://help.keyman.com/developer/language/guide/history.
  `);

  //------------------------------------------------------------------------------|
};
