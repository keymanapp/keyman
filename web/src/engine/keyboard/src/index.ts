export * from "./keyboards/activeLayout.js";
export * from "./keyboards/defaultLayouts.js";
export { default as JSKeyboard } from "./keyboards/keyboard.js";
export * from "./keyboards/keyboard.js";
export { KeyboardHarness, KeyboardKeymanGlobal, MinimalCodesInterface, MinimalKeymanGlobal } from "./keyboards/keyboardHarness.js";
export { KeyboardLoaderBase } from "./keyboards/keyboardLoaderBase.js";
export { KeyboardLoadErrorBuilder, KeyboardMissingError, KeyboardScriptError, KeyboardDownloadError, InvalidKeyboardError } from './keyboards/keyboardLoadError.js'
export {
  CloudKeyboardFont,
  internalizeFont,
  InternalKeyboardFont,
  KeyboardAPIPropertyMultilangSpec,
  KeyboardAPIPropertySpec,
  KeyboardInternalPropertySpec,
  default as KeyboardProperties,
  KeyboardFont,
  MetadataObj as RawKeyboardMetadata,
  LanguageAPIPropertySpec
} from "./keyboards/keyboardProperties.js";
export { default as SpacebarText } from "./keyboards/spacebarText.js";
export { default as StateKeyMap } from "./keyboards/stateKeyMap.js";

export { default as Codes } from "./codes.js";
export * from "./codes.js";
export { default as DefaultRules } from "./defaultRules.js";
export * from "./defaultRules.js";
export { default as KeyEvent } from "./keyEvent.js";
export * from "./keyEvent.js";
export { default as KeyMapping } from "./keyMapping.js";
export { OutputTarget } from "./outputTarget.interface.js";

export * from "@keymanapp/web-utils";

// At the top level, there should be no default export.

// Without the line below... OutputTarget would likely be aliased there, as it's
// the last `export { default as _ }` => `export * from` pairing seen above.
export default undefined;
