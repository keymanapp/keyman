export * from "./keyboards/activeLayout.js";
export * from "./keyboards/defaultLayouts.js";
export { default as Keyboard } from "./keyboards/keyboard.js";
export * from "./keyboards/keyboard.js";
export { KeyboardHarness, KeyboardKeymanGlobal, MinimalCodesInterface, MinimalKeymanGlobal } from "./keyboards/keyboardHarness.js";
export {
  default as KeyboardLoaderBase,
  KeyboardLoadErrorBuilder,
  KeyboardMissingError,
  KeyboardScriptError
} from "./keyboards/keyboardLoaderBase.js";
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

export * from "./text/deadkeys.js";
export { default as DefaultRules } from "./text/defaultRules.js";
export * from "./text/defaultRules.js";
export { default as KeyboardInterface } from "./text/kbdInterface.js";
export * from "./text/kbdInterface.js";
export { default as KeyboardProcessor } from "./text/keyboardProcessor.js";
export * from "./text/keyboardProcessor.js";
export { default as KeyEvent } from "./text/keyEvent.js";
export * from "./text/keyEvent.js";
export { default as KeyMapping } from "./text/keyMapping.js";
export { default as OutputTarget } from "./text/outputTarget.js";
export * from "./text/outputTarget.js";
export { default as RuleBehavior } from "./text/ruleBehavior.js";
export * from "./text/stringDivergence.js";
export * from "./text/systemStores.js";

export * from "@keymanapp/web-utils";

// At the top level, there should be no default export.

// Without the line below... OutputTarget would likely be aliased there, as it's
// the last `export { default as _ }` => `export * from` pairing seen above.
export default undefined;