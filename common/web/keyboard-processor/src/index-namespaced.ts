// This file exists as a bundling intermediary that attempts to present all of
// keyboard-processor's offerings in the 'old', namespaced format - at least,
// as of the time that this submodule was converted to ES6 module use.

// Unfortunately, the declaration-bundling tool that works well for the modules...
// struggles a bit here.

import { ActiveKey, ActiveRow, ActiveLayer, ActiveLayout } from "./keyboards/activeLayout.js";
import { Layouts } from "./keyboards/defaultLayouts.js";
import Keyboard, { LayoutState } from "./keyboards/keyboard.js";

import Codes from "./text/codes.js";
import { Deadkey, DeadkeyTracker} from "./text/deadkeys.js";
import DefaultOutput, { EmulationKeystrokes } from "./text/defaultOutput.js";
import KeyboardInterface, { KeyInformation, SystemStoreIDs } from "./text/kbdInterface.js";
import KeyboardProcessor from "./text/keyboardProcessor.js";
import KeyEvent from "./text/keyEvent.js";
import KeyMapping from "./text/keyMapping.js";
import OutputTarget, { TextTransform, Transcription, Mock } from "./text/outputTarget.js";
import RuleBehavior from "./text/ruleBehavior.js";
import { SystemStore, MutableSystemStore, PlatformSystemStore } from "./text/systemStores.js";

import { deepCopy, DeviceSpec, extendString, globalObject as getGlobalObject, Version } from "@keymanapp/web-utils/build/obj/index.js";

// DeviceSpec's merged declaration style isn't well-handled by the declaration bundler without this.
export { DeviceSpec } from "@keymanapp/web-utils/build/obj/index.js";

export let com = {
  keyman: {
    keyboards: {
      ActiveKey, ActiveRow, ActiveLayer, ActiveLayout,  // keyboards/activeLayouts.ts
      Layouts, // keyboards/defaultLayouts.ts
      Keyboard, LayoutState // keyboards/keyboard.ts
    },
    text: {
      Codes,
      Deadkey, DeadkeyTracker, // text/deadkeys.ts
      DefaultOutput, EmulationKeystrokes, // text/defaultOutput.ts
      KeyboardInterface, KeyInformation, SystemStoreIDs, // text/kbdInterface.ts
      KeyboardProcessor,
      KeyEvent,
      KeyMapping,
      OutputTarget, TextTransform, Transcription, Mock, // text/outputTarget.ts
      RuleBehavior,
      SystemStore, MutableSystemStore, PlatformSystemStore // text/systemStores.ts
    },
    utils: {
      deepCopy, DeviceSpec, extendString, getGlobalObject, Version
    }
  }
}

// Force-exports it as the global it always was.
getGlobalObject()['com'] = com;

export default com;
