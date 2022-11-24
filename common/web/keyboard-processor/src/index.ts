// This file exists as a bundling intermediary that attempts to present all of
// keyboard-processor's offerings in the 'old', namespaced format - at least,
// as of the time that this submodule was converted to ES6 module use.

import * as ActiveLayout from "keyboards/activeLayout.js";
import * as DefaultLayout from "keyboards/defaultLayouts.js";
import Keyboard, * as KeyboardContents from "keyboards/keyboard.js";

import Codes, * as CodesContents from "text/codes.js";
import * as Deadkeys from "text/deadkeys.js";
import DefaultOutput, * as DefaultOutputContents from "text/defaultOutput.js";
import KbdInterface, * as KbdInterfaceContents from "text/kbdInterface.js";
import KeyboardProcessor, * as KeyboardProcessorContents from "text/keyboardProcessor.js";
import KeyEvent from "text/keyEvent.js";
import KeyMapping from "text/keyMapping.js";
import OutputTarget, * as OutputTargetContents from "text/outputTarget.js";
import RuleBehavior from "text/ruleBehavior.js";
import * as SystemStores from "text/systemStores.js";

import * as utils from "utils/build/modules/index.js";

export let com = {
  keyman: {
    keyboards: {
      ...ActiveLayout,
      ...DefaultLayout,
      Keyboard, ...KeyboardContents
    },
    text: {
      Codes, ...CodesContents,
      ...Deadkeys,
      DefaultOutput, ...DefaultOutputContents,
      KbdInterface, ...KbdInterfaceContents,
      KeyboardProcessor, ...KeyboardProcessorContents,
      KeyEvent,
      KeyMapping,
      OutputTarget, ...OutputTargetContents,
      RuleBehavior,
      ...SystemStores
    },
    utils: {... utils}
  }
}

// A consequence of the spread-operator use + the modules with defaults.
delete com.keyman.keyboards.default;
delete com.keyman.text.default;

// Force-exports it as the global it always was.
utils.globalObject()['com'] = com;

export default com;
