import ContextManager from "./contextManager.js";
import HardKeyboard from "./hardKeyboard.js";
import { default as Configuration } from "keyman/build/engine/configuration/obj/configuration.js";
import { OSKView } from "keyman/build/engine/osk/obj/index.js";
import { Keyboard, OutputTarget } from "@keymanapp/keyboard-processor";
import { InputProcessor, PredictionContext } from "@keymanapp/input-processor";

export default /*abstract*/ class KeymanEngine {
  readonly config: Configuration;

  constructor(config: Configuration) {
    this.config = config;
  }

  initialize(): void { }
}

// Intent:  define common behaviors for both primary app types; each then subclasses & extends where needed.