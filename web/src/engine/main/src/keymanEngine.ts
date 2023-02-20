import { default as Configuration } from "keyman/build/engine/configuration/obj/configuration.js";
import { Keyboard, ManagedPromise, OutputTarget } from "@keymanapp/keyboard-processor";
import { InputProcessor, PredictionContext } from "@keymanapp/input-processor";
import { OSKView } from "keyman/build/engine/osk/obj/index.js";
import { StubAndKeyboardCache } from "keyman/build/engine/keyboard-cache/obj/index.js";

import ContextManager from "./contextManager.js";
import HardKeyboard from "./hardKeyboard.js";

export default /*abstract*/ class KeymanEngine {
  readonly config: Configuration;
  readonly cache: StubAndKeyboardCache = new StubAndKeyboardCache();
  readonly initPromise: ManagedPromise<void> = new ManagedPromise();

  constructor(config: Configuration) {
    this.config = config;

    this.cache.on('stubAdded', (stub) => {
      let eventRaiser = () => {
        // The corresponding event is needed in order to update UI modules as new keyboard stubs "come online".
        this.doKeyboardRegistered(stub.KI, stub.KL, stub.KN, stub.KLC, stub.KP);
      }

      if(this.initPromise.hasFinalized) {
        eventRaiser();
      } else {
        this.initPromise.then(eventRaiser);
      }
    });

    this.cache.on('keyboardAdded', (keyboard) => {
      let eventRaiser = () => {
        // Execute any external (UI) code needed after loading keyboard
        this.doKeyboardLoaded(keyboard.id);
      }

      if(this.initPromise.hasFinalized) {
        eventRaiser();
      } else {
        this.initPromise.then(eventRaiser);
      }
    });
  }

  initialize(): void {
    // There may be some valid mutations possible even on repeated calls?
    // The original seems to allow it.

    if(this.initPromise.hasFinalized) {
      // abort!
      return;
    }

    // Once initialization is fully done:
    this.initPromise.resolve();
  }
}

// Intent:  define common behaviors for both primary app types; each then subclasses & extends where needed.