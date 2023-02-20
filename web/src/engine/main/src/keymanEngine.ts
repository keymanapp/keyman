import { default as Configuration } from "keyman/build/engine/configuration/obj/configuration.js";
import { Keyboard, KeyboardKeymanGlobal, ManagedPromise, OutputTarget } from "@keymanapp/keyboard-processor";
import { DOMKeyboardLoader as KeyboardLoader } from "@keymanapp/keyboard-processor/build/obj/keyboards/loaders/dom-keyboard-loader.js";
import { InputProcessor, PredictionContext } from "@keymanapp/input-processor";
import { OSKView } from "keyman/build/engine/osk/obj/index.js";
import { StubAndKeyboardCache } from "keyman/build/engine/keyboard-cache/obj/index.js";

import KeyboardInterface from "./keyboardInterface.js";
import ContextManager from "./contextManager.js";
import HardKeyboard from "./hardKeyboard.js";

export default /*abstract*/ class KeymanEngine implements KeyboardKeymanGlobal {
  readonly config: Configuration;
  readonly cache: StubAndKeyboardCache = new StubAndKeyboardCache();
  readonly interface: KeyboardInterface;
  readonly initPromise: ManagedPromise<void> = new ManagedPromise();
  readonly keyboardLoader: KeyboardLoader;

  constructor(config: Configuration) {
    this.config = config;

    // Since we're not sandboxing keyboard loads yet, we just use `window` as the jsGlobal object.
    this.interface = new KeyboardInterface(window, this, this.cache);
    this.keyboardLoader = new KeyboardLoader(this.interface);

    this.cache.on('stubAdded', (stub) => {
      let eventRaiser = () => {
        // The corresponding event is needed in order to update UI modules as new keyboard stubs "come online".
        this.doKeyboardRegistered(stub.KI, stub.KL, stub.KN, stub.KLC, stub['KP']);
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

  public get osk(): OSKView {
    // TODO:
    return null;
  }

  private set osk(value: OSKView) {
    // TODO:
  }
}

// Intent:  define common behaviors for both primary app types; each then subclasses & extends where needed.