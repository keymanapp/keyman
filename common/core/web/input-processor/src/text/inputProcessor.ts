// Defines a 'polyfill' of sorts for NPM's events module
/// <reference path="../includes/events.ts" />
/// <reference path="../../node_modules/@keymanapp/keyboard-processor/src/text/keyboardProcessor.ts" />
/// <reference path="contextWindow.ts" />
/// <reference path="prediction/languageProcessor.ts" />

namespace com.keyman.text {
  export class InputProcessor {
    public static readonly DEFAULT_OPTIONS: ProcessorInitOptions = {
      baseLayout: 'us'
    }

    private kbdProcessor: KeyboardProcessor;
    private lngProcessor: prediction.LanguageProcessor;

    constructor(options?: ProcessorInitOptions) {
      if(!options) {
        options = InputProcessor.DEFAULT_OPTIONS;
      }

      this.kbdProcessor = new KeyboardProcessor(options);
      this.lngProcessor = new prediction.LanguageProcessor();
    }

    public get languageProcessor(): prediction.LanguageProcessor {
      return this.lngProcessor;
    }

    public get keyboardProcessor(): KeyboardProcessor {
      return this.kbdProcessor;
    }

    public get keyboardInterface(): text.KeyboardInterface {
      return this.keyboardProcessor.keyboardInterface;
    }

    public get activeKeyboard(): keyboards.Keyboard {
      return this.keyboardInterface.activeKeyboard;
    }

    public set activeKeyboard(keyboard: keyboards.Keyboard) {
      this.keyboardInterface.activeKeyboard = keyboard;

      // All old deadkeys and keyboard-specific cache should immediately be invalidated
      // on a keyboard change.
      this.resetContext();
    }

    public get activeModel(): prediction.ModelSpec {
      return this.languageProcessor.activeModel;
    }

        /**
     * Simulate a keystroke according to the touched keyboard button element
     *
     * Handles default output and keyboard processing for both OSK and physical keystrokes.
     * 
     * @param       {Object}      keyEvent      The abstracted KeyEvent to use for keystroke processing
     * @param       {Object}      outputTarget  The OutputTarget receiving the KeyEvent
     * @returns     {Object}                    A RuleBehavior object describing the cumulative effects of
     *                                          all matched keyboard rules.
     */
    processKeyEvent(keyEvent: KeyEvent, outputTarget: OutputTarget): RuleBehavior {
      let formFactor = keyEvent.device.formFactor;
      let fromOSK = keyEvent.isSynthetic;

      // The default OSK layout for desktop devices does not include nextlayer info, relying on modifier detection here.
      // It's the OSK equivalent to doModifierPress on 'desktop' form factors.
      if((formFactor == utils.FormFactor.Desktop || !this.activeKeyboard || this.activeKeyboard.usesDesktopLayoutOnDevice(keyEvent.device)) && fromOSK) {
        // If it's a desktop OSK style and this triggers a layer change,
        // a modifier key was clicked.  No output expected, so it's safe to instantly exit.
        if(this.keyboardProcessor.selectLayer(keyEvent)) {
          return new RuleBehavior();
        }
      }

      // Will handle keystroke-based non-layer change modifier & state keys, mapping them through the physical keyboard's version
      // of state management.
      if(this.keyboardProcessor.doModifierPress(keyEvent, outputTarget, !fromOSK) && !fromOSK) {
        return new RuleBehavior();
      }

      // If suggestions exist AND space is pressed, accept the suggestion and do not process the keystroke.
      // If a suggestion was just accepted AND backspace is pressed, revert the change and do not process the backspace.
      // We check the first condition here, while the prediction UI handles the second through the try__() methods below.
      if(this.languageProcessor.isActive) {
        // The following code relies on JS's logical operator "short-circuit" properties to prevent unwanted triggering of the second condition.

        // Can the suggestion UI revert a recent suggestion?  If so, do that and swallow the backspace.
        if((keyEvent.kName == "K_BKSP" || keyEvent.Lcode == Codes.keyCodes["K_BKSP"]) && this.languageProcessor.tryRevertSuggestion()) {
          return new RuleBehavior();
          // Can the suggestion UI accept an existing suggestion?  If so, do that and swallow the space character.
        } else if((keyEvent.kName == "K_SPACE" || keyEvent.Lcode == Codes.keyCodes["K_SPACE"]) && this.languageProcessor.tryAcceptSuggestion('space')) {
          return new RuleBehavior();
        }
      }

      // // ...end I3363 (Build 301)

      // Create a "mock" backup of the current outputTarget in its pre-input state.
      // Current, long-existing assumption - it's DOM-backed.
      let preInputMock = Mock.from(outputTarget);

      // We presently need the true keystroke to run on the FULL context.  That index is still
      // needed for some indexing operations when comparing two different output targets.
      let ruleBehavior = this.keyboardProcessor.processKeystroke(keyEvent, outputTarget);

      // Swap layer as appropriate.
      if(keyEvent.kNextLayer) {
        this.keyboardProcessor.selectLayer(keyEvent);
      }
      
      // Should we swallow any further processing of keystroke events for this keydown-keypress sequence?
      if(ruleBehavior != null) {
        let alternates: Alternate[];

        // If we're performing a 'default command', it's not a standard 'typing' event - don't do fat-finger stuff.
        // Also, don't do fat-finger stuff if predictive text isn't enabled.
        if(this.languageProcessor.isActive && !ruleBehavior.triggersDefaultCommand) {
          let keyDistribution = keyEvent.keyDistribution;

          // We don't need to track absolute indexing during alternate-generation; 
          // only position-relative, so it's better to use a sliding window for context
          // when making alternates.  (Slightly worse for short text, matters greatly
          // for long text.)
          let contextWindow = new ContextWindow(preInputMock, ContextWindow.ENGINE_RULE_WINDOW);
          let windowedMock = contextWindow.toMock();

          // Note - we don't yet do fat-fingering with longpress keys.
          if(keyDistribution && keyEvent.kbdLayer) {
            // Tracks a 'deadline' for fat-finger ops, just in case both context is long enough
            // and device is slow enough that the calculation takes too long.
            //
            // Consider use of https://developer.mozilla.org/en-US/docs/Web/API/Performance/now instead?
            // Would allow finer-tuned control.
            let TIMEOUT_THRESHOLD: number = Number.MAX_VALUE;
            let _globalThis = com.keyman.utils.getGlobalObject();
            let timer: () => number;

            // Available by default on `window` in browsers, but _not_ on `global` in Node, 
            // surprisingly.  Since we can't use code dependent on `require` statements
            // at present, we have to condition upon it actually existing.
            if(_globalThis['performance'] && _globalThis['performance']['now']) {
              timer = function() {
                return _globalThis['performance']['now']();
              };

              TIMEOUT_THRESHOLD = timer() + 16; // + 16ms.
            } // else {
              // We _could_ just use Date.now() as a backup... but that (probably) only matters
              // when unit testing.  So... we actually don't _need_ time thresholding when in 
              // a Node environment.
            // }

            // Tracks a minimum probability for keystroke probability.  Anything less will not be
            // included in alternate calculations. 
            //
            // Seek to match SearchSpace.EDIT_DISTANCE_COST_SCALE from the predictive-text engine.
            // Reasoning for the selected value may be seen there.  Short version - keystrokes 
            // that _appear_ very precise may otherwise not even consider directly-neighboring keys.
            let KEYSTROKE_EPSILON = Math.exp(-5);

            // Sort the distribution into probability-descending order.
            keyDistribution.sort((a, b) => b.p - a.p);

            let activeLayout = this.activeKeyboard.layout(keyEvent.device.formFactor);
            alternates = [];
    
            let totalMass = 0; // Tracks sum of non-error probabilities.
            for(let pair of keyDistribution) {
              if(pair.p < KEYSTROKE_EPSILON) {
                break;
              } else if(timer && timer() >= TIMEOUT_THRESHOLD) {
                break;
              }

              let mock = Mock.from(windowedMock);
              
              let altKey = activeLayout.getLayer(keyEvent.kbdLayer).getKey(pair.keyId);
              if(!altKey) {
                console.warn("Potential fat-finger key could not be found in layer!");
                continue;
              }

              let altEvent = altKey.constructKeyEvent(this.keyboardProcessor, keyEvent.device);
              let alternateBehavior = this.keyboardProcessor.processKeystroke(altEvent, mock);
              
              // If alternateBehavior.beep == true, ignore it.  It's a disallowed key sequence,
              // so we expect users to never intend their use.
              if(alternateBehavior && !alternateBehavior.beep && pair.p > 0) {
                let transform: Transform = alternateBehavior.transcription.transform;
                
                // Ensure that the alternate's token id matches that of the current keystroke, as we only
                // record the matched rule's context (since they match)
                transform.id = ruleBehavior.transcription.token;
                alternates.push({sample: transform, 'p': pair.p});
                totalMass += pair.p;
              }
            }

            // Renormalizes the distribution, as any error (beep) results
            // will result in a distribution that doesn't sum to 1 otherwise.
            // All `.p` values are strictly positive, so totalMass is
            // guaranteed to be > 0 if the array has entries.
            alternates.forEach(function(alt) {
              alt.p /= totalMass;
            });
          }
        }

        // Now that we've done all the keystroke processing needed, ensure any extra effects triggered
        // by the actual keystroke occur.
        ruleBehavior.finalize(this.keyboardProcessor, outputTarget);

        // -- All keystroke (and 'alternate') processing is now complete.  Time to finalize everything! --
        
        // Notify the ModelManager of new input - it's predictive text time!
        ruleBehavior.transcription.alternates = alternates;
        // Yes, even for ruleBehavior.triggersDefaultCommand.  Those tend to change the context.
        ruleBehavior.predictionPromise = this.languageProcessor.predict(ruleBehavior.transcription);

        // Text did not change (thus, no text "input") if we tabbed or merely moved the caret.
        if(!ruleBehavior.triggersDefaultCommand) {
          // For DOM-aware targets, this will trigger a DOM event page designers may listen for.
          outputTarget.doInputEvent();
        }
      }

      return ruleBehavior;
    }

    public resetContext(outputTarget?: OutputTarget) {
      this.keyboardProcessor.resetContext();
      this.languageProcessor.invalidateContext(outputTarget);
    }
  }
}

(function () {
  let ns = com.keyman.text;

  // Let the InputProcessor be available both in the browser and in Node.
  if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    module.exports = ns.InputProcessor;
    //@ts-ignore
    ns.InputProcessor.com = com; // Export the root namespace so that all InputProcessor classes are accessible by unit tests.
  }
}());