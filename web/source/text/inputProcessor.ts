// Defines a 'polyfill' of sorts for NPM's events module
/// <reference path="../includes/events.ts" />
/// <reference path="../includes/lmlayer.ts" />
/// <reference path="../includes/lmMsgs.d.ts" />
/// <reference path="keyboardProcessor.ts" />
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
     * @param       {Object}      e      The abstracted KeyEvent to use for keystroke processing
     */
    processKeyEvent(keyEvent: KeyEvent) {
      let formFactor = keyEvent.device.formFactor;

      // Determine the current target for text output and create a "mock" backup
      // of its current, pre-input state.  Current, long-existing assumption - it's DOM-backed.
      let outputTarget = keyEvent.Ltarg;
      let fromOSK = keyEvent.isSynthetic;

      // The default OSK layout for desktop devices does not include nextlayer info, relying on modifier detection here.
      // It's the OSK equivalent to doModifierPress on 'desktop' form factors.
      if((formFactor == FormFactor.Desktop || !this.activeKeyboard || this.activeKeyboard.usesDesktopLayoutOnDevice(keyEvent.device)) && fromOSK) {
        // If it's a desktop OSK style and this triggers a layer change,
        // a modifier key was clicked.  No output expected, so it's safe to instantly exit.
        if(this.keyboardProcessor.selectLayer(keyEvent)) {
          return true;
        }
      }

      // Will handle keystroke-based non-layer change modifier & state keys, mapping them through the physical keyboard's version
      // of state management.
      if(!fromOSK && this.keyboardProcessor.doModifierPress(keyEvent, !fromOSK)) {
        return true;
      }

      // If suggestions exist AND space is pressed, accept the suggestion and do not process the keystroke.
      // If a suggestion was just accepted AND backspace is pressed, revert the change and do not process the backspace.
      // We check the first condition here, while the prediction UI handles the second through the try__() methods below.
      if(this.languageProcessor.isActive) {
        // The following code relies on JS's logical operator "short-circuit" properties to prevent unwanted triggering of the second condition.

        // Can the suggestion UI revert a recent suggestion?  If so, do that and swallow the backspace.
        if((keyEvent.kName == "K_BKSP" || keyEvent.Lcode == Codes.keyCodes["K_BKSP"]) && this.languageProcessor.tryRevertSuggestion()) {
          return;
          // Can the suggestion UI accept an existing suggestion?  If so, do that and swallow the space character.
        } else if((keyEvent.kName == "K_SPACE" || keyEvent.Lcode == Codes.keyCodes["K_SPACE"]) && this.languageProcessor.tryAcceptSuggestion('space')) {
          return;
        }
      }

      // // ...end I3363 (Build 301)

      let preInputMock = Mock.from(outputTarget);
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
          // Note - we don't yet do fat-fingering with longpress keys.
          if(keyEvent.keyDistribution && keyEvent.kbdLayer) {
            let activeLayout = this.activeKeyboard.layout(keyEvent.device.formFactor);
            alternates = [];
    
            for(let pair of keyEvent.keyDistribution) {
              let mock = Mock.from(preInputMock);
              
              let altKey = activeLayout.getLayer(keyEvent.kbdLayer).getKey(pair.keyId);
              if(!altKey) {
                console.warn("Potential fat-finger key could not be found in layer!");
                continue;
              }

              let altEvent = altKey.constructKeyEvent(this.keyboardProcessor, mock, keyEvent.device);
              let alternateBehavior = this.keyboardProcessor.processKeystroke(altEvent, mock);
              if(alternateBehavior) {
                // TODO: if alternateBehavior.beep == true, set 'p' to 0.  It's a disallowed key sequence,
                //       so a user should never have intended to type it.  Should probably renormalize 
                //       the distribution afterward, though...
                
                let transform: Transform = alternateBehavior.transcription.transform;
                
                // Ensure that the alternate's token id matches that of the current keystroke, as we only
                // record the matched rule's context (since they match)
                transform.id = ruleBehavior.transcription.token;
                alternates.push({sample: transform, 'p': pair.p});
              }
            }
          }
        }

        // Now that we've done all the keystroke processing needed, ensure any extra effects triggered
        // by the actual keystroke occur.
        ruleBehavior.finalize(this.keyboardProcessor);

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

      /* I732 END - 13/03/2007 MCD: End Positional Layout support in OSK */

      // TODO:  rework the return value to be `ruleBehavior` instead.  Functions that call this one are
      //        the ones that should worry about event handler returns, etc.  Not this one.
      //
      //        They should also be the ones to handle the TODOs seen earlier in this function -
      //        once THOSE are properly relocated.  (They're too DOM-heavy to remain in web-core.)

      // Only return true (for the eventual event handler's return value) if we didn't match a rule.
      return ruleBehavior;
    }

    public resetContext() {
      this.keyboardProcessor.resetContext();
      this.languageProcessor.invalidateContext();
    }
  }
}