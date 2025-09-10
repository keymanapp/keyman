// Defines a 'polyfill' of sorts for NPM's events module

import ContextWindow from "./contextWindow.js";
import { LanguageProcessor }  from "./languageProcessor.js";
import type { ModelSpec, PredictionContext }  from "keyman/engine/interfaces";
import { globalObject, DeviceSpec } from "@keymanapp/web-utils";

import { Codes, type Keyboard, type KeyEvent } from "keyman/engine/keyboard";
import {
  type Alternate,
  isEmptyTransform,
  KeyboardInterface,
  KeyboardProcessor,
  Mock,
  type OutputTarget,
  RuleBehavior,
  type ProcessorInitOptions,
  SystemStoreIDs,
  TextTransform
} from 'keyman/engine/js-processor';

import { TranscriptionCache } from "./transcriptionCache.js";
import { LexicalModelTypes } from '@keymanapp/common-types';
import { WorkerFactory } from "@keymanapp/lexical-model-layer";

// Only consider raw-insertion transforms.  Delete-left and delete-right disqualify an
// incoming transform from reverting post-suggestion whitespace (or similar).
const transformMatchesPattern = (transform: TextTransform, breakingMarks: string[]) => {
  if(transform.deleteLeft || transform.deleteRight) {
    return false;
  }

  return breakingMarks.find((pattern) => transform.insert == pattern);
}

export class InputProcessor {
  public static readonly DEFAULT_OPTIONS: ProcessorInitOptions = {
    baseLayout: 'us'
  }

  /**
   * Indicates the device (platform) to be used for non-keystroke events,
   * such as those sent to `begin postkeystroke` and `begin newcontext`
   * entry points.
   */
  private contextDevice: DeviceSpec;
  private kbdProcessor: KeyboardProcessor;
  private lngProcessor: LanguageProcessor;

  private readonly contextCache = new TranscriptionCache();

  constructor(device: DeviceSpec, predictiveWorkerFactory: WorkerFactory, options?: ProcessorInitOptions) {
    if(!device) {
      throw new Error('device must be defined');
    }

    if(!options) {
      options = InputProcessor.DEFAULT_OPTIONS;
    }

    this.contextDevice = device;
    this.kbdProcessor = new KeyboardProcessor(device, options);
    this.lngProcessor = new LanguageProcessor(predictiveWorkerFactory, this.contextCache);
  }

  public get languageProcessor(): LanguageProcessor {
    return this.lngProcessor;
  }

  public get keyboardProcessor(): KeyboardProcessor {
    return this.kbdProcessor;
  }

  public get keyboardInterface(): KeyboardInterface {
    return this.keyboardProcessor.keyboardInterface;
  }

  public get activeKeyboard(): Keyboard {
    return this.keyboardInterface.activeKeyboard;
  }

  public set activeKeyboard(keyboard: Keyboard) {
    this.keyboardInterface.activeKeyboard = keyboard;

    // All old deadkeys and keyboard-specific cache should immediately be invalidated
    // on a keyboard change.
    this.resetContext();
  }

  public get activeModel(): ModelSpec {
    return this.languageProcessor.activeModel;
  }

  /**
   * Simulate a keystroke according to the touched keyboard button element
   *
   * Handles default output and keyboard processing for both OSK and physical keystrokes.
   *
   * @param       {Object}      keyEvent      The abstracted KeyEvent to use for keystroke processing
   * @param       {Object}      outputTarget  The OutputTarget receiving the KeyEvent
   * @param       {Object}      predictionContext  Context for the state of all predictive-text
   *                                               interactions associated with `outputTarget`
   * @returns     {Object}                    A RuleBehavior object describing the cumulative effects of
   *                                          all matched keyboard rules.
   */
  processKeyEvent(keyEvent: KeyEvent, outputTarget: OutputTarget, predictionContext: PredictionContext): RuleBehavior {
    const kbdMismatch = keyEvent.srcKeyboard && this.activeKeyboard != keyEvent.srcKeyboard;
    const trueActiveKeyboard = this.activeKeyboard;

    try {
      if(kbdMismatch) {
        // Avoid force-reset of context per our setter above.
        this.keyboardInterface.activeKeyboard = keyEvent.srcKeyboard;
      }

      // Support for multitap context reversion; multitap keys should act as if they were
      // the first thing typed since `preInput`, the state before the original base key.
      if(keyEvent.baseTranscriptionToken) {
        const transcription = this.contextCache.get(keyEvent.baseTranscriptionToken);
        if(transcription) {
          this.contextCache.rewindTo(keyEvent.baseTranscriptionToken);
          // Has there been a context change at any point during the multitap?  If so, we need
          // to revert it.  If not, we assume it's a layer-change multitap, in which case
          // no such reset is needed.
          if(!isEmptyTransform(transcription.transform) || !transcription.preInput.isEqual(Mock.from(outputTarget))) {
            // Restores full context, including deadkeys in their exact pre-keystroke state.
            outputTarget.restoreTo(transcription.preInput);
          }
          /*
            else:
            1. We don't need to restore the original context, as it's already
               in-place.
            2. Restoring anyway would obliterate any selected text, which is bad
               if this is a purely-layer-switching multitap.  (#11230)
          */
        } else {
          console.warn('The base context for the multitap could not be found!\n\n' + this.contextCache.buildLog());
          // would be lovely to report a desire for gesture debug output
          // maybe add something to RuleBehavior?
        }
      }

      return this._processKeyEvent(keyEvent, outputTarget, predictionContext);
    } finally {
      if(kbdMismatch) {
        // Restore our "current" activeKeyboard to its setting before the mismatching KeyEvent.
        this.keyboardInterface.activeKeyboard = trueActiveKeyboard;
      }
    }
  }

  /**
   * Acts as the core of `processKeyEvent` once we're comfortable asserting that the incoming
   * keystroke matches the current `activeKeyboard`.
   * @param keyEvent
   * @param outputTarget
   * @returns
   */
  private _processKeyEvent(keyEvent: KeyEvent, outputTarget: OutputTarget, predictionContext: PredictionContext): RuleBehavior {
    const formFactor = keyEvent.device.formFactor;
    const fromOSK = keyEvent.isSynthetic;

    // The default OSK layout for desktop devices does not include nextlayer info, relying on modifier detection here.
    // It's the OSK equivalent to doModifierPress on 'desktop' form factors.
    if((formFactor == DeviceSpec.FormFactor.Desktop || !this.activeKeyboard || this.activeKeyboard.usesDesktopLayoutOnDevice(keyEvent.device)) && fromOSK) {
      // If it's a desktop OSK style and this triggers a layer change,
      // a modifier key was clicked.  No output expected, so it's safe to instantly exit.
      if(this.keyboardProcessor.selectLayer(keyEvent)) {
        return new RuleBehavior();
      }
    }

    // Will handle keystroke-based non-layer change modifier & state keys, mapping them through the physical keyboard's version
    // of state management.  `doModifierPress` must always run.
    if(this.keyboardProcessor.doModifierPress(keyEvent, outputTarget, !fromOSK)) {
      // If run on a desktop platform, we know that modifier & state key presses may not
      // produce output, so we may make an immediate return safely.
      if(!fromOSK) {
        return new RuleBehavior();
      }
    }

    // // ...end I3363 (Build 301)

    // Create a "mock" backup of the current outputTarget in its pre-input state.
    // Current, long-existing assumption - it's DOM-backed.
    const preInputMock = Mock.from(outputTarget, true);

    const startingLayerId = this.keyboardProcessor.layerId;

    // We presently need the true keystroke to run on the FULL context.  That index is still
    // needed for some indexing operations when comparing two different output targets.
    let ruleBehavior = this.keyboardProcessor.processKeystroke(keyEvent, outputTarget);

    // Check to see if the incoming keystroke should revert the appended component of an
    // immediately-preceding applied Suggestion.
    const autoBreakingBehavior = this.doPredictiveAutoBreaking(ruleBehavior, outputTarget, predictionContext);
    ruleBehavior = autoBreakingBehavior ?? ruleBehavior;

    // Swap layer as appropriate.
    if(keyEvent.kNextLayer) {
      this.keyboardProcessor.selectLayer(keyEvent);
    }

    // If it's a key that we 'optimize out' of our fat-finger correction algorithm,
    // we MUST NOT trigger it for this keystroke.
    let isOnlyLayerSwitchKey = Codes.isFrameKey(keyEvent.kName);

    // Best-guess stopgap for possible custom modifier keys.
    // If a key (1) does not affect the context and (2) shifts the active layer,
    // we assume it's a modifier key.  (Touch keyboards may define custom modifier keys.)
    //
    // Note:  this will mean we won't generate alternates in the niche scenario where:
    // 1.  Keypress does not alter the actual context
    // 2.  It DOES emit a deadkey with an earlier processing rule.
    // 3.  The FINAL processing rule does not match.
    // 4.  The key ALSO signals a layer shift.
    // If any of the four above conditions aren't met - no problem!
    // So it's a pretty niche scenario.

    if(isEmptyTransform(ruleBehavior?.transcription?.transform) && keyEvent.kNextLayer) {
      isOnlyLayerSwitchKey = true;
    }

    const haveValidKeyRuleBehavior = ruleBehavior != null;
    // Should we swallow any further processing of keystroke events for this keydown-keypress sequence?
    if(haveValidKeyRuleBehavior) {
      // alternates are our fat-finger alternate outputs. We don't build these for keys we detect as
      // layer switch keys
      const alternates = isOnlyLayerSwitchKey ? null : this.buildAlternates(ruleBehavior, keyEvent, preInputMock);

      // Now that we've done all the keystroke processing needed, ensure any extra effects triggered
      // by the actual keystroke occur.
      ruleBehavior.finalize(this.keyboardProcessor, outputTarget, false);

      // -- All keystroke (and 'alternate') processing is now complete.  Time to finalize everything! --

      // Notify the ModelManager of new input - it's predictive text time!
      if(alternates && alternates.length > 0) {
        ruleBehavior.transcription.alternates = alternates;
      }
    } else {
      // We need a dummy RuleBehavior for keys which have no output (e.g. Shift)
      ruleBehavior = new RuleBehavior();
      ruleBehavior.transcription = outputTarget.buildTranscriptionFrom(outputTarget, null, false);
      ruleBehavior.triggersDefaultCommand = true;
    }

    // Multitaps operate in part by referencing 'committed' Transcriptions to rewind
    // the context as necessary.
    this.contextCache.save(ruleBehavior.transcription); //

    // The keyboard may want to take an action after all other keystroke processing is
    // finished, for example to switch layers. This action may not have any output
    // but may change system store or variable store values. Given this, we don't need to
    // save anything about the post behavior, after finalizing it

    // We need to tell the keyboard if the layer has been changed, either by a keyboard rule itself,
    // or by the touch layout 'nextlayer' control.
    const hasLayerChanged = ruleBehavior.setStore[SystemStoreIDs.TSS_LAYER] || keyEvent.kNextLayer;
    this.keyboardProcessor.newLayerStore.set(hasLayerChanged ? this.keyboardProcessor.layerId : '');
    this.keyboardProcessor.oldLayerStore.set(hasLayerChanged ? startingLayerId : '');

    const postRuleBehavior = this.keyboardProcessor.processPostKeystroke(this.contextDevice, outputTarget);
    if(postRuleBehavior) {
      postRuleBehavior.finalize(this.keyboardProcessor, outputTarget, true);
    }

    // Yes, even for ruleBehavior.triggersDefaultCommand.  Those tend to change the context.
    ruleBehavior.predictionPromise = this.languageProcessor.predict(ruleBehavior.transcription, this.keyboardProcessor.layerId);

    if(autoBreakingBehavior) {
      // We need a separate prediction request for this case; we don't want to
      // replace the results of a appended-transform that auto-accepted a
      // suggestion.  New suggestions should be done from the current state,
      // without affecting the results of this keystroke.
      this.languageProcessor.predictFromTarget(outputTarget, this.keyboardProcessor.layerId);
    }

    // Text did not change (thus, no text "input") if we tabbed or merely moved the caret.
    if(!ruleBehavior.triggersDefaultCommand) {
      // For DOM-aware targets, this will trigger a DOM event page designers may listen for.
      outputTarget.doInputEvent();
    }

    return haveValidKeyRuleBehavior ? ruleBehavior : null;
  }

  /**
   * Implements two separate-but-related predictive-text behaviors:
   *
   * 1.  When a suggestion is manually applied and then followed by input of a
   * model-defined "wordbreaking mark", any usual appended (usually, whitespace)
   * transform will be reverted and replaced with the "wordbreaking mark".
   *
   * 2.  When a suggestion is auto-selected and the user inputs a "wordbreaking
   * mark", the suggestion will be automatically applied, with its appended
   * transform replaced by the "wordbreaking mark" input.  (I.e., it will
   * trigger auto-correct behaviors.)
   *
   * @param ruleBehavior  The rule behavior from the keyboard for the incoming keystroke
   * @param outputTarget  The context source affected by the incoming keystroke
   * @param predictionContext  The "prediction context" corresponding to the context source.
   * @returns If an auto-correct behavior is triggered, returns a new `RuleBehavior` instance
   * for the keystroke (as the location of its application may change).  If no autocorrect
   * behaviors are triggered, returns `null`.
   */
  private doPredictiveAutoBreaking(
    ruleBehavior: RuleBehavior,
    outputTarget: OutputTarget,
    predictionContext: PredictionContext
  ): RuleBehavior {
    // If there's no prediction-context instance or predictive-text available, this function is a no-op.
    if(!predictionContext || !this.languageProcessor.isActive) {
      return null;
    }

    // Do we have an active model that appends wordbreaking transforms to suggestions...
    // and might it support reverting them or auto-applying them?  Finally, does the
    // incoming Transform match a mark that would activate this behavior?
    const breakingMarks = this.languageProcessor.wordbreaksAfterSuggestions?.breakingMarks;
    const ruleTransform = ruleBehavior.transcription.transform;
    if(!breakingMarks || !transformMatchesPattern(ruleTransform, breakingMarks)) {
      return null;
    }

    const keyEvent = ruleBehavior.transcription.keystroke;

    // ...and is this immediately after a Suggestion with an appended Transform was applied?
    // (If not, don't consider reverting an appended transform.)
    if(predictionContext.immediateReversion?.appendedTransform && predictionContext.recentAcceptCause == 'banner') {
      const reversion = predictionContext.immediateReversion;
      // For reversions, the appended transform exists (if it did for the applied suggestion)
      // and has an ID set to the appended transform from the suggestion.
      const base = this.contextCache.get(reversion.appendedTransform.id);
      const postRevertBehavior = this.keyboardProcessor.processKeystroke(keyEvent, Mock.from(base.preInput));
      const postRevertTransform = postRevertBehavior.transcription.transform;

      // Does the rule produce the same text & still match one of the breaking-mark patterns?
      // (with no deleteLeft, etc)
      if(postRevertTransform.insert == ruleTransform.insert && transformMatchesPattern(postRevertTransform, breakingMarks)) {
        // Then auto-revert the appended transform...
        const targetContext = Mock.from(base.preInput);
        targetContext.apply(postRevertBehavior.transcription.transform);

        // Update the worker's context!  (Which also wants to revert internally, so do it before
        // applying the final editing transform to the true output target!)
        this.languageProcessor.applyReversion(reversion, outputTarget, true);

        // ... & use the new result as the final form of the context
        const transform = targetContext.buildTransformFrom(outputTarget);
        outputTarget.apply(transform);

        // And overwrite the rule behavior transform with the form from the reverted context.
        return postRevertBehavior;
      }
    }

    // ... if we're not reverting appended whitespace... might we be in a position
    // to apply an auto-selected suggestion instead?
    //
    // Don't actually do it if it's a keep suggestion, though.  No need for
    // the extra noise.
    if(predictionContext?.selected && predictionContext.selected != predictionContext.keepSuggestion) {
      // At this stage, merely "post-key".
      const postApplyContext = Mock.from(ruleBehavior.transcription.preInput);
      // And now it's post-application.
      postApplyContext.apply(predictionContext.selected.transform);

      // Do we get the same result if the key is typed in the post-application context?
      const postApplyBehavior = this.keyboardProcessor.processKeystroke(keyEvent, Mock.from(postApplyContext));
      const postApplyTransform = postApplyBehavior.transcription.transform;

      // Does the rule produce the same text & still match one of the breaking-mark patterns?
      // (with no deleteLeft, etc)
      //
      // If so, since it behaves the same in either case, and it's a known word-breaking mark,
      // let's apply the selected suggestion automatically.
      if(postApplyTransform.insert == ruleTransform.insert && transformMatchesPattern(postApplyTransform, breakingMarks)) {
        const baseTransition = this.contextCache.get(predictionContext.selected.transformId);

        // Somehow, the base state is out of context - abort!
        if(!baseTransition) {
          return null;
        }

        // First, we revert to the latest prior pre-input transform - that's what suggestions
        // are based from.  This also erases the keystroke output.
        this.contextCache.rewindTo(baseTransition.token);
        outputTarget.restoreTo(baseTransition.preInput);

        // Erase the appended transform AND replace it with the input keystroke;
        // the input should take its place automatically, and the worker needs
        // to know this.
        //
        // This pipeline will also automatically output the keystroke's transform in
        // the correct location.
        predictionContext.selected.appendedTransform = ruleBehavior.transcription.transform;

        // This call will also produce the corrected RuleBehavior for us.
        const results = predictionContext.accept(predictionContext.selected, ruleBehavior);

        if(!results) {
          console.error("Breaking-mark auto-applied suggestion did not build expected RuleBehavior!");

          const appliedAppendedTranscription = this.contextCache.get(ruleBehavior.transcription.token);

          // Rebuild the rule behavior; preInput needs a rewrite, but it's readonly!
          // This version does NOT have a matching transition ID to what the actual
          // suggestion application built, so it's suboptimal.
          ruleBehavior.transcription = outputTarget.buildTranscriptionFrom(
            appliedAppendedTranscription.preInput,
            ruleBehavior.transcription.keystroke,
            false,
            ruleBehavior.transcription.alternates
          );
        }

        return results ?? ruleBehavior;
      }
    }

    return null;
  }

  private buildAlternates(ruleBehavior: RuleBehavior, keyEvent: KeyEvent, preInputMock: Mock): Alternate[] {
    let alternates: Alternate[];

    // If we're performing a 'default command', it's not a standard 'typing' event - don't do fat-finger stuff.
    // Also, don't do fat-finger stuff if predictive text isn't enabled.
    if(this.languageProcessor.isActive && !ruleBehavior.triggersDefaultCommand) {
      const keyDistribution = keyEvent.keyDistribution;

      // We don't need to track absolute indexing during alternate-generation;
      // only position-relative, so it's better to use a sliding window for context
      // when making alternates.  (Slightly worse for short text, matters greatly
      // for long text.)
      const contextWindow = new ContextWindow(preInputMock, ContextWindow.ENGINE_RULE_WINDOW, this.keyboardProcessor.layerId);
      const windowedMock = contextWindow.toMock();

      // Note - we don't yet do fat-fingering with longpress keys.
      if(this.languageProcessor.isActive && keyDistribution && keyEvent.kbdLayer) {
        // Tracks a 'deadline' for fat-finger ops, just in case both context is long enough
        // and device is slow enough that the calculation takes too long.
        //
        // Consider use of https://developer.mozilla.org/en-US/docs/Web/API/Performance/now instead?
        // Would allow finer-tuned control.
        let TIMEOUT_THRESHOLD: number = Number.MAX_VALUE;
        const _globalThis = globalObject();
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
        const KEYSTROKE_EPSILON = Math.exp(-5);

        alternates = [];

        let totalMass = 0; // Tracks sum of non-error probabilities.
        for(const pair of keyDistribution) {
          if(pair.p < KEYSTROKE_EPSILON) {
            totalMass += pair.p;
            break;
          } else if(timer && timer() >= TIMEOUT_THRESHOLD) {
            // Note:  it's always possible that the thread _executing_ our JS
            // got paused by the OS, even if JS itself is single-threaded.
            //
            // The case where `alternates` is initialized (line 167) but empty
            // (because of net-zero loop iterations) MUST be handled.
            break;
          }

          const mock = Mock.from(windowedMock, false);

          const altKey = pair.keySpec;
          if(!altKey) {
            console.warn("Internal error:  failed to properly filter set of keys for corrections");
            continue;
          }

          const altEvent = this.keyboardProcessor.activeKeyboard.constructKeyEvent(altKey, keyEvent.device, this.keyboardProcessor.stateKeys);
          const alternateBehavior = this.keyboardProcessor.processKeystroke(altEvent, mock);

          // If alternateBehavior.beep == true, ignore it.  It's a disallowed key sequence,
          // so we expect users to never intend their use.
          //
          // Also possible that this set of conditions fail for all evaluated alternates.
          if(alternateBehavior && !alternateBehavior.beep && pair.p > 0) {
            const transform: LexicalModelTypes.Transform = alternateBehavior.transcription.transform;

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
    return alternates;
  }

  public resetContext(outputTarget?: OutputTarget) {
    // Also handles new-context events, which may modify the layer
    this.keyboardProcessor.resetContext(outputTarget);
    // With the layer now set, we trigger new predictions.
    this.languageProcessor.invalidateContext(outputTarget, this.keyboardProcessor.layerId);
  }
}