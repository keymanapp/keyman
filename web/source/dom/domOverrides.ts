namespace com.keyman.dom {
  text.prediction.LanguageProcessor.prototype.canEnable = function(): boolean {
    if(typeof Worker != 'function') {
      console.warn("WebWorkers are not supported by this browser.");
      return false;
    }

    return true;
  }

  let headlessRuleBehaviorFinalize = text.RuleBehavior.prototype.finalize;
  text.RuleBehavior.prototype.finalize = function(this: text.RuleBehavior, processor: text.KeyboardProcessor, outputTarget: text.OutputTarget, readonly: boolean) {
    let keyman = com.keyman.singleton;
    // Execute the standard baseline stuff first.
    headlessRuleBehaviorFinalize.call(this, processor);

    // newContext and postKeystroke events cannot emit content
    if(readonly) {
      return;
    }

    // If the transform isn't empty, we've changed text - which should produce a 'changed' event in the DOM.
    let ruleTransform = this.transcription.transform;
    if(ruleTransform.insert != "" || ruleTransform.deleteLeft > 0 || ruleTransform.deleteRight > 0) {
      if(outputTarget instanceof targets.OutputTarget && outputTarget.getElement() == keyman.domManager.activeElement) {
        dom.DOMEventHandlers.states.changed = true;
      }
    }

    // KMEA and KMEI (embedded mode) use direct insertion of the character string
    if(keyman.isEmbedded) {
      // A special embedded callback used to setup direct callbacks to app-native code.
      keyman['oninserttext'](ruleTransform.deleteLeft, ruleTransform.insert, ruleTransform.deleteRight);
      if(outputTarget instanceof targets.OutputTarget) {
        keyman.refreshElementContent(outputTarget.getElement());
      }
    }
  }
}