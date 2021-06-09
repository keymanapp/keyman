namespace com.keyman.text {
  /**
   * Represents the commands and state changes that result from a matched keyboard rule.
   */
  export class RuleBehavior {
    /**
     * The before-and-after Transform from matching a keyboard rule.  May be `null`
     * if no keyboard rules were matched for the keystroke.
     */
    transcription: Transcription = null;

    /**
     * Indicates whether or not a BEEP command was issued by the matched keyboard rule.
     */
    beep?: boolean;

    /**
     * A set of changed store values triggered by the matched keyboard rule.
     */
    setStore: {[id: number]: string} = {};

    /**
     * A set of variable stores with save requests triggered by the matched keyboard rule
     */
    saveStore: {[name: string]: VariableStore} = {};

    /**
     * Denotes a non-output default behavior; this should be evaluated later, against the true keystroke.
     */
    triggersDefaultCommand: boolean = false;

    /**
     * Denotes error log messages generated when attempting to generate this behavior.
     */
    errorLog?: string;

    /**
     * Denotes warning log messages generated when attempting to generate this behavior.
     */
    warningLog?: string;

    /**
     * If predictive text is active, contains a Promise returning predictive Suggestions.
     */
    predictionPromise?: Promise<Suggestion[]>;

    finalize(processor: KeyboardProcessor, outputTarget: OutputTarget) {
      if(!this.transcription) {
        throw "Cannot finalize a RuleBehavior with no transcription.";
      }

      if(processor.beepHandler && this.beep) {
        processor.beepHandler(outputTarget);
      }

      for(let storeID in this.setStore) {
        let sysStore = processor.keyboardInterface.systemStores[storeID];
        if(sysStore) {
          try {
            sysStore.set(this.setStore[storeID]);
          } catch (error) {
            if(processor.errorLogger) {
              processor.errorLogger("Rule attempted to perform illegal operation - 'platform' may not be changed.");
            }
          }
        } else if(processor.warningLogger) {
          processor.warningLogger("Unknown store affected by keyboard rule: " + storeID);
        }
      }

      if(processor.keyboardInterface.variableStoreSerializer) {
        for(let storeID in this.saveStore) {
          processor.keyboardInterface.variableStoreSerializer.saveStore(processor.activeKeyboard.id, storeID, this.saveStore[storeID]);
        }
      }

      if(this.triggersDefaultCommand) {
        let keyEvent = this.transcription.keystroke;
        DefaultOutput.applyCommand(keyEvent, outputTarget);
      }

      if(processor.warningLogger && this.warningLog) {
        processor.warningLogger(this.warningLog);
      } else if(processor.errorLogger && this.errorLog) {
        processor.errorLogger(this.errorLog);
      }
    }
  }
}