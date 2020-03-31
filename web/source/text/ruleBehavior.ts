namespace com.keyman.text {
  /**
   * Represents the commands and state changes that result from a matched keyboard rule.
   */
  export class RuleBehavior {
    /**
     * The before-and-after Transform from matching a keyboard rule.
     */
    transcription: Transcription;

    /**
     * Indicates whether or not a BEEP command was issued by the matched keyboard rule.
     */
    beep?: boolean;

    /**
     * A set of changed store values triggered by the matched keyboard rule.
     */
    setStore: {[id: number]: string} = {};

    /**
     * Denotes a non-output default behavior; this should be evaluated later, against the true keystroke.
     */
    triggersDefaultCommand?: boolean;

    /**
     * Denotes error log messages generated when attempting to generate this behavior.
     */
    errorLog?: string;

    /**
     * Denotes warning log messages generated when attempting to generate this behavior.
     */
    warningLog?: string;

    finalize() {
      let keyman = com.keyman.singleton;
      let outputTarget = this.transcription.keystroke.Ltarg;

      if(this.beep) {
        // TODO:  Must be relocated further 'out' to complete the full, planned web-core refactor.
        //        We're still referencing the DOM, even if only the manager object.  (It's an improvement, at least.)
        keyman.domManager.doBeep(outputTarget);
      }

      for(let storeID in this.setStore) {
        // TODO:  Must be relocated further 'out' to complete the full, planned web-core refactor.
        //        `Processor` shouldn't be directly setting anything on the OSK when the refactor is complete.
        //
        //        There's also the issue of Stores in general, which rely on cookies...
        //        Gotta handle variable stores as well, which we currently do nothing for!
        
        // How would this be handled in an eventual headless mode?
        switch(Number.parseInt(storeID)) { // Because the number was converted into a String for 'dictionary' use.
          case KeyboardInterface.TSS_LAYER:
            keyman.textProcessor.layerId = this.setStore[storeID];
            break;
          case KeyboardInterface.TSS_PLATFORM:
            console.error("Rule attempted to perform illegal operation - 'platform' may not be changed.");
            break;
          default:
            console.warn("Unknown store affected by keyboard rule: " + storeID);
        }
      }

      if(this.triggersDefaultCommand) {
        let keyEvent = this.transcription.keystroke;
        DefaultOutput.applyCommand(keyEvent);
      }

      // Safe both in browser and Node contexts.
      if(this.warningLog) {
        console.warn(this.warningLog);
      } else if(this.errorLog) {
        console.error(this.errorLog);
      }
    }
  }
}