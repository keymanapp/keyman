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
      // TODO:  Rework logging and references to be fully headless-compatible.  
      //        We'll probably need warning/error callbacks on Processor to facilitate that.
      let keyman = com.keyman.singleton;
      let outputTarget = this.transcription.keystroke.Ltarg;

      if(this.beep) {
        // TODO:  Must be relocated further 'out' to complete the full, planned web-core refactor.
        //        We're still referencing the DOM, even if only the manager object.  (It's an improvement, at least.)
        keyman.domManager.doBeep(outputTarget);
      }

      for(let storeID in this.setStore) {
        let sysStore = keyman.textProcessor.keyboardInterface.systemStores[storeID];
        if(sysStore) {
          try {
            sysStore.set(this.setStore[storeID]);
          } catch (error) {
            console.error("Rule attempted to perform illegal operation - 'platform' may not be changed.");
          }
        } else {
          console.warn("Unknown store affected by keyboard rule: " + storeID);
        }
      }

      // TODO: Gotta handle variable store save commands, which currently rely on cookies.

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