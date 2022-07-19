namespace Testing {
  /** Designed for use with the recorder test-page.  Its recordings will be
   *  used by other classes in this project, and sometimes even for unit-test
   *  verification itself.
   */

  type InputSequence = com.keyman.osk.InputSequence;
  type InputSample = com.keyman.osk.InputSample;

  type WrappedInputSequence = com.keyman.osk.Incomplete<InputSequence, InputSample>;

  export class SequenceRecorder {
    controller: HostFixtureLayoutController;
    records:  {[identifier: string]: RecordedCoordSequence} = {};

    /**
     * Tracks the order in which each sequence was first detected.
     */
    startOrder: string[] = [];

    constructor(controller: HostFixtureLayoutController) {
      this.controller = controller;
      controller.on(HostFixtureLayoutController.CONFIG_CHANGED_EVENT, () => {
        this.clear();
      });

      this._attachRecognizerHooks();
    }

    private _attachRecognizerHooks() {
      this.controller.recognizer.on(com.keyman.osk.GestureRecognizer.TRACKED_INPUT_UPDATE_EVENT_NAME, (wrappedSequence: WrappedInputSequence) => {
        const id = wrappedSequence.item.fullIdentifier;
        // TS, by default, doesn't like how we're "publicizing" the private field by doing this.
        //@ts-ignore (To override it selectively in this location.)
        this.records[id]  = { sequence: wrappedSequence.item };

        this.startOrder.push(id);

        this._attachWrappedSequenceHooks(wrappedSequence);
      });
    }

    private _attachWrappedSequenceHooks(wrappedSequence: WrappedInputSequence) {
      const CANCEL_EVENT = com.keyman.osk.Incomplete.CANCEL_EVENT;
      const END_EVENT    = com.keyman.osk.Incomplete.END_EVENT;

      wrappedSequence.on(CANCEL_EVENT, () => {
        this.records[wrappedSequence.item.fullIdentifier].terminationEvent = CANCEL_EVENT;
      });

      wrappedSequence.on(END_EVENT, () => {
        this.records[wrappedSequence.item.fullIdentifier].terminationEvent = END_EVENT;
      })
    }

    public recordingsToJSON() {
      let arr = [];

      // Ensure a deterministic ordering for the recording's sequences.
      // This facilitates copmarison checks needed for unit testing.
      for(let entry of this.startOrder) {
        arr.push(this.records[entry]);
      }

      let jsonOut = {
        set: arr,
        config: this.controller.layoutConfiguration
      }

      return JSON.stringify(jsonOut, com.keyman.osk.InputSequence._replacer, 2);
    }

    public clear() {
      this.records = {};
      this.startOrder = [];
    }
  }
}