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
    wrappers: {[identifier: string]: WrappedInputSequence}  = {};
    records:  {[identifier: string]: RecordedInputSequence} = {};

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
        this.wrappers[id] = wrappedSequence;
        this.records[id]  = { sequence: wrappedSequence.item };

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

      for(let entry in this.records) {
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
      this.wrappers = {};
    }
  }
}