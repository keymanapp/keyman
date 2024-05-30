import { GestureDebugSource } from "@keymanapp/gesture-recognizer";
import { HostFixtureLayoutController } from "./hostFixtureLayoutController.js";
import { RecordedCoordSequenceSet } from "./inputRecording.js";

/** Designed for use with the recorder test-page.  Its recordings will be
 *  used by other classes in this project, and sometimes even for unit-test
 *  verification itself.
 */

type WrappedInputSequence = GestureDebugSource<any>;

export class SequenceRecorder {
  controller: HostFixtureLayoutController;
  records:  {[identifier: string]: GestureDebugSource<any>} = {};

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

  public get count(): number {
    return this.startOrder.length;
  }

  private _attachRecognizerHooks() {
    this.controller.recognizer.on('inputstart', (wrappedSequence: WrappedInputSequence) => {
      const id = wrappedSequence.identifier;
      this.records[id]  = wrappedSequence;

      this.startOrder.push(id);
    });
  }

  public recordingsToJSON() {
    let arr = [];

    // Ensure a deterministic ordering for the recording's sequences.
    // This facilitates copmarison checks needed for unit testing.
    for(let entry of this.startOrder) {
      arr.push(this.records[entry].toJSON());
    }

    let jsonOut: RecordedCoordSequenceSet = {
      inputs: arr,
      config: this.controller.layoutConfiguration
    }

    return JSON.stringify(jsonOut, null, 2);
  }

  public clear() {
    this.records = {};
    this.startOrder = [];
  }
}