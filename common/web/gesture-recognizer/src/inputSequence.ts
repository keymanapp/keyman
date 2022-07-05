/// <reference path="inputSample.ts" />
namespace com.keyman.osk {

  /**
   * Supported events:
   *
   * - 'inputupdate':  a new sample (coord + timestamp) has been observed
   * - 'inputcancel':  all gesture recognition on the sequence is to be cancelled
   *                   and left incomplete.
   * - 'inputend':     all gesture recognition on the sequence is to be resolved.
   */
  export class InputSequence extends EventEmitter {
    public readonly isFromTouch: boolean;
    public readonly identifier: number;

    private _currentTarget: EventTarget;

    private _sequence: InputSampleSequence = [];
    private _engine: InputEventEngine;

    constructor(engine: InputEventEngine,
                identifier: number,
                initialTarget: EventTarget,
                isFromTouch: boolean) {
      super();

      this._engine = engine;
      this.identifier = identifier;
      this._currentTarget = initialTarget;
      this.isFromTouch = isFromTouch;
    }

    public get currentTarget(): EventTarget {
      return this._currentTarget;
    }

    addSample(sample: InputSample) {
      this._sequence.push(sample);
      this.emit('inputupdate', this, sample);
      // TODO:  generate event / do sequence processing
    }

    cancel() {
      this._engine.cleanupSequenceWithId(this.identifier);
      this._engine = null; // Help garbage collection out.

      this.emit('inputcancel', this);
      // TODO:  generate event / do cleanup
    }

    end() {
      this.emit('inputend', this);
      // TODO:  generate event / do sequence processing

      // Once done:
      this._engine.cleanupSequenceWithId(this.identifier);
      this._engine = null; // Help garbage collection out.
    }
  }
}