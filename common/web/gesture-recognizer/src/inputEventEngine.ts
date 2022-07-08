/// <reference path="inputSequence.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />
/// <reference path="includes/events.ts" />
/// <reference path="incomplete.ts" />

namespace com.keyman.osk {
  /**
   * Supported events:
   *
   * - 'inputstart' - a new mouse or touch input sequence has begun.
   *   - further events are supported on the sequence itself.
   */
  export abstract class InputEventEngine extends EventEmitter {
    public static readonly INPUT_START_EVENT_NAME = "inputstart";

    protected readonly config: Nonoptional<GestureRecognizerConfiguration>;
    private _activeSequenceWrappers: Incomplete<InputSequence, InputSample>[] = [];

    public constructor(config: Nonoptional<GestureRecognizerConfiguration>) {
      super();
      this.config = config;
    }

    abstract registerEventHandlers();
    abstract unregisterEventHandlers();

    /**
     * @param identifier The identifier number corresponding to the input sequence.
     */
    hasActiveSequence(identifier: number) {
      return this._activeSequenceWrappers.findIndex((seq) => seq.item.identifier == identifier) != -1;
    }

    private getSequenceWrapperWithId(identifier: number) {
      return this._activeSequenceWrappers.find((seq) => seq.item.identifier == identifier);
    }

    public cleanupSequenceWithId(identifier: number) {
      this._activeSequenceWrappers = this._activeSequenceWrappers.filter((seq) => seq.item.identifier != identifier);
    }

    onInputStart(identifier: number, sample: InputSample, target: EventTarget) {
      let sequence = new InputSequence(identifier, target, this instanceof TouchEventEngine);
      sequence.addSample(sample);

      let sequenceWrapper = new Incomplete<InputSequence, InputSample>(sequence);

      this._activeSequenceWrappers.push(sequenceWrapper);
      this.emit(InputEventEngine.INPUT_START_EVENT_NAME, sequenceWrapper);
    }

    onInputMove(identifier: number, sample: InputSample) {
      const sequenceWrapper = this.getSequenceWrapperWithId(identifier);
      sequenceWrapper.item.addSample(sample);
      sequenceWrapper.signalUpdate(sample);
    }

    onInputMoveCancel(identifier: number) {
      let sequenceWrapper = this.getSequenceWrapperWithId(identifier);

      if(!sequenceWrapper) {
        return;
      }

      this.cleanupSequenceWithId(identifier);
      sequenceWrapper.cancel();
    }

    onInputEnd(identifier: number, sample: InputSample) {
      let sequenceWrapper = this.getSequenceWrapperWithId(identifier);

      if(!sequenceWrapper) {
        return;
      }

      this.cleanupSequenceWithId(identifier);
      sequenceWrapper.end();
    }
  }
}