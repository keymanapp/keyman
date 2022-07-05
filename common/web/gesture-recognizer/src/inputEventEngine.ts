/// <reference path="inputSequence.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />
/// <reference path="includes/events.ts" />

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

    private _activeSequences: InputSequence[] = [];

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
      return this._activeSequences.findIndex((seq) => seq.identifier == identifier) != -1;
    }

    private getSequenceWithId(identifier: number) {
      return this._activeSequences.find((seq) => seq.identifier == identifier);
    }

    public cancelSequenceWithId(identifier: number) {
      let seq = this.getSequenceWithId(identifier);

      if(!seq) {
        return;
      }

      seq.cancel();
    }

    public cleanupSequenceWithId(identifier: number) {
      this._activeSequences = this._activeSequences.filter((seq) => seq.identifier != identifier);
    }

    onInputStart(identifier: number, sample: InputSample, target: EventTarget) {
      let sequence = new InputSequence(this, identifier, target, this instanceof TouchEventEngine);
      sequence.addSample(sample);

      this._activeSequences.push(sequence);
      this.emit(InputEventEngine.INPUT_START_EVENT_NAME, sequence);
    }

    onInputMove(identifier: number, sample: InputSample) {
      const sequence = this._activeSequences.find((seq) => seq.identifier == identifier);
      sequence.addSample(sample);
    }

    onInputMoveCancel(identifier: number) {
      this.cancelSequenceWithId(identifier);
    }

    onInputEnd(identifier: number, sample: InputSample) {
      let seq = this.getSequenceWithId(identifier);

      if(!seq) {
        return;
      }

      seq.end();
    }
  }
}