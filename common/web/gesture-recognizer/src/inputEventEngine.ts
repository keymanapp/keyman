/// <reference path="trackedPoint.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />
/// <reference path="includes/events.ts" />
/// <reference path="incomplete.ts" />

namespace com.keyman.osk {
  interface EventMap {
    'inputstart': (sequence: Incomplete<TrackedPoint, InputSample>) => void
  }

  /**
   * Supported events:
   *
   * - 'inputstart' - a new mouse or touch input sequence has begun.
   *   - further events are supported on the sequence itself.
   */
  export abstract class InputEventEngine extends EventEmitter<EventMap> {
    public static readonly INPUT_START_EVENT_NAME = "inputstart";

    protected readonly config: Nonoptional<GestureRecognizerConfiguration>;
    private _activeSequenceWrappers: Incomplete<TrackedPoint, InputSample>[] = [];

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
      return this._activeSequenceWrappers.findIndex((seq) => seq.item.rawIdentifier == identifier) != -1;
    }

    private getSequenceWrapperWithId(identifier: number) {
      return this._activeSequenceWrappers.find((seq) => seq.item.rawIdentifier == identifier);
    }

    public cleanupSequenceWithId(identifier: number) {
      this._activeSequenceWrappers = this._activeSequenceWrappers.filter((seq) => seq.item.rawIdentifier != identifier);
    }

    protected buildSampleFor(clientX, clientY): InputSample {
      const targetRect = this.config.targetRoot.getBoundingClientRect();
      return {
        clientX: clientX,
        clientY: clientY,
        targetX: clientX - targetRect.left,
        targetY: clientY - targetRect.top,
        t: performance.now()
      };
    }

    onInputStart(identifier: number, sample: InputSample, target: EventTarget) {
      let sequence = new TrackedPoint(identifier, target, this instanceof TouchEventEngine);
      sequence.path.addSample(sample);

      let sequenceWrapper = new Incomplete<TrackedPoint, InputSample>(sequence);

      // External objects may desire to directly terminate handling of
      // input sequences under specific conditions.
      let _this = this;
      sequenceWrapper.on('cancel', function() {
        _this.cleanupSequenceWithId(identifier);
      });

      sequenceWrapper.on('end', function() {
        _this.cleanupSequenceWithId(identifier);
      });

      this._activeSequenceWrappers.push(sequenceWrapper);
      this.emit(InputEventEngine.INPUT_START_EVENT_NAME, sequenceWrapper);
    }

    onInputMove(identifier: number, sample: InputSample) {
      const sequenceWrapper = this.getSequenceWrapperWithId(identifier);
      sequenceWrapper.item.path.addSample(sample);
      sequenceWrapper.signalUpdate(sample);
    }

    onInputMoveCancel(identifier: number) {
      let sequenceWrapper = this.getSequenceWrapperWithId(identifier);

      if(!sequenceWrapper) {
        return;
      }

      sequenceWrapper.cancel();
    }

    onInputEnd(identifier: number, sample: InputSample) {
      let sequenceWrapper = this.getSequenceWrapperWithId(identifier);

      if(!sequenceWrapper) {
        return;
      }

      sequenceWrapper.end();
    }
  }
}