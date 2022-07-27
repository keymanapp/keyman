/// <reference path="trackedPoint.ts" />
/// <reference path="gestureRecognizerConfiguration.ts" />
/// <reference path="includes/events.ts" />
/// <reference path="trackedInput.ts" />

namespace com.keyman.osk {
  interface EventMap {
    'pointstart': (input: TrackedPoint) => void
  }

  export abstract class InputEventEngine extends EventEmitter<EventMap> {
    protected readonly config: Nonoptional<GestureRecognizerConfiguration>;
    private _activeTouchpoints: TrackedPoint[] = [];

    public constructor(config: Nonoptional<GestureRecognizerConfiguration>) {
      super();
      this.config = config;
    }

    abstract registerEventHandlers();
    abstract unregisterEventHandlers();

    /**
     * @param identifier The identifier number corresponding to the input sequence.
     */
    hasActiveTouchpoint(identifier: number) {
      return this._activeTouchpoints.findIndex((point) => point.rawIdentifier == identifier) != -1;
    }

    private getTouchpointWithId(identifier: number) {
      return this._activeTouchpoints.find((point) => point.rawIdentifier == identifier);
    }

    public dropTouchpointWithId(identifier: number) {
      this._activeTouchpoints = this._activeTouchpoints.filter((point) => point.rawIdentifier != identifier);
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
      let touchpoint = new TrackedPoint(identifier, target, this instanceof TouchEventEngine);
      touchpoint.path.extend(sample);

      this._activeTouchpoints.push(touchpoint);

      // External objects may desire to directly terminate handling of
      // input sequences under specific conditions.
      let _this = this;
      touchpoint.path.on('invalidated', function() {
        _this.dropTouchpointWithId(identifier);
      });

      touchpoint.path.on('complete', function() {
        _this.dropTouchpointWithId(identifier);
      });

      this.emit('pointstart', touchpoint);
    }

    onInputMove(identifier: number, sample: InputSample) {
      const touchpoint = this.getTouchpointWithId(identifier);
      touchpoint.path.extend(sample);
    }

    onInputMoveCancel(identifier: number, sample: InputSample) {
      let touchpoint = this.getTouchpointWithId(identifier);

      if(!touchpoint) {
        return;
      }

      touchpoint.path.extend(sample);
      touchpoint.path.terminate(true);
    }

    onInputEnd(identifier: number) {
      let touchpoint = this.getTouchpointWithId(identifier);

      if(!touchpoint) {
        return;
      }

      // We do not add a sample here because any 'end' event immediately follows a
      // 'move' if it occurred simultaneously.
      touchpoint.path.terminate(false);
    }
  }
}