namespace com.keyman.osk {
  export interface JSONTrackedInput {
    touchpoints: JSONTrackedPoint[];
  }

  interface EventMap {
    'end':    () => void;
    'cancel': () => void;
  }
  /**
   * Supported events:
   *
   * - 'cancel':  all gesture recognition on the sequence is to be cancelled
   *                   and left incomplete.
   * - 'end':     all gesture recognition on the sequence is to be resolved.
   */
  export class TrackedInput extends EventEmitter<EventMap> {
    public static readonly CANCEL_EVENT = 'cancel';
    public static readonly END_EVENT    = 'end';

    public readonly touchpoints: TrackedPoint[];

    private isActive = true;

    constructor(basePoint: TrackedPoint) {
      super();

      this.touchpoints = [ basePoint ];
      this._attachPointHooks(basePoint);
    }

    private _attachPointHooks(touchpoint: TrackedPoint) {
      touchpoint.path.on('complete', () => {
        this.isActive = false;
        this.emit('end');
        this.removeAllListeners();
      });

      touchpoint.path.on('invalidated', () => {
        this.isActive = false;
        this.emit('cancel');
        this.removeAllListeners();
      })
    }

    cancel() {
      if(this.isActive) {
        for(let point of this.touchpoints) {
          point.path.terminate(true);
        }
      }
    }

    end() {
      if(this.isActive) {
        for(let point of this.touchpoints) {
          point.path.terminate(false);
        }
      }
    }

    toJSON(): JSONTrackedInput {
      return {
        touchpoints: this.touchpoints.map((point) => point.toJSON())
      };
    }
  }
}