namespace com.keyman.osk {
  /**
   * Supported events:
   *
   * - 'update':  a new sample (coord + timestamp) has been observed
   * - 'cancel':  all gesture recognition on the sequence is to be cancelled
   *                   and left incomplete.
   * - 'end':     all gesture recognition on the sequence is to be resolved.
   */
  export class Incomplete<Type, UpdateType> extends EventEmitter {
    public static readonly UPDATE_EVENT = 'update';
    public static readonly CANCEL_EVENT = 'cancel';
    public static readonly END_EVENT    = 'end';

    public readonly item: Type;

    private isActive = true;

    constructor(item: Type) {
      super();

      this.item = item;
    }

    signalUpdate(updateItem: UpdateType) {
      if(this.isActive) {
        this.emit('update', this, updateItem);
      } else {
        throw "No further updates to this object are permitted.";
      }
    }

    cancel() {
      if(this.isActive) {
        this.isActive = false;
        this.emit('cancel', this);
        this.removeAllListeners();
      }
    }

    end() {
      if(this.isActive) {
        this.isActive = false;
        this.emit('end', this);
        this.removeAllListeners();
      }
    }
  }
}