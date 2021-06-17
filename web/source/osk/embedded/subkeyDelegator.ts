namespace com.keyman.osk.embedded {
  export class SubkeyDelegator {
    private resolver: (keyEvent: text.KeyEvent) => void;

    public readonly baseKey: KeyElement;
    public readonly promise: Promise<text.KeyEvent>;

    constructor(e: KeyElement) {
      let _this = this;
      this.promise = new Promise<text.KeyEvent>(function(resolve) {
        _this.resolver = resolve;
      });

      this.baseKey = e;
    }

    public resolve(keyEvent: text.KeyEvent) {
      if(this.resolver) {
        this.resolver(keyEvent);
      }
      this.resolver = null;
    }
  }
}