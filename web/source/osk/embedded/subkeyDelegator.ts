namespace com.keyman.osk.embedded {
  export class SubkeyDelegator {
    private resolver: (keyEvent: text.KeyEvent) => void;

    public readonly baseKey: KeyElement;

    constructor(e: KeyElement, resolve: (keyEvent: text.KeyEvent) => void) {
      this.resolver = resolve;
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