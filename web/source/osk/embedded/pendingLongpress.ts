/// <reference path="subkeyDelegator.ts" />

namespace com.keyman.osk.embedded {
  export class PendingLongpress {
    private resolver: (delegator: SubkeyDelegator) => void;

    public readonly baseKey: KeyElement;
    public readonly promise: Promise<SubkeyDelegator>;

    constructor(e: KeyElement) {
      let _this = this;
      this.promise = new Promise<SubkeyDelegator>(function(resolve) {
        _this.resolver = resolve;
      });
      this.baseKey = e;
    }

    public markActiveSubkeys() {
      if(this.resolver) {
        this.resolver(new SubkeyDelegator(this.baseKey));
      }
      this.resolver = null;
    }
  }
}