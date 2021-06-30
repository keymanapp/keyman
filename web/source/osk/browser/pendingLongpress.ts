/// <reference path="subkeyPopup.ts" />
/// <reference path="../pendingGesture.interface.ts" />

namespace com.keyman.osk.browser {
  export class PendingLongpress implements PendingGesture {
    public readonly baseKey: KeyElement;
    public readonly promise: Promise<SubkeyPopup>;

    public readonly subkeyUI: SubkeyPopup;

    private readonly vkbd: VisualKeyboard;
    private resolver: (subkeyPopup: SubkeyPopup) => void;

    private timerId: number;
    private popupDelay: number = 500;

    constructor(vkbd: VisualKeyboard, baseKey: KeyElement) {
      this.vkbd = vkbd;
      this.baseKey = baseKey;

      let _this = this;
      this.promise = new Promise<SubkeyPopup>(function(resolve, reject) {
        _this.resolver = resolve;
        // After the timeout, it's no longer deferred; it's being fulfilled.
        // Even if the actual subkey itself is still async.
        _this.timerId = window.setTimeout(_this.resolve, _this.popupDelay);
      });
    }

    public cancel() {
      if(this.timerId) {
        window.clearTimeout(this.timerId);
        this.timerId = null;
      }

      if(this.resolver) {        
        this.resolver(null);
        this.resolver = null;
      }
    }

    public resolve() {
      if(this.resolver) {
        this.resolver(new SubkeyPopup(this.vkbd, this.baseKey));
      }
    } 
  }
}
