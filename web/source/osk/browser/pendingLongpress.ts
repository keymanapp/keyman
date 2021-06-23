/// <reference path="subkeyPopup.ts" />

namespace com.keyman.osk.browser {
  export class PendingLongpress {
    public readonly baseKey: KeyElement;
    //public readonly initialTouch: Touch;

    public readonly subkeyUI: SubkeyPopup;

    public readonly promise: Promise<SubkeyPopup>;

    private readonly vkbd: VisualKeyboard;
    private resolver: (subkeyPopup: SubkeyPopup) => void;

    private timerId: number;
    private popupDelay: number = 500;

    constructor(vkbd: VisualKeyboard, baseKey: KeyElement/*, initialTouch: Touch*/) {
      this.vkbd = vkbd;
      this.baseKey = baseKey;
      //this.initialTouch = initialTouch;

      let _this = this;
      this.promise = new Promise<SubkeyPopup>(function(resolve, reject) {
        _this.resolver = resolve;
        _this.timerId = window.setTimeout(
          function() {
            // It's no longer deferred; it's being fulfilled.
            // Even if the actual subkey itself is still async.
            _this.showSubkeys();
          }, _this.popupDelay);
      });
    }

    updateTouch(touch: Touch) {
      this.subkeyUI.updateTouch(touch);
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

    public showSubkeys() {
      if(this.resolver) {
        this.resolver(new SubkeyPopup(this.vkbd, this.baseKey));
      }
    } 
  }
}