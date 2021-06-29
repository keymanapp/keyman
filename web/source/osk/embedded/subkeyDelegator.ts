/// <reference path="../realizedGesture.interface.ts" />

namespace com.keyman.osk.embedded {
  // "Delegator", rather than "Popup", because KMW delegates display + selection
  // of subkeys to the host app when in the embedded context.
  export class SubkeyDelegator implements RealizedGesture {
    private resolver: (keyEvent: text.KeyEvent) => void;
    private readonly vkbd: VisualKeyboard;

    public readonly baseKey: KeyElement;
    public readonly promise: Promise<text.KeyEvent>;

    private movedFromBaseKey: boolean = false;
    private baseKeySelected: boolean = false;

    constructor(vkbd: VisualKeyboard, e: KeyElement) {
      this.vkbd = vkbd;

      let _this = this;
      this.promise = new Promise<text.KeyEvent>(function(resolve) {
        _this.resolver = resolve;
      });

      this.baseKey = e;
    }

    public resolve(keyCoreID: string) {
      if(this.resolver) {
        let keyEvent: text.KeyEvent = null;

        if(keyCoreID == null && this.baseKeySelected) {
          // Handle selection of base key underneath the subkey array.
          keyEvent = this.vkbd.keyEventFromSpec(this.baseKey.key.spec as keyboards.ActiveKey, null);
          this.baseKey.key.highlight(false);
        } else {
          // This is set with the base key of our current subkey elsewhere within the engine.
          var baseKey: OSKKeySpec = this.baseKey.key.spec;
          var found = false;
          let selectedKey: OSKKeySpec;

          if(baseKey.coreID == keyCoreID) {
            selectedKey = baseKey;
            found = true;
          } else {
            // ... yeah, there are some funky type shenanigans between the two.
            // OSKKeySpec is the OSK's... reinterpretation of the ActiveKey type.
            selectedKey = (baseKey as keyboards.ActiveKey).getSubkey(keyCoreID) as OSKKeySpec;
            found = !!selectedKey;
          }

          if(!found) {
            this.resolver(null); // Maintains existing behavior.
            throw new Error("Could not find subkey '" + keyCoreID + "' under base key '" + baseKey.coreID + "'!");
          }

          if(selectedKey) {
            keyEvent = this.vkbd.keyEventFromSpec(selectedKey as keyboards.ActiveKey, null);
            keyEvent.vkCode=keyEvent.Lcode;
          }
        }

        this.resolver(keyEvent);
      }
      this.resolver = null;
    }

    public isVisible(): boolean {
      return true;
    }

    public clear() {
      // no-op; it's fully controlled on the app side.
    }

    updateTouch(touch: Touch) {
      let baseKeyTouched = this.baseKey.key.isUnderTouch(touch);
      this.baseKeySelected = this.baseKey.key.isUnderTouch(touch)

      // Prevent highlighting & selection before the touch has moved from the base key.
      if(this.movedFromBaseKey) {
        this.baseKey.key.highlight(this.baseKeySelected);
      } else {
        this.movedFromBaseKey = !baseKeyTouched;
      }
    }
  }
}