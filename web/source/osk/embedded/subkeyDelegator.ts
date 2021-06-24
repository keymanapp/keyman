namespace com.keyman.osk.embedded {
  export class SubkeyDelegator {
    private resolver: (keyEvent: text.KeyEvent) => void;
    private readonly vkbd: VisualKeyboard;

    public readonly baseKey: KeyElement;
    public readonly promise: Promise<text.KeyEvent>;

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

        let keyEvent: text.KeyEvent = null;
        if(selectedKey) {
          keyEvent = this.vkbd.keyEventFromSpec(selectedKey as keyboards.ActiveKey, null);
          keyEvent.vkCode=keyEvent.Lcode;
        }

        this.resolver(keyEvent);
      }
      this.resolver = null;
    }
  }
}