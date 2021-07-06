namespace com.keyman.osk {
  export class HelpPage {
    private readonly kbd: keyboards.Keyboard;
    public readonly root: HTMLDivElement;

    private static readonly ID = 'kmw-osk-help-page';

    constructor(keyboard: keyboards.Keyboard) {
      this.kbd = keyboard;

      var Ldiv = this.root = document.createElement('div');
      Ldiv.style.userSelect = "none";
      Ldiv.className = 'kmw-osk-static';
      Ldiv.id = HelpPage.ID;
      Ldiv.innerHTML = keyboard.helpText;
    }

    public postInsert() {
      if(!this.root.parentElement || !document.getElementById(HelpPage.ID)) {
        throw new Error("The HelpPage root element has not yet been inserted into the DOM.");
      }

      if(this.kbd.hasScript) {
        // .parentElement:  ensure this matches the _Box element from OSKManager / OSKView
        // Not a hard requirement for any known keyboards, but is asserted by legacy docs.
        this.kbd.embedScript(this.root.parentElement);
      }
    }
  }
}