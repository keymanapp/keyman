/// <reference path="keyboardView.interface.ts" />

namespace com.keyman.osk {
  export class HelpPageView implements KeyboardView {
    private readonly kbd: keyboards.Keyboard;
    public readonly element: HTMLDivElement;

    private static readonly ID = 'kmw-osk-help-page';

    constructor(keyboard: keyboards.Keyboard) {
      this.kbd = keyboard;

      var Ldiv = this.element = document.createElement('div');
      Ldiv.style.userSelect = "none";
      Ldiv.className = 'kmw-osk-static';
      Ldiv.id = HelpPageView.ID;
      Ldiv.innerHTML = keyboard.helpText;
    }

    public postInsert() {
      if(!this.element.parentElement || !document.getElementById(HelpPageView.ID)) {
        throw new Error("The HelpPage root element has not yet been inserted into the DOM.");
      }

      if(this.kbd.hasScript) {
        // .parentElement:  ensure this matches the _Box element from OSKManager / OSKView
        // Not a hard requirement for any known keyboards, but is asserted by legacy docs.
        this.kbd.embedScript(this.element.parentElement);
      }
    }

    public updateState() { }
    public refreshLayout() { }

    public get layoutHeight(): ParsedLengthStyle {
      return ParsedLengthStyle.inPercent(100);
    }
  }
}