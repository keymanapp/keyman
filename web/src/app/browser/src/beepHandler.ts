import { type KeyboardMinimalInterface } from 'keyman/engine/keyboard';
import { DesignIFrameElementTextStore, AbstractElementTextStore } from 'keyman/engine/element-text-stores';

// Utility object used to handle beep (keyboard error response) operations.
class BeepData {
  e: HTMLElement;
  c: string;

  constructor(e: HTMLElement) {
    this.e = e;
    this.c = e.style.backgroundColor;
  }

  reset(): void {
    this.e.style.backgroundColor = this.c;
  }
}

export class BeepHandler {
  readonly keyboardInterface: KeyboardMinimalInterface;

  constructor(keyboardInterface: KeyboardMinimalInterface) {
    this.keyboardInterface = keyboardInterface;
  }

  _BeepObjects: BeepData[] = [];  // BeepObjects - maintains a list of active 'beep' visual feedback elements
  _BeepTimeout: number = 0;       // BeepTimeout - a flag indicating if there is an active 'beep'.
                                  // Set to 1 if there is an active 'beep', otherwise leave as '0'.
  /**
   * Function     beep          KB      (DOM-side implementation)
   * Scope        Public
   * @param       {Object}      Pelem     element to flash
   * Description  Flash body as substitute for audible beep; notify embedded device to vibrate
   */
  beep(textStore: AbstractElementTextStore<any>) {
    if (!(textStore instanceof AbstractElementTextStore)) {
      return;
    }

    // All code after this point is DOM-based, triggered by the beep.
    let Pelem: HTMLElement = textStore.getElement();
    if(textStore instanceof DesignIFrameElementTextStore) {
      Pelem = textStore.docRoot; // I1446 - beep sometimes fails to flash when using OSK and rich control
    }

    if(!Pelem) {
      return; // There's no way to signal a 'beep' to null, so just cut everything short.
    }

    if(!Pelem.style || typeof(Pelem.style.backgroundColor)=='undefined') {
      return;
    }

    for(let Lbo=0; Lbo<this._BeepObjects.length; Lbo++) { // I1446 - beep sometimes fails to return background color to normal
                                                                // I1511 - array prototype extended
      if(this._BeepObjects[Lbo].e == Pelem) {
        return;
      }
    }

    this._BeepObjects.push(new BeepData(Pelem));

    // TODO:  This is probably a bad color choice if "dark mode" is enabled.  A proper implementation
    //        would probably require some 'fun' CSS work, though.
    Pelem.style.backgroundColor = '#000000';
    if(this._BeepTimeout == 0) {
      this._BeepTimeout = 1;
      window.setTimeout(this.reset, 50);
    }
  }

  /**
   * Function     reset
   * Scope        Public
   * Description  Reset/terminate beep or flash (not currently used: Aug 2011)
   */
  readonly reset = () => {
    this._BeepTimeout = 0;
    for(let Lbo=0;Lbo<this._BeepObjects.length;Lbo++) { // I1511 - array prototype extended
      this._BeepObjects[Lbo].reset();
    }
    this._BeepObjects = [];
  }
}