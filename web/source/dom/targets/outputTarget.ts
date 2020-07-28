namespace com.keyman.dom.targets {
  export abstract class OutputTarget extends text.OutputTarget {
    /**
     * Returns the underlying element / document modeled by the wrapper.
     */
    abstract getElement(): HTMLElement;

    /**
     * A helper method for doInputEvent; creates a simple common event and default dispatching.
     * @param elem 
     */
    protected dispatchInputEventOn(elem: HTMLElement) {
      let event: InputEvent;

      // `undefined` in Edge and IE.
      if(window['InputEvent']) { // can't condition on the type directly; TS optimizes that out.
        event = new InputEvent('input', {"bubbles": true, "cancelable": false});
      }

      if(elem && event) {
        elem.dispatchEvent(event);
      }
    }
  }
}