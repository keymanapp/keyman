namespace com.keyman.dom.targets {
  export abstract class OutputTarget extends text.OutputTarget {
    /**
     * Returns the underlying element / document modeled by the wrapper.
     */
    abstract getElement(): HTMLElement;

    public focus(): void {
      const ele = this.getElement();
      if(ele.focus) {
        ele.focus();
      }
    }

    /**
     * A helper method for doInputEvent; creates a simple common event and default dispatching.
     * @param elem 
     */
    protected dispatchInputEventOn(elem: HTMLElement) {
      let event: InputEvent;

      // `undefined` in pre-Chrome Edge and Chrome for Android before version 60.
      if(window['InputEvent']) { // can't condition on the type directly; TS optimizes that out.
        event = new InputEvent('input', {"bubbles": true, "cancelable": false});
      }

      if(elem && event) {
        elem.dispatchEvent(event);
      }
    }

    apply(transform: Transform) {
      super.apply(transform);

      // This class has non-integrated unit tests in which the `singleton` object doesn't exist.
      // Thus, we need to test for this case.
      let keyman = com.keyman['singleton'];

      // Signal the necessary text changes to the embedding app, if it exists.
      if(keyman && keyman['oninserttext'] && keyman.isEmbedded) {
        keyman['oninserttext'](transform.deleteLeft, transform.insert, transform.deleteRight);
      }
    }
  }
}