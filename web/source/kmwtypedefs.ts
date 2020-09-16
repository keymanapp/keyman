namespace com.keyman {
  export class AttachmentInfo {
    /**
     * Provides the core interface between the DOM and the actual keyboard.
     */
    interface:      text.OutputTarget;

    /**
     * Tracks the control's independent keyboard selection, when applicable.
     */
    keyboard:       string;

    /**
     * Tracks the language code corresponding to the `keyboard` field.
     */
    languageCode:   string;

    /**
     * Tracks if the control has an aliased control for touch functionality.
     * 
     * Future note - could be changed to track the DOMEventHandler instance used by this control;
     *               this may be useful for an eventual hybrid touch/non-touch implementation.
     */
    touchEnabled:   boolean;

    constructor(eleInterface: text.OutputTarget, kbd: string, touch?: boolean) {
      this.interface = eleInterface;
      this.keyboard = kbd;
      this.touchEnabled = touch || false;
    }
  }
}