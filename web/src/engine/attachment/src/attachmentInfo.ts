import { OutputTargetElementWrapper } from 'keyman/engine/element-text-stores';

export class AttachmentInfo {
  /**
   * Provides the core interface between the DOM and the actual keyboard.
   */
  interface: OutputTargetElementWrapper<any>;

  /**
   * Tracks the control's independent keyboard selection, when applicable.
   */
  keyboard:       string;

  /**
   * Tracks the language code corresponding to the `keyboard` field.
   */
  languageCode:   string;

  /**
   * Tracks the inputmode originally set by the webpage.
   */
  inputMode?: string;

  constructor(eleInterface: OutputTargetElementWrapper<any>, kbd: string, touch?: boolean) {
    this.interface = eleInterface;
    this.keyboard = kbd;
  }
}