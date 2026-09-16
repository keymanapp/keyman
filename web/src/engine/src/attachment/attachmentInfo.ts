import { AbstractElementTextStore } from 'keyman/engine/element-text-stores';

export class AttachmentInfo {
  /**
   * Tracks the language code corresponding to the `keyboard` field.
   */
  public languageCode: string | null = null;

  /**
   * Tracks the inputmode originally set by the webpage.
   */
  public inputMode?: string;

  /**
   * For elements that don't use the global keyboard, this tracks whether
   * or not the OSK is enabled for the element. The state will be saved
   * when loosing focus, and restored when regaining focus.
   */
  public oskEnabled: boolean = false;

  /**
   * Constructor for AttachmentInfo.
   *
   * @param textStore - Provides the core interface between the DOM and the actual keyboard.
   * @param keyboard  - Provides the keyboard identifier, empty string for system keyboard,
   *                    or null to use the global keyboard.
   */
  constructor(
    public readonly textStore: AbstractElementTextStore<any>,
    public keyboard: string | null) { }
}
