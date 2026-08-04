import { AbstractElementTextStore } from 'keyman/engine/element-text-stores';

export class AttachmentInfo {
  /**
   * Tracks the language code corresponding to the `keyboard` field.
   */
  public languageCode:   string;

  /**
   * Tracks the inputmode originally set by the webpage.
   */
  public inputMode?: string;

  /**
   * Constructor for AttachmentInfo.
   *
   * @param textStore - Provides the core interface between the DOM and the actual keyboard.
   * @param keyboard  - Provides the keyboard identifier.
   */
  constructor(public readonly textStore: AbstractElementTextStore<any>, public keyboard: string) {}
}
