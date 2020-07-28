/// <reference path="outputTarget.ts" />

namespace com.keyman.text {
  // Represents a probability distribution over a keyboard's keys.
  // Defined here to avoid compilation issues.
  export type KeyDistribution = {keyId: string, p: number}[];

  /**
   * This class is defined within its own file so that it can be loaded by code outside of KMW without
   * having to actually load the entirety of KMW.
   */
  export class KeyEvent {
    Ltarg: OutputTarget;
    Lcode: number;
    Lstates: number;
    LmodifierChange?: boolean;
    Lmodifiers: number;
    LisVirtualKey: boolean;
    vkCode: number;
    kName: string;
    kLayer?: string;   // The key's layer property
    kbdLayer?: string; // The virtual keyboard's active layer
    kNextLayer?: string;
    
    // Holds relevant event properties leading to construction of this KeyEvent.
    source?: any; // Technically, KeyEvent|MouseEvent|Touch - but those are DOM types that must be kept out of headless mode.
    // Holds a generated fat-finger distribution (when appropriate)
    keyDistribution?: KeyDistribution;
    
    /**
     * The device model for web-core to follow when processing the keystroke.
     */
    device: utils.DeviceSpec;

    /**
     * `true` if this event was produced by sources other than a DOM-based KeyboardEvent.
     */
    isSynthetic: boolean = true;
  };
}