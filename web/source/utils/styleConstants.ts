// Includes Device definitions, which may play a role in constant logic.
///<reference path="../kmwdevice.ts" />

/*
 * This file is intended for CSS-styling constants that see use with the OSK.
 */

namespace com.keyman.utils {
  /**
   * Defines device-level constants used for CSS styling.
   */
  export class StyleConstants {
    constructor(device: Device) {
      // popupCanvasBackgroundColor
      if(device.OS == 'Android') {
        this.popupCanvasBackgroundColor = '#999';
      } else {
        this.popupCanvasBackgroundColor = device.colorScheme == 'dark' ? '#0f1319' : '#ffffff';
      }  
    }

    public readonly popupCanvasBackgroundColor: string;
  }
}