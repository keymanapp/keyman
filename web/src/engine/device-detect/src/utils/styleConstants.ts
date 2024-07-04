import { DeviceSpec } from '@keymanapp/web-utils';

/*
 * This file is intended for CSS-styling constants that see use with the OSK.
 */

/**
 * Defines device-level constants used for CSS styling.
 */
export default class StyleConstants {
  constructor(device: DeviceSpec) {
    // popupCanvasBackgroundColor
    if(device.OS == DeviceSpec.OperatingSystem.Android) {
      this.popupCanvasBackgroundColor = '#999';
    } else {
      this.popupCanvasBackgroundColor = StyleConstants.prefersDarkMode() ? '#0f1319' : '#ffffff';
    }
  }

  /**
   * Checks is a user's browser is in dark mode, if the feature is supported.  Returns false otherwise.
   *
   * Thanks to https://stackoverflow.com/a/57795518 for this code.
   */
    static prefersDarkMode(): boolean {
    // Ensure the detector exists (otherwise, returns false)
    return window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
  }

  public readonly popupCanvasBackgroundColor: string;
}