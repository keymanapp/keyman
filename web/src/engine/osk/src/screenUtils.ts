import { landscapeView } from "keyman/engine/dom-utils";
import { DeviceSpec } from "@keymanapp/web-utils";

/**
 * Get viewport scale factor for this document
 *
 * @return      {number}
 */
export function getViewportScale(formFactor: DeviceSpec.FormFactor): number {
  // This can sometimes fail with some browsers if called before document defined,
  // so catch the exception
  try {
    // For emulation of iOS on a desktop device, use a default value
    if(formFactor == 'desktop') {
      return 1;
    }

    // Get viewport width
    var viewportWidth = document.documentElement.clientWidth;

    // Return a default value if screen width is greater than the viewport width (not fullscreen).
    if(screen.width > viewportWidth) {
      return 1;
    }

    // Get the orientation corrected screen width
    var screenWidth = screen.width;
    if(landscapeView()) {
      // Take larger of the two dimensions
      if(screen.width < screen.height) {
        screenWidth = screen.height;
      }
    } else {
      // Take smaller of the two dimensions
      if(screen.width > screen.height) {
        screenWidth = screen.height;
      }
    }
    // Calculate viewport scale
    return Math.round(100*screenWidth / viewportWidth)/100;
  } catch(ex) {
    return 1;
  }
}