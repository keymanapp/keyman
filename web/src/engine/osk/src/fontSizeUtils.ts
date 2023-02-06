import { DeviceSpec } from "@keymanapp/web-utils";
import { ParsedLengthStyle } from "./lengthStyle.js";

export function getFontSizeStyle(e: HTMLElement|string): {val: number, absolute: boolean} {
  var fs: string;

  if(typeof e == 'string') {
    fs = e;
  } else {
    fs = e.style.fontSize;
    if(!fs) {
      fs = getComputedStyle(e).fontSize;
    }
  }

  return new ParsedLengthStyle(fs);
}

export function defaultFontSize(device: DeviceSpec, computedHeight: number, isEmbedded: boolean): ParsedLengthStyle {
  if(device.touchable) {
    const fontScale = device.formFactor == 'phone'
      ? 1.6 * (isEmbedded ? 0.65 : 0.6) * 1.2  // Combines original scaling factor with one previously applied to the layer group.
      : 2; // iPad or Android tablet
    return ParsedLengthStyle.special(fontScale, 'em');
  } else {
    return computedHeight ? ParsedLengthStyle.inPixels(computedHeight / 8) : undefined;
  }
}