/// <reference path="lengthStyle.ts" />

namespace com.keyman.osk {
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

    if(fs.indexOf('em') != -1) {
      const val = parseFloat(fs);
      return ParsedLengthStyle.forScalar(val);
    } else {
      return new ParsedLengthStyle(fs);
    }
  }
}