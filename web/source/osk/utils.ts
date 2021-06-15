namespace com.keyman.osk {
  export function getFontSizeStyle(e: HTMLElement|string): {val: number, absolute: boolean} {
    var val: number;
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
      val = parseFloat(fs.substr(0, fs.indexOf('em')));
      return {val: val, absolute: false};
    } else if(fs.indexOf('px') != -1) {
      val = parseFloat(fs.substr(0, fs.indexOf('px')));
      return {val: val, absolute: true};
    } else if(fs.indexOf('pt') != -1) {
      // 16 px ~= 12 pt.
      // Reference: https://kyleschaeffer.com/css-font-size-em-vs-px-vs-pt-vs-percent
      val = parseFloat(fs.substr(0, fs.indexOf('pt')));
      return {val: (4 * val / 3), absolute: true};
    } else if(fs.indexOf('%') != -1) {
      val = parseFloat(fs.substr(0, fs.indexOf('%')));
      return {val: val/100, absolute: false};
    } else if(!isNaN(val = Number(fs))) {
      // Note:  this one is NOT natively handled by browsers!
      //        We'll treat it as if it were 'pt', since that's likely the user's
      //        most familiar font size unit.
      return {val: (4 * val / 3), absolute: true};
    } else {
      // Cannot parse.
      console.error("Could not properly parse specified fontsize info: '" + fs + "'.");
      return null;
    }
  }
}