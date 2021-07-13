namespace com.keyman.osk {
  export interface LengthStyle {
    val: number,
    absolute: boolean
  };

  export class ParsedLengthStyle implements LengthStyle {
    public readonly val: number;
    public readonly absolute: boolean;

    public constructor(style: LengthStyle | string) {
      if(typeof style == 'string') {
        const parsed = ParsedLengthStyle.parseLengthStyle(style);
        this.val = parsed.val;
        this.absolute = parsed.absolute;
      } else {
        this.val = style.val;
        this.absolute = style.absolute;
      }
    }

    public get styleString(): string {
      if(this.absolute) {
        return this.val + 'px';
      } else {
        return this.absolute + '%';
      }
    }

    public scaledBy(scalar: number): ParsedLengthStyle {
      return new ParsedLengthStyle({
        val: scalar * this.val,
        absolute: this.absolute
      });
    }

    public static inPixels(val: number): ParsedLengthStyle {
      return new ParsedLengthStyle({val: val, absolute: true});
    }

    public static inPercent(val: number): ParsedLengthStyle {
      return new ParsedLengthStyle({val: val/100, absolute: false});
    }

    public static forScalar(val: number): ParsedLengthStyle {
      return new ParsedLengthStyle({val: val, absolute: false});
    }

    private static parseLengthStyle(spec: string): {val: number, absolute: boolean} {
      var val: number;
  
      if(spec.indexOf('px') != -1) {
        val = parseFloat(spec.substr(0, spec.indexOf('px')));
        return {val: val, absolute: true};
      } else if(spec.indexOf('pt') != -1) {
        // 16 px ~= 12 pt.
        // Reference: https://kyleschaeffer.com/css-font-size-em-vs-px-vs-pt-vs-percent
        val = parseFloat(spec.substr(0, spec.indexOf('pt')));
        return {val: (4 * val / 3), absolute: true};
      } else if(spec.indexOf('%') != -1) {
        val = parseFloat(spec.substr(0, spec.indexOf('%')));
        return {val: val/100, absolute: false};
      } else if(!isNaN(val = Number(spec))) {
        // Note:  this one is NOT natively handled by browsers!
        //        We'll treat it as if it were 'pt', since that's likely the user's
        //        most familiar font size unit.
        return {val: (4 * val / 3), absolute: true};
      } else {
        // Cannot parse.
        console.error("Could not properly parse specified length style info: '" + spec + "'.");
        return null;
      }
    }
  }
}