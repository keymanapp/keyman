namespace com.keyman.osk {
  export interface LengthStyle {
    val: number,
    absolute: boolean,
    special?: 'em' | 'rem';
  };

  export class ParsedLengthStyle implements LengthStyle {
    public readonly val: number;
    public readonly absolute: boolean;
    public readonly special: 'em' | 'rem';

    public constructor(style: LengthStyle | string) {
      if(typeof style == 'string') {
        const parsed = ParsedLengthStyle.parseLengthStyle(style);
        this.val = parsed.val;
        this.absolute = parsed.absolute;
        if(parsed.special) {
          this.special = parsed.special;
        }
      } else {
        this.val = style.val;
        this.absolute = style.absolute;
        if(style.special) {
          this.special = style.special;
        }
      }
    }

    public get styleString(): string {
      if(this.absolute) {
        return this.val + 'px';
      } else if(this.special) {
        // Only 'em' and 'rem' are allowed, and both may be treated similarly.
        // Both relate to font sizes, though the path to the reference element
        // differs between them.
        return this.val + this.special;
      } else {
        return (this.val * 100) + '%';
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

    public static special(val: number, suffix: 'em' | 'rem'): ParsedLengthStyle {
      return new ParsedLengthStyle({val: val, absolute: false, special: suffix});
    }

    private static parseLengthStyle(spec: string): LengthStyle {
      var val: number;
  
      if(spec.indexOf('px') != -1) {
        val = parseFloat(spec);
        return {val: val, absolute: true};
      } else if(spec.indexOf('pt') != -1) {
        // 16 px ~= 12 pt.
        // Reference: https://kyleschaeffer.com/css-font-size-em-vs-px-vs-pt-vs-percent
        val = parseFloat(spec);
        return {val: (4 * val / 3), absolute: true};
      } else if(spec.indexOf('%') != -1) {
        val = parseFloat(spec);
        return {val: val/100, absolute: false};
      } else if(!isNaN(val = Number(spec))) {
        // Note:  this one is NOT natively handled by browsers!
        //        We'll treat it as if it were 'pt', since that's likely the user's
        //        most familiar font size unit.
        return {val: (4 * val / 3), absolute: true};
      } else if(spec.indexOf('rem') != -1) {
        val = parseFloat(spec);
        return {val: val, absolute: false, special: 'rem'};
      } else if(spec.indexOf('em') != -1) {
        val = parseFloat(spec);
        return {val: val, absolute: false, special: 'em'};
      } else {
        // Cannot parse.
        console.error("Could not properly parse specified length style info: '" + spec + "'.");
        return null;
      }
    }
  }
}