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
    let parsed: LengthStyle = (typeof style == 'string') ? ParsedLengthStyle.parseLengthStyle(style) : style;

    const fallback: LengthStyle = {
      val: 1, // 100%
      absolute: false
    };

    if(typeof style == 'number') {
      // Addresses #13766 for compiled layouts with numbers in the touch-layout `fontsize` field

      // Do not actually emit console warning; we handle those as "errors" in the mobile apps, and
      // this is more an issue with the compiled keyboard.
      parsed = fallback;
    }

    if(isNaN(parsed.val)) {
      console.error("NaN appeared while trying to calculate style scaling effects");
      parsed = fallback
    }

    // While Object.assign would be nice (and previously, was used), it will break
    // on old but still supported versions of Android if their Chrome isn't updated.
    // Requires mobile Chrome 45+, but API 21 (5.0) launches with an older browser.

    // Object.assign(this, parsed);
    this.val = parsed.val;
    this.absolute = parsed.absolute;
    if(parsed.special) {
      this.special = parsed.special;
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
    if(isNaN(scalar)) {
      console.error("NaN appeared while trying to calculate style scaling effects");
      return this;
    }

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
    if(spec == '') {
      return CONSTANT_STYLE;
    }

    const val = parseFloat(spec);

    if(isNaN(val)) {
      // Cannot parse.
      console.error("Could not properly parse specified length style info: '" + spec + "'.");
      return CONSTANT_STYLE;
    }

    return spec.indexOf('px') != -1 ? {val: val, absolute: true} :
      // 16 px ~= 12 pt.
      // Reference: https://kyleschaeffer.com/css-font-size-em-vs-px-vs-pt-vs-percent
      spec.indexOf('pt') != -1 ? {val: (4 * val / 3), absolute: true} :
      spec.indexOf('%') != -1 ? {val: val/100, absolute: false} :
      spec.indexOf('rem') != -1 ? {val: val, absolute: false, special: 'rem'} :
      spec.indexOf('em') != -1 ?  {val: val, absolute: false, special: 'em'} :
      // At this point, assuming either Number or number in a string without units
      // Note:  this one is NOT natively handled by browsers!
      //        We'll treat it as if it were 'pt', since that's likely the user's
      //        most familiar font size unit.
      {val: (4 * val / 3), absolute: true};
  }
}

const CONSTANT_STYLE = new ParsedLengthStyle('1em');