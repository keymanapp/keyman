import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Loca } from "../kmx/kmx-plus";
import { SectionCompiler } from "./section-compiler";
import { CompilerErrors } from "./errors";
import { LKKeyboard } from "../ldml-keyboard/ldml-keyboard-xml";

export class LocaCompiler extends SectionCompiler {

  public get id() {
    return constants.section.loca;
  }

  /**
   *
   * @param keyboard
   * @returns list of BCP 47 tags in the keyboard xml, potentially with invalid or repeated entries
   */
  private getLocales =
    (keyboard: LKKeyboard) =>
    [keyboard.locale].concat(Array.isArray(keyboard.locales?.locale) ? keyboard.locales.locale.map(v => v.id) : [])

  public validate(): boolean {
    let valid = true;
    const locales = this.getLocales(this.keyboard);
    for(let tag of locales) {
      try {
        new Intl.Locale(tag);
      } catch(e) {
        if(e instanceof RangeError) {
          this.callbacks.reportMessage(CompilerErrors.InvalidLocale({tag}));
          valid = false;
        } else {
          throw e;
        }
      }
    }
    return valid;
  }

  public compile(): Loca {
    let result = new Loca();

    // This also minimizes locales according to Remove Likely Subtags algorithm:
    // https://www.unicode.org/reports/tr35/#Likely_Subtags
    const sourceLocales = this.getLocales(this.keyboard);
    const locales = sourceLocales.map((sourceLocale: string) => {
      const locale = new Intl.Locale(sourceLocale).minimize().toString();
      if(locale != sourceLocale) {
        this.callbacks.reportMessage(CompilerErrors.LocaleIsNotMinimalAndClean(sourceLocale, locale));
      }
      return locale;
    });

    // TODO: remove `as any` cast: (Intl as any): ts lib version we have doesn't
    // yet include `getCanonicalLocales` but node 16 does include it so we can
    // safely use it. Also well supported in modern browsers.
    result.locales = (Intl as any).getCanonicalLocales(locales);

    if(result.locales.length < locales.length) {
      this.callbacks.reportMessage(CompilerErrors.OneOrMoreRepeatedLocales());
    }

    return result;
  }
}
