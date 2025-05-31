import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlus } from '@keymanapp/common-types';
import { LDMLKeyboard, ObjectWithMetadata } from '@keymanapp/developer-utils';
import { SectionCompiler } from "./section-compiler.js";
import { LdmlCompilerMessages } from "./ldml-compiler-messages.js";

import DependencySections = KMXPlus.DependencySections;
import Loca = KMXPlus.Loca;
import LKKeyboard = LDMLKeyboard.LKKeyboard;

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
    [keyboard.locale].concat(Array.isArray(keyboard.locales?.locale) ? keyboard.locales.locale.map(v => v.id) : []);

  /**
   * Attempt to locate context metadata for a locale, for error messages
   * @param loc locale tag
   * @returns context metadata or null
   */
  private contextForLocale(loc: string) : ObjectWithMetadata {
    if (loc == this.keyboard3.locale) return this.keyboard3;
    return this.keyboard3?.locales;
  }

  public validate(): boolean {
    let valid = true;
    const locales = this.getLocales(this.keyboard3);
    for(const tag of locales) {
      try {
        new Intl.Locale(tag);
      } catch(e) {
        if(e instanceof RangeError) {
          this.callbacks.reportMessage(LdmlCompilerMessages.Error_InvalidLocale({tag}, this.contextForLocale(tag)));
          valid = false;
        } else {
          /* c8 ignore next 2 */
          throw e;
        }
      }
    }
    return valid;
  }

  public compile(sections: DependencySections): Loca {
    const result = new Loca();

    // This also minimizes locales according to Remove Likely Subtags algorithm:
    // https://www.unicode.org/reports/tr35/#Likely_Subtags
    const sourceLocales = this.getLocales(this.keyboard3);
    const locales = sourceLocales.map((sourceLocale: string) => {
      const locale = new Intl.Locale(sourceLocale).minimize().toString();
      if(locale != sourceLocale) {
        this.callbacks.reportMessage(LdmlCompilerMessages.Hint_LocaleIsNotMinimalAndClean({ sourceLocale, locale },
        this.contextForLocale(sourceLocale)));
      }
      return locale;
    });

    // TODO: remove `as any` cast: (Intl as any): ts lib version we have doesn't
    // yet include `getCanonicalLocales` but node 16 does include it so we can
    // safely use it. Also well supported in modern browsers.
    const canonicalLocales = (Intl as any).getCanonicalLocales(locales) as string[];
    result.locales = canonicalLocales.map(locale => sections.strs.allocString(locale));

    if(result.locales.length < locales.length) {
      this.callbacks.reportMessage(LdmlCompilerMessages.Hint_OneOrMoreRepeatedLocales(this.keyboard3?.locales));
    }

    return result;
  }
}
