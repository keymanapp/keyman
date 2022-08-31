import { constants } from "@keymanapp/ldml-keyboard-constants";
import { Loca } from "../kmx/kmx-plus";
import { SectionCompiler } from "./section-compiler";
import { CompilerErrors } from "./errors";
import { LKKeyboard } from "../ldml-keyboard/ldml-keyboard-xml";

export class LocaCompiler extends SectionCompiler {

  public get id() {
    return constants.section.loca;
  }

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
          this.callbacks.reportMessage(CompilerErrors.ERROR_InvalidLocale, `Invalid BCP 47 locale form '${tag}'`);
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

    let locales = this.getLocales(this.keyboard);
    // TODO: remove `as any` cast: (Intl as any): ts lib version we have doesn't
    // yet include `getCanonicalLocales` but node 16 does include it so we can
    // safely use it. Also well supported in modern browsers.
    result.locales = (Intl as any).getCanonicalLocales(locales);

    if(result.locales.length < locales.length) {
      // TODO-LDML: hint on repeated locales
    }

    // TODO-LDML: do we do any locale normalization (e.g. en-Latn-US => en-US)

    return result;
  }
}
