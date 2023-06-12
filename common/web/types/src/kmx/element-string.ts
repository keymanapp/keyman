import { constants } from '@keymanapp/ldml-keyboard-constants';
import { Strs, StrsItem } from './kmx-plus.js';

export enum ElemElementFlags {
  none = 0,
  unicode_set = constants.elem_flags_unicode_set,
  tertiary_base = constants.elem_flags_tertiary_base,
  prebase = constants.elem_flags_prebase
}
;

export class ElemElement {
  value: StrsItem; // UnicodeSet or UCS32LE character
  order: number; // -128 to +127; used only by reorder element values
  tertiary: number; // -128 to +127; used only by reorder element values
  flags: ElemElementFlags;
  isEqual(a: ElemElement) {
    return a.value === this.value &&
      a.order === this.order &&
      a.tertiary === this.tertiary &&
      a.flags === this.flags;
  }
}
;

export class ElementString extends Array<ElemElement> {
  constructor(strs: Strs, source: string | string[], order?: string, tertiary?: string, tertiary_base?: string, prebase?: string) {
    super();
    //TODO-LDML: full UnicodeSet and parsing
    if(!source) {
      return;
    }

    let items = Array.isArray(source) ? source : source.split("");

    let orders: Array<string> = order ? order.split(" ") : [];
    if(orders.length == 1) {
      orders = Array<string>(items.length).fill(orders[0]);
    }

    let tertiaries: Array<string> = tertiary ? tertiary.split(" ") : [];
    if(tertiaries.length == 1) {
      tertiaries = Array<string>(items.length).fill(tertiaries[0]);
    }

    let tertiary_bases: Array<string> = tertiary_base ? tertiary_base.split(" ") : [];
    if(tertiary_bases.length == 1) {
      tertiary_bases = Array<string>(items.length).fill(tertiary_bases[0]);
    }

    let prebases: Array<string> = prebase ? prebase.split(" ") : [];
    if(prebases.length == 1) {
      prebases = Array<string>(items.length).fill(prebases[0]);
    }

    for(let i = 0; i < items.length; i++) {
      let elem = new ElemElement();
      elem.value = strs.allocString(items[i]);
      elem.order = orders.length ? parseInt(orders[i], 10) : 0;
      elem.tertiary = tertiaries.length ? parseInt(tertiaries[i], 10) : 0;
      elem.flags = ElemElementFlags.none |
        (tertiary_bases?.[i] == '1' /* TODO-LDML: or 'true'? */ ? ElemElementFlags.tertiary_base : 0) |
        (prebases?.[i] == '1' /* TODO-LDML: or 'true'? */ ? ElemElementFlags.prebase : 0);
      this.push(elem);
    };
  }
  isEqual(a: ElementString): boolean {
    if (a.length != this.length) {
      return false;
    }
    for (let i = 0; i < a.length; i++) {
      if (!this[i].isEqual(a[i])) {
        return false;
      }
    }
    return true;
  }
}
;
