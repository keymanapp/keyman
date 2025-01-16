/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { KMXPlus } from "@keymanapp/common-types";
import { KeymanXMLWriter, LDMLKeyboard } from "@keymanapp/developer-utils";
import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KeysCompiler } from "../compiler/keys.js";

export function kmxToXml(kmx: KMXPlus.KMXPlusFile): string {
  const writer = new KeymanXMLWriter("keyboard3");
  const { kmxplus } = kmx;
  const {
    //    sect,
    bksp,
    disp,
    //    elem,
    keys,
    layr,
    //    list,
    loca,
    meta,
    //    strs,
    tran,
    //    uset,
    vars,
  } = kmxplus;
  const data = {
    keyboard3: {
      ...getRootAttributes(),
      ...getLocales(),
      version: getVersion(),
      info: getInfo(),
      ...getDisplays(),
      ...getKeys(),
      ...getFlicks(),
      ...getLayers(),
      ...getVariables(),
      ...getTransforms(),
    }
  };

  return writer.write(data);

  function getRootAttributes() {
    return {
      '$xmlns': `https://schemas.unicode.org/cldr/${constants.cldr_version_latest}/keyboard3`,
      '$locale': kmx.kmxplus.loca.locales[0].value,
      '$conformsTo': constants.cldr_version_latest,
    };
  }

  function getLocales() {
    if (loca?.locales?.length < 2) {
      return {}; // no additional locales
    } else {
      return {
        locales:
          loca.locales.map(({ value }) => ({ '$id': value })),
      }
    }
  }

  function getInfo() {
    return {
      '$author': meta.author.value,
      '$name': meta.name.value,
      '$layout': meta.layout.value,
      '$indicator': meta.indicator.value,
    };
  }

  function getVersion() {
    return { '$value': kmx.kmxplus.meta.version.value };
  }

  function getDisplays() {
    const displays = {
      display: disp?.disps.map(disp => getDisplay(disp)) || [],
      ...getDisplaySettings(),
    };
    if (displays?.display?.length || displays?.displayOptions) {
      return { displays }
    } else {
      return {};
    }
  }

  function stringToAttr(attr: string, s?: KMXPlus.StrsItem) {
    if (!s || !s?.value?.length) return {};
    return Object.fromEntries([[`\$${attr}`, s.value]]);
  }

  function asAttr(attr: string, s?: any) {
    if (s === undefined) return {};
    return Object.fromEntries([[`\$${attr}`, s]]);
  }

  function numberToAttr(attr: string, s?: number) {
    if (s === undefined) return {};
    return Object.fromEntries([[`\$${attr}`, s.toString()]]);
  }

  function getDisplay(disp: KMXPlus.DispItem) {
    return {
      ...stringToAttr('output', disp?.to),
      ...stringToAttr('keyId', disp?.id),
      ...stringToAttr('display', disp?.display),
    };
  }

  function getDisplaySettings() {
    if (!disp?.baseCharacter?.value) return {};
    return {
      displayOptions: {
        '$baseCharacter': disp?.baseCharacter?.value,
      }
    };
  }

  function getKeys() {
    if (!keys?.keys?.length) {
      return {};
    }
    return {
      keys: {
        key: keys.keys
          // skip reserved keys (gap)
          .filter((key: KMXPlus.KeysKeys) =>
            !KeysCompiler.isReserved(key) &&
            !LDMLKeyboard.ImportStatus.isImpliedImport(key))
          .map((key: KMXPlus.KeysKeys) => ({
            ...stringToAttr('id', key.id),
            ...stringToAttr('output', key.to),
          })),
      },
    };
  }

  function getFlicks() {
    // skip the null flicks
    if (keys?.flicks?.length < 2) {
      return {};
    }
    return {
      flicks: {
        // keys.key..
      }
    };
  }

  function getLayers() {
    if (!layr?.lists?.length) {
      return {};
    }
    return {
      layers: layr.lists.map(({ hardware, minDeviceWidth, layers }) => ({
        ...stringToAttr('formId', hardware),
        ...numberToAttr('minDeviceWidth', minDeviceWidth),
        layer: layers.map(({ id, mod, rows }) => ({
          ...stringToAttr('id', id),
          ...asAttr('modifiers', modToString(mod)),
          row: rows.map(({ keys }) => ({
            ...asAttr('keys', keys.map(({ value }) => value).join(' ')),
          })),
        })),
      })),
    };
  }

  function getVariables() {
    if (!vars?.strings.length && !vars?.sets.length && !vars?.usets.length) {
      return {};
    }
    function varToObj(v: KMXPlus.VarsItem): any {
      const { id, value } = v;
      return {
        ...stringToAttr('id', id),
        ...stringToAttr('value', value),
      };
    }
    function varsToArray(vars: KMXPlus.VarsItem[]): any[] {
      return vars.map(varToObj);
    }
    const { strings, sets, usets } = vars;

    return {
      variables: {
        string: varsToArray(strings),
        set: varsToArray(sets),
        uset: varsToArray(usets),
      },
    };
  }

  function getTransforms() {
    return {
      transforms: [
      ...getTransformType("simple", tran),
      ...getTransformType("backspace", bksp),
      ],
    };
  }

  /** NB: Bksp is a child class of Tran */
  function getTransformType(type: string, t: KMXPlus.Tran) {
    if (!t?.groups?.length) {
      return [];
    }
    const { groups } = t;
    return [{
        ...asAttr('type', type),
        transformGroup: groups.map((group) => {
          if (group.type === constants.tran_group_type_transform) {
            return {
              transform: group.transforms.map(({from, to}) => ({
                ...stringToAttr('from', from),
                ...stringToAttr('to', to),
              })),
            };
          } else if(group.type === constants.tran_group_type_reorder) {
            return {
              transform: group.reorders.map(({before, elements}) => ({
                ...asAttr('before', before.toString()),
                ...asAttr('from', elements.toString()),
              })),
            };
          } else {
            throw Error(`Invalid tran.group.type ${group.type}`);
          }
        }),
    }];
  }
}


/** convert a keys_mod value to a space-separated string list */
function modToString(mod: number) {
  // first try exact match
  const matches: string[] = [];
  for (const [name, value] of constants.keys_mod_map.entries()) {
    if (mod === value) return name; // exact match
    if (mod & value) matches.push(name);
  }
  return matches.sort().join(' ');
}
