/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { KMXPlus } from "@keymanapp/common-types";
import { KeymanXMLWriter } from "@keymanapp/developer-utils";
import { constants } from "@keymanapp/ldml-keyboard-constants";

export function kmxToXml(kmx: KMXPlus.KMXPlusFile): string {
    const writer = new KeymanXMLWriter("keyboard3");
    const { kmxplus } = kmx;
    const {
        //    sect,
        //    bksp,
        disp,
        //    elem,
        //    keys,
        //    layr,
        //    list,
        loca,
        meta,
        //    strs,
        //    tran,
        //    uset,
        //    vars,
    } = kmxplus;
    const data = {
        keyboard3: {
            ...getRootAttributes(),
            ...getLocales(),
            version: getVersion(),
            info: getInfo(),
            ...getDisplays(),
            ...getKeys(),
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
            '$conffiormsTo': constants.cldr_version_latest,
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
        return {};
    }

    function getLayers() {
        return {};
    }

    function getVariables() {
        return {};
    }

    function getTransforms() {
        return {};
    }
}
