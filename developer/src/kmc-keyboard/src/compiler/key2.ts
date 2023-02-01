import { constants } from '@keymanapp/ldml-keyboard-constants';
import { KMXPlus } from '@keymanapp/common-types';
import { SectionCompiler } from "./section-compiler.js";

import GlobalSections = KMXPlus.GlobalSections;
import Key2 = KMXPlus.Key2;
import ListItem = KMXPlus.ListItem;
import Key2Flicks = KMXPlus.Key2Flicks;
import { allUsedKeyIdsInLayers, calculateUniqueKeys } from '../util/util.js';


export class Key2Compiler extends SectionCompiler {

  public get id() {
    return constants.section.key2;
  }

  public validate() {
    let valid = true;
    // TODO-LDML: some validation needed here?
    return valid;
  }

  public compile(sections: GlobalSections): Key2 {
    if (!this.keyboard?.keys?.key && !this.keyboard?.keys?.flicks) {
      // short-circuit if no keys or flicks
      return null;
    }

    let sect = new Key2(sections.strs);

    // Load the flicks first
    this.loadFlicks(sections, sect);

    // Now, load the keys
    this.loadKeys(sections, sect);

    return sect;
  }

  public loadFlicks(sections: GlobalSections, sect: Key2) {
    for (let lkflicks of this.keyboard.keys.flicks) {
      let flicks: Key2Flicks = new Key2Flicks(sections.strs.allocString(lkflicks.id));

      for (let lkflick of lkflicks.flick) {
        let flags = 0;
        // TODO-LDML: single char
        const to = sections.strs.allocAndUnescapeString(lkflick.to);
        flags |= constants.key2_flick_flags_extend;
        let directions : ListItem = sections.list.allocListFromSpaces(sections.strs, lkflick.directions);
        flicks.flicks.push({
          directions,
          flags,
          to,
        });
      }

      sect.flicks.push(flicks);
    }
  }

  public loadKeys(sections: GlobalSections, sect: Key2) {
    const usedKeys = allUsedKeyIdsInLayers(this.keyboard?.layers);
    const uniqueKeys = calculateUniqueKeys([...this.keyboard.keys?.key]);

    for (let key of uniqueKeys) {
      if (!usedKeys.has(key.id)) {
        // TODO-LDML: linting for unused keys
        continue; // unused key, skip
      }
      let flags = 0;
      const flicks = key.flicks;
      // TODO-LDML: verify that this flick id exists
      if (!!key.gap) {
        flags |= constants.key2_key_flags_gap;
      }
      if (key.transform === 'no') {
        flags |= constants.key2_key_flags_notransform;
      }
      const id = sections.strs.allocString(key.id);
      const longPress: ListItem = sections.list.allocListFromEscapedSpaces(sections.strs, key.longPress);
      const longPressDefault = sections.strs.allocAndUnescapeString(key.longPressDefault);
      const multiTap: ListItem = sections.list.allocListFromEscapedSpaces(sections.strs, key.multiTap);
      const keySwitch = sections.strs.allocString(key.switch); // 'switch' is a reserved word
      flags |= constants.key2_key_flags_extend;
      const to = sections.strs.allocAndUnescapeString(key.to); // TODO-LDML: single char
      const width = Math.ceil((key.width || 1) * 10.0);  // default, width=1
      const vkey: any = 0; // TODO-LDML: fill in later
      sect.keys.push({
        flags,
        flicks,
        id,
        longPress,
        longPressDefault,
        multiTap,
        switch: keySwitch, // 'switch' is a reserved word
        to,
        vkey,
        width,
      });
    }
  }
}
