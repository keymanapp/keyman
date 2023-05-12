import { constants } from "@keymanapp/ldml-keyboard-constants";
import { KMXPlusData } from "../kmx-plus.js";
import { build_strs_index, BUILDER_STRS } from "./build-strs.js";
import { BUILDER_SECTION } from "./builder-section.js";


interface BUILDER_VARS_ITEM {
  type: number;
  id: number; // str
  value: number; // str
  elem?: number; // elem, TODO-LDML
};

export interface BUILDER_VARS extends BUILDER_SECTION {
  markers: number; // list, TODO-LDML
  varCount: number;
  varEntries: BUILDER_VARS_ITEM[];
};

/**
* Builder for the 'vars' section
*/
export function build_vars(kmxplus: KMXPlusData, sect_strs: BUILDER_STRS /*, sect_elem: BUILDER_ELEM*/) : BUILDER_VARS {
  if(!kmxplus.vars) {
    return null;
  }

  const stringVars = kmxplus.vars.strings.map(v => <BUILDER_VARS_ITEM>{
    type: constants.vars_entry_type_string,
    id: build_strs_index(sect_strs, v.id),
    value: build_strs_index(sect_strs, v.value),
  });
  const setVars = kmxplus.vars.sets.map(v => <BUILDER_VARS_ITEM>{
    type: constants.vars_entry_type_set,
    id: build_strs_index(sect_strs, v.id),
    value: build_strs_index(sect_strs, v.value),
  });
  const uniSetVars = kmxplus.vars.unicodeSets.map(v => <BUILDER_VARS_ITEM>{
    type: constants.vars_entry_type_unicodeSet,
    id: build_strs_index(sect_strs, v.id),
    value: build_strs_index(sect_strs, v.value),
  });

  const vars: BUILDER_VARS = {
    ident: constants.hex_section_id(constants.section.vars),
    size: constants.length_vars +
      (constants.length_vars_item * kmxplus.vars.totalCount()),
    _offset: 0,
    markers: 0,
    varCount: kmxplus.vars.totalCount(),
    varEntries: [
      ...stringVars,
      ...setVars,
      ...uniSetVars,
    ],
  };
  // sort IDs by binary order
  vars.varEntries.sort((a,b) => a.id - b.id);

  return vars;
}
