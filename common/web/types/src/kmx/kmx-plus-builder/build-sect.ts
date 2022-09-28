import { constants } from "@keymanapp/ldml-keyboard-constants";
import { BUILDER_SECTION } from "./builder-section.js";

/* ------------------------------------------------------------------
 * sect section
   ------------------------------------------------------------------ */

interface BUILDER_SECT_ITEM {
  sect: number;
  offset: number; //? new r.VoidPointer(r.uint32le, {type: 'global'})
};

/**
 * Builder for the 'sect' (Section table of contents) section
 */
export interface BUILDER_SECT extends BUILDER_SECTION {
    total: number;
    count: number;
    items: BUILDER_SECT_ITEM[];
  };

export function build_sect(): BUILDER_SECT {
  return {
    ident: constants.hex_section_id(constants.section.sect),
    size: 0, // finalized later
    _offset: 0,
    total: 0, // finalized later
    count: 0, // finalized later
    items: [], // finalized later
  };
}
