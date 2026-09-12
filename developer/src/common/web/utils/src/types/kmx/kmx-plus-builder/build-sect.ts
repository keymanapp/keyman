import { constants, KMXPlusVersion } from "@keymanapp/ldml-keyboard-constants";
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

export function build_sect(version: KMXPlusVersion): BUILDER_SECT {
  return {
    header: {
      // v19+ uses ident "sec2" to indicate that we have a `version` header field
      ident: constants.hex_section_id(version == KMXPlusVersion.Version17 ? constants.section.sect : constants.sectionname_sec2),
      size: 0, // finalized later
      version,
    },
    _offset: 0,
    total: 0, // finalized later
    count: 0, // finalized later
    items: [], // finalized later
  };
}
