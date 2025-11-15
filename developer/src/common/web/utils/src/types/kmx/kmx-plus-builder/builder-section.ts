import { KMXPlusVersion } from "@keymanapp/ldml-keyboard-constants";

/** for a 4-byte section identity */
export type BUILDER_IDENT = number;
/** for a single UTF-32 character (Unicode codepoint) */
export type BUILDER_U32CHAR = number;

export interface BUILDER_SECTION {
  header: {
    ident: BUILDER_IDENT;
    size: number;
    version?: KMXPlusVersion; // v19+
  };
  _offset: number; // used only for building the output
}
;
