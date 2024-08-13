/** for a 4-byte section identity */
export type BUILDER_IDENT = number;
/** for a single UTF-32 character (Unicode codepoint) */
export type BUILDER_U32CHAR = number;

export interface BUILDER_SECTION {
  ident: BUILDER_IDENT;
  size: number;
  _offset: number; // used only for building the output
}
;
