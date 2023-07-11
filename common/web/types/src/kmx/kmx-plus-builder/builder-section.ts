export type BUILDER_IDENT = number;
export interface BUILDER_SECTION {
  ident: BUILDER_IDENT;
  size: number;
  _offset: number; // used only for building the output
}
;
