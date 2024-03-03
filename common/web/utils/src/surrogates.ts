/*
 * The definitions below are duplicated from common/web/types/util/util.ts;
 * we can't downcompile the originals to ES5 when bundling with esbuild.
 * `import type` stuff is fine, but not non-type `import` statements.
 *
 * TODO:  Use those instead, once we're no longer building ES5 versions of Web.
 */

export const Uni_LEAD_SURROGATE_START = 0xD800;
export const Uni_LEAD_SURROGATE_END = 0xDBFF;
export const Uni_TRAIL_SURROGATE_START = 0xDC00;
export const Uni_TRAIL_SURROGATE_END = 0xDFFF;

/**
 * @brief True if a lead surrogate
 * \def Uni_IsSurrogate1
 */
export function Uni_IsSurrogate1(ch : number) {
  return ((ch) >= Uni_LEAD_SURROGATE_START && (ch) <= Uni_LEAD_SURROGATE_END);
}
/**
 * @brief True if a trail surrogate
 * \def Uni_IsSurrogate2
 */
export function Uni_IsSurrogate2(ch : number) {
  return ((ch) >= Uni_TRAIL_SURROGATE_START && (ch) <= Uni_TRAIL_SURROGATE_END);
}
