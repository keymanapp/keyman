/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Wraps langtags.json as a Javascript object
 */

import langtags from './imports/langtags.js';

// These interfaces and types correspond to https://github.com/silnrsi/langtags/blob/master/source/langtags_schema.json

export type LangTagBcp47 = string; // BCP 47
export type LangTagBcp47Variant = string; // BCP 47 variant
export type LangTagIso639_3 = string;  // ISO639-3
export type LangTagIso3166_1 = string; // region
export type LangTagIso15924 = string; // script


export interface LangTag {
  full: string; // should be LangTagBcp47?
  iana?: string[];
  iso639_3?: LangTagIso639_3;
  latnnames?: string[];
  localname?: string;
  localnames?: string;
  macrolang?: LangTagBcp47;
  name?: string;
  names?: string[];
  nophonvars?: boolean;
  obsolete?: boolean;
  region?: LangTagIso3166_1;
  regionname?: string;
  regions?: LangTagIso3166_1[];
  rod?: string;
  script?: LangTagIso15924;
  sldr?: boolean;
  suppress?: boolean;
  tag?: LangTagBcp47;
  tags?: LangTagBcp47[];
  unwritten?: boolean;
  variants?: LangTagBcp47Variant[];
  windows?: LangTagBcp47;
};

export interface LangTagConformance {
  regions: LangTagIso3166_1[];
  scripts: LangTagIso15924[];
  tag: '_conformance';
};

export interface LangTagGlobalVar {
  variants: LangTagBcp47Variant[];
  tag: '_globalvar';
};

export interface LangTagPhonVar {
  variants: LangTagBcp47Variant[];
  tag: '_phonvar';
};

export interface LangTagVersion {
  api: string;
  date: string;
  tag: '_version';
};

/**
 * A map of all language tags, lower cased
 */
const langtagsByTag: Map<string, any> = new Map<string, any>();

/* metadata variables */
let conformance: LangTagConformance = undefined;
let globalvar: LangTagGlobalVar = undefined;
let phonvar: LangTagPhonVar = undefined;
let version: LangTagVersion = undefined;

const metadata = {
  conformance: () => ({ regions: [...conformance.regions], scripts: [...conformance.scripts] }),
  globalvar: () => ({ variants: [...globalvar.variants] }),
  phonvar: () => ({ variants: [...phonvar.variants] }),
  version: () => ({ api: version.api, date: version.date })
};

/**
 * Build a dictionary of language tags from langtags.json. This takes under 10ms
 * on a reasonable laptop.
 */
function preinit(): void {
  for(const tag of langtags) {
    if(tag.tag == '_conformance') {
      conformance = tag;
    } else if(tag.tag == '_globalvar') {
      globalvar = tag;
    } else if(tag.tag == '_phonvar') {
      phonvar = tag;
    } else if(tag.tag == '_version') {
      version = tag;
    } else if(tag.full && tag.tag) {
      langtagsByTag.set(tag.tag.toLowerCase(), tag);
      langtagsByTag.set(tag.full.toLowerCase(), tag);
      if(tag.tags) {
        for(const t of tag.tags) {
          langtagsByTag.set(t.toLowerCase(), tag);
        }
      }
    }
  }
}

/**
 * Find a LangTag from a given tag; matches on tag, tags, full properties,
 * with a case-insensitive search.
 * @param tag
 * @returns LangTag or undefined if not found
 */
function getLangtagByTag(tag: string): LangTag {
  return langtagsByTag.get(tag.toLowerCase());
}

preinit();

export { langtags, getLangtagByTag, metadata };
