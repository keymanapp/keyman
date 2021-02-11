#!/usr/bin/python3

from keyman_config.bcp47tag import Bcp47Tag
from keyman_config.standards.lang_tags_map import LangTagsMap


class CanonicalLanguageCodeUtils():
    def findBestTag(tag, addRegion, addScriptIfNotSuppressed):
        """
        Find a language code with appropriate script and region subtags</summary>

        This will canonicalize known tags, then apply rules to ensure script subtag
        is present if not suppressed, and add a default region if none given.
        """
        # See also windows/src/global/delphi/general/Keyman.System.CanonicalLanguageCodeUtils.pas
        if not tag:
            return None

        bcp47Tag = Bcp47Tag.create(tag)
        if not bcp47Tag:
            return None

        # Special case for IPA keyboards otherwise we'd end up with und-Zyyy-fonipa
        if bcp47Tag.language == 'und' and bcp47Tag.variant[0] == 'fonipa':
            return 'und-fonipa'

        # First, canonicalize any unnecessary ISO639-3 codes
        bcp47Tag.language = LangTagsMap.translateISO6393ToBCP47(bcp47Tag.language)

        # Lookup the tag first, canonicalize to the base tag for known tags
        result = LangTagsMap.lookupAllTags(bcp47Tag.tag)
        if result:
            bcp47Tag.tag = result

        langTag = LangTagsMap.lookupLangTags(bcp47Tag.tag)
        if not langTag:
            # Not a known tag but perhaps it's a custom language
            # We'll make no further assumptions
            return bcp47Tag.tag

        # Then, lookup the lang-script and see if there is a suppress-script
        # Or add the default script in if it is missing and not a suppress-script
        if not bcp47Tag.script and not langTag['suppress'] and addScriptIfNotSuppressed:
            bcp47Tag.script = langTag['script']

        # Add the region if not specified
        if not bcp47Tag.region and addRegion:
            bcp47Tag.region = langTag['region']

        return bcp47Tag.tag
