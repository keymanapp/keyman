#!/usr/bin/python3
#
# Keyman is copyright (C) SIL Global. MIT License.
# 
# Created by Marc Durdin on 2026-08-18
# 
# Find the current display locale ID, using the same method as
# gettext.find()
# 

import os
import re

#
# @returns array of language identifiers in POSIX style, i.e. 'en_US', not 'en-US'.
#
# The returned array always includes the value 'C' for the default static locale,
# which should be replaced with 'en' when passed to other processes
#
def find_locales():
    # Approach comes from gettext.find()
    languages = []
    for envvar in ('LANGUAGE', 'LC_ALL', 'LC_MESSAGES', 'LANG'):
        val = os.environ.get(envvar)
        if val:
            # technically only LANGUAGE should have multiple entries,
            # but we match gettext's implementation for consistency
            languages = val.split(':')
            break
    if 'C' not in languages:
        languages.append('C')

    # the env vars may also have .encoding or @modifier suffixes that we don't 
    # care about
    # https://www.linux.com/news/controlling-your-locale-environment-variables/
    locale_ids = []
    for lang in languages:
        locale_id = re.split(r"[.@]", lang)
        locale_ids.append(locale_id[0])

    return locale_ids
