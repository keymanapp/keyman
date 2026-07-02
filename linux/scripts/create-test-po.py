#!/usr/bin/python3
# This script is used to convert all strings in a .po file to uppercase.
# It is used to test the .po file translations.
import polib
import sys
import re


# Requires installation of python3-polib

def uppercase_except_braces(s):
    # Replace all text outside braces and %... (to next space) with uppercase, keep inside braces and %... as is
    def repl(match):
        text = match.group(0)
        if text.startswith('{') and text.endswith('}'):
            return text
        return text if text.startswith('%') else text.upper()

    # This regex splits the string into:
    # - brace blocks: { ... }
    # - percent blocks: %... (up to next space or end of string)
    # - everything else
    pattern = r'\{[^}]*\}|%[^\s]*|[^{%]+'
    return re.sub(pattern, repl, s)


if len(sys.argv) != 3:
    print(f"Usage: {sys.argv[0]} <file.po> <outfile.po>")
    sys.exit(1)

po = polib.pofile(sys.argv[1])

for entry in po:
    # Only update if not a fuzzy translation and not the header
    if not entry.obsolete and entry.msgid and not entry.msgid_plural:
        entry.msgstr = uppercase_except_braces(entry.msgid)
    elif not entry.obsolete and entry.msgid:
        # For plural forms
        for idx in entry.msgstr_plural:
            entry.msgstr_plural[idx] = uppercase_except_braces(entry.msgid_plural)

po.save(f"{sys.argv[2]}")
print(f"Updated file written to {sys.argv[2]}")
