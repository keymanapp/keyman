---
title: TSV files
---

Used by:
:   <span class="application">Keyman Developer</span> and
    <span class="application">Lexical Model compiler</span>.

Description:
:   A .tsv file or a <span class="dfn">tab-separated values file</span>
    contains a word list. This word list is used to predict and correct
    words using the predictive text functionality.

Details:
:   A .tsv file is a plain-text file containing of tabular data.
    Spreadsheet programs such as Microsoft Excel and Google Sheets can
    export into TSV format. TSVs can also be programmatically generated
    from other data sources. For advanced users, see [File
    Format](#file-format) for more details.

Distributed with lexical model:
:   No. This is a development file and should not be distributed.

## File format

**For advanced users only**: this documentation is intended for users
who wish to develop their own word list exporters. Most users can use
existing word list exporters.

The **lexical model compiler** expects word lists to abide by the
following **tab-separated values** (TSV) format:

-   the file is a **UTF-8** encoded text file
-   newlines are either **LF** or **CRLF**
-   the file **MAY** start with the UTF-8 **byte-order mark** (BOM);
    that is, if the first three bytes of the file are `EF BB BF`, these
    will be interpreted as the BOM and will be ignored.
-   the file either consists of a **comment** or an **entry**
-   **comment** lines MUST start with the `#` character on the very
    first column
-   **entries** are one to three columns, separated by the (horizontal)
    **tab character** (U+0009)
-   column 1 (**REQUIRED**): the **word form**: can have any character
    except tab, CR, or LF. Surrounding whitespace characters are
    trimmed. Quote characters (`'` or `"`) are **NOT** required to
    surround the text and are **NOT** parsed in any special manner.
-   column 2 (*optional*): the **count**: a non-negative integer
    specifying how many times this entry has appeared in the corpus.
    Blank means ‘indeterminate’, and is treated as if the word exists in
    the corpus, but will be predicted at the lowest possible priority.
-   column 3 (*optional*): **comment**: an informative comment, ignored
    by this tool.

Source:
[build-trie.js@029fb7c8](https://github.com/keymanapp/keyman/blob/029fb7c822c5a5619eaca845ecd2e5a2497d3056/developer/js/lexical-model-compiler/build-trie.ts#L21-L40)

## Additional notes

Exporting a spreadsheet from Google Sheets as a TSV will produce
properly formatted output.
