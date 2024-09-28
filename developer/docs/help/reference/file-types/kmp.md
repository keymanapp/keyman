---
title: KMP files
---

Used by:
:   All Keyman and custom Keyman Engine products can use keyboard
    packages.
:   Keyman for Android and Keyman for iPhone and iPad can use lexical
    model packages.

Description:
:   A .KMP file is a Keyman Package file for distributing keyboards or
    lexical models.

Details:
:   A .KMP file is compiled from a Keyman Package source file
    ([.KPS](kps)) using the Package Editor in
    <span class="application">Keyman Developer</span>. Normal contents
    of a Keyman keyboard Package are one or more keyboards with fonts,
    documentation, and On Screen Keyboard ([.KVK](kvk)) files. Keyman
    Developer will also include a [metadata](metadata) file in the
    package. Lexical model packages contain one lexical model instead of
    keyboards.

Distributed with keyboard:
:   A Keyman keyboard Package file (.KMP) can include keyboard files
    ([.KMX](kmx)/.JS), fonts, documentation, and On Screen Keyboard
    ([.KVK](kvk)) files. Do not include source files ([.KMN](kmn)/.KVKS)
    in the Package. The Keyman Package is normally distributed instead
    of the plain keyboard file ([.KMX](kmx)) in order to include the
    extra files.

Distributed with lexical model:
:   A Keyman lexical model Package file (.KMP) includes one lexical
    model file ([.MODEL.JS)](model-js), and documentation files. Do not
    include lexical model source files ([.MODEL.TS](model-ts) or
    [.TSV](tsv)) in the Package.
