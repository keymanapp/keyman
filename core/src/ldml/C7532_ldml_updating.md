# How to update KMXPlus sections

A diary.
By Steven R. Loomis

Keyman Section Update Journal

working on ‘layr’, using ‘disp’ as a model from https://github.com/keymanapp/keyman/pull/7568

## Constants and Scaffolding

- *Edit/Commit*: `core/include/ldml/keyboardprocessor_ldml.ts`
    - update `SectionIdent` and keep in order
    - update `SectionMap` and keep in order
    - add a comment block in order `layr section`
        - add `length_layr` with the nonvariable length
        - add a `length_layr_*` for each subitem
        - add parameters for each flag/bitfield
    - Check indentation, check for copypasta errs!
- Run: `./core/tools/ldml-const-builder/build.sh clean build run`
- Verify/Commit: `core/include/ldml/keyboardprocessor_ldml.h`

## XML changes

- `resources/standards-data/ldml-keyboards/techpreview/`  : update / reimport / fix fixup script if needed
- E/C: `common/web/types/src/ldml-keyboard/ldml-keyboard-xml.ts`
    - add to `LKKeyboard` and subproperties as needed to support the structure on the XML side
- Now would be a good time to stop and make sure everything compiles. It didn’t, there was an unrelated issue with snprintf!

## In-memory data: Phase 1

- `common/web/types/src/kmx/kmx-plus.ts`
    - update `KMXPlusData` to include new section
    - add the new section and any in-memory data for the compiler
    - It’s enough temporarily to add `export class Sect extends Section{ /* TODO-LDML */};` for now so that it compiles, and come back to it
- `core/src/kmx/kmx_plus.h`
    - add new structs
- `core/src/kmx/kmx_plus.cpp`
    - add validate implementation for the section and any new structs
    - update `kmx_plus::kmx_plus()` to include the loader
- `common/web/types/src/kmx/kmx-plus.ts`
    - Also update class KMXPlusFile to include the actual binary format (coordinate with `kmx_plus.h`)
- E/C `common/web/types/src/ldml-keyboard/ldml-keyboard-xml-reader.ts` if there’s any changing to the boxing needed

## In-memory data: Phase 2

- first the compiler tests
    - add a section in `developer/src/kmc-keyboard/test/fixtures/sections` if needed
    - add a test case such as `developer/src/kmc-keyboard/src/compiler/layr.ts`
- add a compiler
    - `developer/src/kmc-keyboard/compiler/key2.ts`

## Writing out

- update basic.xml and basic.txt
    - TODO-LDML: regen fixtures
    - … BUT DO NOT CHECK THEM IN!

## more to come
