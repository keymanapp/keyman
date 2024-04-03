# How to update KMXPlus sections

A diary.
By Steven R. Loomis

Keyman Section Update Journal

working on ‘layr’, using ‘disp’ as a model from https://github.com/keymanapp/keyman/pull/7568

## Constants and Scaffolding

- *Edit/Commit*: `core/include/ldml/keyman_core_ldml.ts`
    - update `SectionIdent` and keep in order: `'layr' |`
    - update `Constants.section` (near the end of the file) and keep in order: `'layr': 'layr',`
    - add a comment block in order `layr section`
        - add `length_layr` with the nonvariable length
        - add a `length_layr_*` for each subitem
        - add parameters for each flag/bitfield
    - Check indentation, check for copypasta errs!
- Run: `./core/tools/ldml-const-builder/build.sh clean build run`
- Verify/Commit: `core/include/ldml/keyman_core_ldml.h`

## XML changes

- `resources/standards-data/ldml-keyboards/45/`  : update / reimport / fix fixup script if needed
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
    - By the way, you might get errors such as “Error validating LDML XML file: /keyboard/transforms/0: additionalProperties: must NOT have additional properties additionalProperty="transform"” if the boxing functions don't match the schema. `boxArrays()` might be trying to add empty objects/arrays that aren't (anymore) legal per schema

## In-memory data: Phase 2

- first the compiler tests
    - add a section in `developer/src/kmc-ldml/test/fixtures/sections` if needed
    - add a test case such as `developer/src/kmc-ldml/test/test-key2.ts`
- add a compiler
    - `developer/src/kmc-ldml/compiler/key2.ts`
    - make sure the compiler has an `id()` function returning the correct id, such as `key2`
    - link it in to `developer/src/kmc-ldml/src/compiler/compiler.ts`
        - add the import
        - add to `SECTION_COMPILERS`

    - Note: the in-memory compiler can affect `basic.kmx` even _before_ you add the section writing code. How? Simple… `Strs.allocString()` is called for the in-memory structures, so the string table will start growing even before those strings are actually used by the new sections.  This is why it's fine to ignore the basic failure until you actually do the KMXPlus write.

## Writing out

- The moment you've been waiting for! Crack open `common/web/types/src/kmx/kmx-plus-builder/kmx-plus-builder.ts` and do it.
    - Add `import { BUILDER_DISP, build_disp } from './build-disp.js';` to the top (and a new file to go with it) — and, in order
    - Add `private sect_disp: BUILDER_DISP` to `class  KMXPlusBuilder {` — and, in order
    - add `this.sect_disp = build_disp(this.file.kmxplus, this.sect_strs);` to the `build()` function.  Include any other sections that need to be cross referenced.
    - Update `finalize_sect` and add `offset = this.finalize_sect_item(this.sect_disp, offset);`
    - Finally, add `this.emitSection(file, this.file.COMP_PLUS_DISP, this.sect_disp);` to `compile()` — and, in order.
        - Note that some variable length parts (such as the actual text data in `strs`) are sometimes in a separate emit function. Anything that's not in the `COMP_PLUS_STRS` `r.Struct` definition needs one of these.
        - Also note that restructure will happily ignore (write zeros for) any fields where the BUILDER_* fields don't match the COMP_PLUS_* fields.
    - Pro Tip: If you see `Error: Not a fixed size` (or have any other data generation issues), double check that the `COMP_PLUS_DISP*` entries in `kmx-plus.ts` has _exactly_ the same properties (and property names!) as the `BUILDER_DISP*` structures in your `build-disp.ts` file. Restructure will happily insert zeros on mismatches, but complains if a variable length item doesn't match.

- update basic.xml and basic.txt
    - Tweak `eveloper/src/kmc-ldml/test/fixtures/basic.xml` as needed
    - You can use `developer/src/kmc-ldml/build.sh build-fixtures` which will generate these. The two .kmx files are supposed to match: if not, fix `basic.txt` or fix other bugs.
        - `developer/src/kmc-ldml/build/test/fixtures/basic-txt.kmx` - KMX generated from basic.txt.
        - `developer/src/kmc-ldml/build/test/fixtures/basic-xml.kmx` - KMX generated from basic.xml.
        - `developer/src/kmc-ldml/build/test/fixtures/basic-xml.kvk` - KVK generated from basic.xml.

## more to come
