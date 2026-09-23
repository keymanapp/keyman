
# Keylayout

A keylayout file is a configuration file used macOS to define custom keyboard mappings, mapping physical key presses to specific characters or Unicode symbols. These XML-based files, created with Ukelele, allow users to create custom layouts for different languages or special haracters, such as mapping Option + Key combinations. With kmc-convert we can convert these keylayout files to .kmn files which can be used in Keyman keyboards. 
For validating a .keylayout file a keylayout.schema.json is needed.

This is to get a **keylayout.schema.json**:


- We first need to have a `.XSD file` created from a `.keylayout file` (which is an xml-file type )
`   e.g. resources\standards-data\keylayout\dtd\keylayout.xsd`


- Then we need to run `./create_keylayout_schema.sh` to obtain a `.schema.json` file
`    e.g resources\standards-data\keylayout\create_keylayout_schema.sh`


- Then we need to run `build.sh configure` of `common/web/types/` which will create a `schema.validate.mjs` file 
`   e.g. common/web/types/build.sh configure`


- The .mjs can then be used via `SchemaValidators` to validate keylayout files
`   e.g. SchemaValidators.default.keylayout(source)`

