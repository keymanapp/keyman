
# Keylayout

This is to get a **keylayout.schema.json** for validating keylayout files


- We first need to have a `.XSD file` created from a `.keylayout file` (which is an xml-file type )
`   e.g. resources\standards-data\keylayout\dtd\keylayout.xsd`


- Then we need to run `./create_keylayout_schema.sh` to obtain a `.schema.json` file
`    e.g resources\standards-data\keylayout\create_keylayout_schema.sh`


- Then we need to run `build.sh configure` of `common/web/types/` which will create a `schema.validate.mjs` file 
`   e.g. common/web/types/build.sh configure`


- The .mjs can then be used via `SchemaValidators` to validate keylayout files
`   e.g. SchemaValidators.default.keylayout(source)`

