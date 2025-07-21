
# Keylayout

This is to get a **keylayout.schema.json** for validating keylayout files


- We first need to have a `.XSD file` created from a `.keylayout file` (which is an xml-file type )
<small><span style="color: grey"> e.g. resources\standards-data\keylayout\dtd\keylayout.xsd  </small> <br></br>

- Then we need to run `./create_keylayout_schema.sh` to obtain a `.schema.json` file
<small><span style="color: grey"> e.g resources\standards-data\keylayout\create_keylayout_schema.sh  </small> <br></br>

- Then we need to run `build.sh configure` of `common/web/types/` which will create a `schema.validate.mjs` file 
<small><span style="color: grey"> e.g. common/web/types/build.sh configure  </small> <br></br>


- The .mjs can then be used via `SchemaValidators` to validate keylayout files
<small><span style="color: grey"> e.g. SchemaValidators.default.keylayout(source)  </small> <br></br>

