# ldml-keyboards

Data will eventually be versioned, so there would be a `42`, `43` etc directory.

Currently there is a `techpreview` directory referring to the technical preview—work in progress work in 2022.
That will very roughly correspond to <https://github.com/unicode-org/cldr/tree/keyboard-preview>, or pull requests thereunto.

## Data Format

Each directory contains:

- `ldmlKeyboard.dtd` - the DTD file
- `ldmlKeyboard.xsd` - the XSD file, automatically converted from the DTD using
  Visual Studio, hand tweaked as necessary
- `ldml-keyboard.schema.json` - the JSON schema file, automatically converted
  from the XSD using xsd2json (https://github.com/Mermade/jgeXml), hand tweaked
  as necessary:
    - change toplevel "id" to "$id"
    - add "type": "object" to /keyboard
    - change /keyboard/keys/key type to "array"

- `imports/` - the importable data files (TODO-LDML)
