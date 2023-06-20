# .kvks schema

This schema validates .kvks files, according to the reference implementation
from VisualKeyboardLoaderXML.pas.

## Notes on conversion from xsd to json-schema

Converted using xsd2json. Following structural changes:

* kvk-version base type from km-version to string, copy km-version pattern in
* remove xs:all bracketing
* remove format:double from fontsize, change type to string
* encoding property changed type to array
* kvk-key added _ property for base text value