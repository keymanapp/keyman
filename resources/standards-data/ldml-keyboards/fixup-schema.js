/*
  Copyright:    © SIL International.
  Description:  Fix up schema from xsd2js
  Create Date:  17 Oct 2022
  Authors:      Steven R. Loomis (SRL)
*/

const { readFileSync, writeFileSync } = require('fs');
const { argv } = require('process');

// Usage:
//    node fixup-schema.js [filename.json]
// If no filename, reads and writes stdin/stdout

// Read stuff
const input = readFileSync(argv[2] || 0, "utf-8");
const data = JSON.parse(input);

// Fix stuff
if (!data['$id'] && data['id']) {
    data['$id'] = data['id'];
    delete data['id'];
}

/**
 * Turn a schema node from an array into a singleton of the specified type
 * @param {Object} o
 */
function arrayToSingle(o) {
    if (!o) return;
    if (o.type === 'array') {
        o["$ref"] = o.items["$ref"];
        delete o.items;
        delete o.type;
    }
}

/**
 * Turn a schema node from a singleton of some type into an array of that type
 * @param {Object} o
 */
function singleToArray(o) {
    if (!o) return;
    if (!o.type) {
        o.items = { "$ref": o["$ref"] };
        o.type = "array";
        delete o["$ref"];
    }
}

if (data.title.endsWith('ldmlKeyboard3.xsd')) {
    if (data?.properties?.keyboard3) {
        data.properties.keyboard3.type = 'object';

        // add the xmlns property as allowed
        if (!data.properties.keyboard3?.properties?.xmlns) {
          data.properties.keyboard3.properties.xmlns = { type: 'string' };
        }
    }



    arrayToSingle(data?.properties?.keyboard3?.properties?.vkeys);
    singleToArray(data?.definitions?.keys?.properties?.key);
    singleToArray(data?.definitions?.keys?.properties?.flicks);
    arrayToSingle(data?.definitions?.displays?.properties?.displayOptions);

    // So we have a little problem where the element 'string' becomes the schema built in type 'string'
    // may be a bug. anyway, fix it
    if (data.definitions["string"]) {
      // if we have the problematic case
      const oldName = "string";
      const newName = "stringVariable";
      // move the definition
      data.definitions[newName] = data.definitions[oldName]
      delete data.definitions[oldName]
      // move the reference
      const item = data.definitions.variables.properties[oldName];
      // Note: not renaming the property, just the type
      if (item?.items?.type !== oldName) {
        // sanity check!
        throw `Couldn’t fixup type, at this location expected to find ${oldName}`;
      }
      // Change from a type to a ref
      delete item.items.type;
      item.items['$ref'] = `#/definitions/${newName}`;
    }
    // Workaround CLDR-18138 by not making 'cldr' the default for the base
    delete data.definitions?.import?.properties?.base?.enum;
}

if (data.title.endsWith('ldmlKeyboardTest3.xsd')) {
  if (data?.properties?.keyboardTest3) {
      data.properties.keyboardTest3.type = 'object';

      // support this proactively
      if (!data.properties.keyboardTest3?.properties?.xmlns) {
        data.properties.keyboardTest3.properties.xmlns = { type: 'string' };
      }
  }
}

// Write stuff
const outstr = JSON.stringify(data, null, "  ");
writeFileSync(argv[2] || 1, outstr, "utf-8");
