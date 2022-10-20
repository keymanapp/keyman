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

if (data.title.endsWith('ldmlKeyboard.xsd')) {
    if (data?.properties?.keyboard) {
        data.properties.keyboard.type = 'object';
    }

    arrayToSingle(data?.properties?.keyboard?.properties?.vkeys);
    singleToArray(data?.definitions?.keys?.properties?.key);
    singleToArray(data?.definitions?.keys?.properties?.flicks);
}

// Write stuff
const outstr = JSON.stringify(data, null, "  ");
writeFileSync(argv[2] || 1, outstr, "utf-8");
