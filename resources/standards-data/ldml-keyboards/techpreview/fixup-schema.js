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

if (data.title.endsWith('ldmlKeyboard.xsd')) {
    if (data?.properties?.keyboard) {
        data.properties.keyboard.type = 'object';
    }

    if (data?.properties?.keyboard?.properties?.vkeyMaps?.type === 'array') {
        data.properties.keyboard.properties.vkeyMaps["$ref"] = data.properties.keyboard.properties.vkeyMaps.items["$ref"];
        delete data.properties.keyboard.properties.vkeyMaps.items;
        delete data.properties.keyboard.properties.vkeyMaps.type;
    }

    if (!data?.definitions?.keys?.properties?.key?.type) {
        data.definitions.keys.properties.key.items = { "$ref":  data.definitions.keys.properties.key["$ref"] };
        data.definitions.keys.properties.key.type = "array";
        delete data.definitions.keys.properties.key["$ref"];
    }
}

// Write stuff
const outstr = JSON.stringify(data, null, "  ");
writeFileSync(argv[2] || 1, outstr, "utf-8");
