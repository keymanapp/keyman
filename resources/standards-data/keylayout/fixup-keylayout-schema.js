/*
  Copyright:    © SIL International.
  Description:  Fix up schema from xsd2js
  Create Date:  2 June 2025
  Authors:      Sabine Schmitt
  Copied and altered from SRL´s : resources\standards-data\ldml-keyboards\fixup-schema.js
*/

const { strict } = require('assert');
const { readFileSync, writeFileSync } = require('fs');
const { argv } = require('process');

// Read stuff
const input = readFileSync(argv[2] || 0, "utf-8");
const data = JSON.parse(input);

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

if (data.title.endsWith('keylayout.xsd')) {
  if (data?.properties?.keyboard) {
    data.properties.keyboard.type = 'object';
  }

  /*
      arrayToSingle(data?.properties?.keyboard3?.properties?.vkeys);
      singleToArray(data?.definitions?.keys?.properties?.key);
      singleToArray(data?.definitions?.keys?.properties?.key);
      singleToArray(data?.definitions?.keys?.properties?.flicks);
      arrayToSingle(data?.definitions?.displays?.properties?.displayOptions);
  */
}

// Write stuff
const outstr = JSON.stringify(data, null, "  ");
writeFileSync(argv[2] || 1, outstr, "utf-8");
