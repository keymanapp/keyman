/*
  Copyright:    © SIL International.
  Description:  Fix up schema from xsd2js
  Create Date:  17 Oct 2022
  Authors:      Steven R. Loomis (SRL)
*/

const { strict } = require('assert');
const { readFileSync, writeFileSync } = require('fs');
const { argv } = require('process');

// Usage:
//    node fixup-schema.js [filename.json]
// If no filename, reads and writes stdin/stdout

// Read stuff
const input = readFileSync(argv[2] || 0, "utf-8");
// uses C:\Projects\keyman\keyman\developer\src\common\web\utils\src\xml-utils.ts - parse()
const data = JSON.parse(input);

const aaa = data.properties.keyboard.properties.id;
const b = data['$id'];
const c = data['id'];


// Fix stuff
// SAB brauch ich nicht
if (!data['$id'] && data['id']) {
  data['$id'] = data['id'];
  delete data['id'];
}

/*
public parse(data: string): any {
    const parser = this.parser();
    let result = parser.parse(data, true);
    if (PARSER_OPTIONS[this.type].attributeNamePrefix === '$') {
      result = KeymanXMLReader.fixupDollarAttributes(result);
    } else if (PARSER_OPTIONS[this.type].attributeNamePrefix === '@__') {
      result = KeymanXMLReader.fixupEmptyStringToEmptyObject(result);
    } else if (PARSER_OPTIONS[this.type].preserveOrder) {
      result = KeymanXMLReader.fixupPreserveOrder(result);
    }
    delete result['?xml'];
    return result;
  }




  //**
  // * Requires attribute prefix @__ (double underscore)
   //* For attributes, just remove @__ and continue.
  // * For objects, replace any empty string "" with an empty object {} 
  private static fixupEmptyStringToEmptyObject(data: any) : any {
    if (typeof data === 'object') {
      // For arrays of objects, we map "" to {}
      // "" means an empty object
      if (Array.isArray(data)) {
        return data.map(v => {
          if (v === '') {
            return {};
          } else {
            return KeymanXMLReader.fixupEmptyStringToEmptyObject(v);
          }
        });
      }
      // otherwise: remove @__ for attributes, remap objects
      const e: any = [];
      Object.entries(data).forEach(([k, v]) => {
        if (k.startsWith('@__')) {
          e.push([k.substring(3), KeymanXMLReader.fixupEmptyStringToEmptyObject(v)]);
        } else {
          if (v === '') {
            e.push([k, {}]);
          } else {
            e.push([k, KeymanXMLReader.fixupEmptyStringToEmptyObject(v)]);
          }
        }
      });
      return Object.fromEntries(e);
    } else {
      return data;
    }
  }


*/


function trysomething(o) {
  console.log("trysomething ",);
  if (!o) return;
  if (o.type === 'object') {
    o.type = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
  }
}
function trysomething1(o) {
  console.log("trysomething1 ",);
  console.log("data zuerts",data);

  data.type = 'string';     // change tag type
  data.title = "blabla";    // change tag title
  data.required = "myTag";    // change tag required

  data.properties.keyboard.properties.actions.type= "bool"
delete data.properties.keyboard.properties.terminators.properties.when.items
  console.log("data dann ",data);
}



/**
 * Turn a schema node from an array into a singleton of the specified type
 * @param {Object} o
 */
function arrayToSingle(o) {
  console.log("arrayToSingle(o) ",);

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
  console.log("function singleToArray(o) { ",);

  if (!o) return;
  if (!o.type) {
    o.items = { "$ref": o["$ref"] };
    o.type = "array";
    delete o["$ref"];
  }
}


/**
   * Requires attribute prefix @_ (double underscore)
   * For attributes, just remove @_ and continue.
   * For objects, replace any empty string "" with an empty object {} */
function fixupEmptyStringToEmptyObject_keylayout_1(data) {

  //console.log("in fixupEmptyStringToEmptyObject_keylayout_1 ", data);
  data.title = "blabla";
  trysomething1(data);
  trysomething1(data.properties.keyboard.properties);

  if (typeof data === 'object') {
    console.log(" ",);
    trysomething(data);
    trysomething(data.properties.keyboard.properties);

  }
  /*
    if (typeof data === 'object') {
      // For arrays of objects, we map "" to {}
      // "" means an empty object
      if (Array.isArray(data)) {
        return data.map(v => {
          if (v === '') {
            return {};
          } else {
            return data.fixupEmptyStringToEmptyObject_keylayout_1(v);
          }
        });
      }
      // otherwise: remove @_ for attributes, remap objects
      const e = [];
      Object.entries(data).forEach(([k, v]) => {
        if (k.startsWith('@_')) {
          e.push([k.substring(2), data.fixupEmptyStringToEmptyObject_keylayout_1(v)]);
        } else {
          if (v === '') {
            e.push([k, {}]);
          } else {
            e.push([k, data.fixupEmptyStringToEmptyObject_keylayout_1(v)]);
          }
        }
      });
      return Object.fromEntries(e);
    } else {
      return data;
    }*/
}
console.log("data ", data);



if (data.title.endsWith('keylayout.xsd')) {
  if (data?.properties?.keyboard) {
    data.properties.keyboard.type = 'object';

    // SAB brauch ich nicht
    // add the xmlns property as allowed
    /* if (!data.properties.keyboard3?.properties?.xmlns) {
       data.properties.keyboard3.properties.xmlns = { type: 'string' };
     }*/
  }

  // SAB da ruft er für verschiedene tags die funct. auf. 
  arrayToSingle(data?.properties?.keyboard3?.properties?.vkeys);
  singleToArray(data?.definitions?.keys?.properties?.key);
  singleToArray(data?.definitions?.keys?.properties?.flicks);
  arrayToSingle(data?.definitions?.displays?.properties?.displayOptions);
  fixupEmptyStringToEmptyObject_keylayout_1(data);


  //let result = parser.parse(data, true);
  //result = KeymanXMLReader.fixupEmptyStringToEmptyObject_keylayout(data);



  console.log("doneXX ",);


  // So we have a little problem where the element 'string' becomes the schema built in type 'string'
  // may be a bug. anyway, fix it
  /*if (data.definitions["string"]) {
    // if we have the problematic case
    const oldName = "string";
    const newName = "stringVariable";
    // move the definition
    data.definitions[newName] = data.definitions[oldName];
    delete data.definitions[oldName];
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
  delete data.definitions?.import?.properties?.base?.enum;*/
}

if (data.title.endsWith('ldmlKeyboardTest3.xsd')) {
  if (data?.properties?.keyboardTest3) {
    data.properties.keyboardTest3.type = 'object';

    // SAB brauch ich nicht
    // support this proactively
    if (!data.properties.keyboardTest3?.properties?.xmlns) {
      data.properties.keyboardTest3.properties.xmlns = { type: 'string' };
    }
  }
}

// Write stuff
const outstr = JSON.stringify(data, null, "  ");
writeFileSync(argv[2] || 1, outstr, "utf-8");
