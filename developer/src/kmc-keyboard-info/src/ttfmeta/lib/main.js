import property from './meta.js';

const TABLE_COUNT_OFFSET  = 4,
      TABLE_HEAD_OFFSET   = 12,
      TABLE_HEAD_SIZE     = 16,
      TAG_OFFSET          = 0,
      TAG_SIZE            = 4,
      CHECKSUM_OFFSET     = TAG_OFFSET + TAG_SIZE,
      CHECKSUM_SIZE       = 4,
      CONTENTS_PTR_OFFSET = CHECKSUM_OFFSET + CHECKSUM_SIZE,
      CONTENTS_PTR_SIZE   = 4,
      LENGTH_OFFSET       = TABLE_HEAD_SIZE + CONTENTS_PTR_OFFSET;

/**
 * org: count
 * @param {*} data
 */
function offsetCount(data) {
  return u16(data,TABLE_COUNT_OFFSET);
}

/**
 * org: offset
 * @param {*} data
 * @param {string} name
 */
function offsetContent(data, name) {
  return offsetData(data, name).contents;
}

/**
 * @param {*} data
 * @param {string} name
 * @returns {{tag:any,checksum:any,contents:any,length:any}}
 */
function offsetData(data, name) {
  var numTables = offsetCount(data);
  var header={
    tag: '',
    checksum: '',
    contents: '',
    length: ''
  };

  for (var i = 0; i < numTables; ++i) {
    var o = TABLE_HEAD_OFFSET + i * TABLE_HEAD_SIZE;
    var tag = utf8(data.buffer.slice(o, o + CONTENTS_PTR_SIZE));

    if (tag === name) {
      header.tag= tag,
      header.checksum= u32(data,o + CHECKSUM_OFFSET),
      header.contents= u32(data,o + CONTENTS_PTR_OFFSET),
      header.length= u32(data,o + LENGTH_OFFSET)
      return header;
    }
  }
  return header
}

/**
 * org: tableName.js
 * @param {*} data
 */
function name(data) {
  var ntOffset = offsetContent(data, 'name'),
      offsetStorage = u16(data,ntOffset+4),
      numberNameRecords = u16(data,ntOffset+2);

  var storage = offsetStorage + ntOffset;

  /**
   * @type {any}
   */
  var info = {};

  for (var j = 0; j < numberNameRecords; j++) {
    var o = ntOffset + 6 + j*12;

    /**
     * @type {string}
     */
    // var platformId = u16(data,o);

    /**
     * @type {string}
     */
    var nameId = u16(data,o+6);
    /**
     * @type {number}
     */
    var stringLength = u16(data,o+8);
    /**
     * @type {string}
     */
    var stringOffset = u16(data,o+10);

    if (!info.hasOwnProperty(nameId)) {
      info[nameId] = utf8(data.buffer.slice(storage+stringOffset, storage+stringOffset+stringLength));

      // info[nameId] = '';
      // for (var k = 0; k < stringLength; k++) {
      //   var charCode = data.getInt8(storage+stringOffset+k);
      //   if (charCode === 0) continue;
      //   info[nameId] += String.fromCharCode(charCode);
      // }
    }
  }
  return info;
}

const VERSION_OFFSET = 0, WEIGHT_CLASS_OFFSET = 4;

/**
 * org: tableOS2.js
 * @param {*} data
 */
function os2(data) {
  var o = offsetContent(data, 'OS/2');
  return {
    version     : u16(data,o+VERSION_OFFSET),
    weightClass : u16(data,o+WEIGHT_CLASS_OFFSET)
  };
}

const FORMAT_OFFSET               = 0,
      ITALIC_ANGLE_OFFSET         = FORMAT_OFFSET + 4,
      UNDERLINE_POSITION_OFFSET   = ITALIC_ANGLE_OFFSET + 8,
      UNDERLINE_THICKNESS_OFFSET  = UNDERLINE_POSITION_OFFSET + 2,
      IS_FIXED_PITCH_OFFSET       = UNDERLINE_THICKNESS_OFFSET + 2;

export const  result              = {
  meta:{
    /**
     * @type {{name:string,text:string}[]}
     */
    property:[],
    /**
     * @type {{name:string,text:string}[]}
     */
    description:[],
    /**
     * @type {{name:string,text:string}[]}
     */
    license:[],
    /**
     * @type {{name:string,text:string}[]}
     */
    reference:[]
  },
  tables: {
    name: {},
    post: {},
    os2: {
      version:'',weightClass:''
    }
  }
};

/**
 * @param {*} fixed
 * org: fixed16dot16
 */
function f32(fixed) {
  if (fixed & 0x80000000) {
    // negative number is stored in two's complement
    fixed = -(~fixed + 1);
  }
  return fixed / 65536;
}

/**
 * @param {*} data
 * @param {number} pos
 */
function i16(data,pos) {
  // return data.readInt16BE(pos);
  return data.getInt16(pos);
}

/**
 * @param {*} data
 * @param {number} pos
 */
function u16(data,pos) {
  // return data.readUInt16BE(pos);
  return data.getUint16(pos);
}

/**
 * @param {*} data
 * @param {number} pos
 */
function u32(data,pos) {
  // return data.readUInt32BE(pos);
  return data.getUint32(pos);
}

/**
 * @param {*} str
 * @returns TextDecoder
 */
function utf8(str) {
  // return new TextDecoder("utf-8").decode(new Uint16Array(str));
  return new TextDecoder("utf-8").decode(new Uint8Array(str));
}

/**
 * org: tablePost.js
 * @param {*} data
 */
function post(data) {
  var o = offsetContent(data, 'post');
  return {
    format            : f32(u32(data,o+FORMAT_OFFSET)),
    italicAngle       : f32(u32(data,o+ITALIC_ANGLE_OFFSET)),

    underlinePosition : i16(data,o+UNDERLINE_POSITION_OFFSET),
    underlineThickness: i16(data,o+UNDERLINE_THICKNESS_OFFSET),

    isFixedPitch      : u32(data,o+IS_FIXED_PITCH_OFFSET),
    minMemType42      : u32(data,o+7),
    maxMemType42      : u32(data,o+9),
    minMemType1       : u32(data,o+11),
    maxMemType1       : u32(data,o+13)

  };
}

/**
 * @param {any} data
 * param {CallableFunction} callback
 */
function resultTables(data) {
  result.tables.name = name(data);
  result.tables.post = post(data);
  result.tables.os2 = os2(data);
  result.meta = property(result.tables.name);
  return result;
}

/**
 * @param {Buffer} data
 * @return {DataView}
 */
export function view(data) {
  return new DataView(data.buffer, 0, data.byteLength);
}

/**
 * @namespace
 * @param {*} data
 * @param {{(error:string|null,meta?:typeof result):void}} callback
 */
export function ttfInfo(data, callback) {
  try {
    // let dataview = new DataView(data.buffer, 0, data.length);
    resultTables(data);
    callback(null,result);
  } catch (/** @type {any}*/error) {
    callback(error.message || error.toString());
  }
}

/**
 * @param {string | number | Buffer | URL | DataView} pathOrData
 * @returns {Promise<typeof result>}
 */
export function promise(pathOrData){
  return new Promise(function(res,rej){
    ttfInfo(pathOrData, function(e,d){
      if (d) {
        res(d);
      } else {
        rej(e);
      }
    })
  })
}
