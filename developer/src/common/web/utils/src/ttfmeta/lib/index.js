/**
 * ttfmeta
 * Copyright(c) 2021-2022 Khen Solomon Lethil
 * MIT Licensed
 * v1.0.9
 */

import * as fs from "fs";
import * as ttf from "./main.js";

/**
 * @namespace
 * @param {string | number | Buffer | URL} pathOrData
 * @param {{(error:string|null,meta?:typeof ttf.result):void}} callback
 */
export function ttfInfo(pathOrData, callback) {
  try {
    if (pathOrData instanceof Buffer) {
      ttf.ttfInfo(ttf.view(pathOrData), callback);
    } else {
      fs.readFile(pathOrData, function(error, data) {
        if (error) {
          callback(error.message || error.toString());
        } else {
          ttf.ttfInfo(ttf.view(data), callback);
        }
      });
    }
  } catch (/** @type {any}*/ error) {
    callback(error.message || error.toString());
  }
}

/**
 * @namespace
 * @param {string | number | Buffer | URL} pathOrData
 * @returns {typeof ttf.result}
 */
export function ttfInfoSync(pathOrData) {
  if (pathOrData instanceof Buffer) {
    return ttf.ttfInfoSync(ttf.view(pathOrData));
  } else {
    const data = fs.readFileSync(pathOrData);
    return ttf.ttfInfoSync(ttf.view(data));
  }
}

/**
 * @param {string | number | Buffer | URL} pathOrData
 * @returns {Promise<typeof ttf.result>}
 */
export function promise(pathOrData) {
  return new Promise(function(res, rej) {
    ttfInfo(pathOrData, function(e, d) {
      if (d) {
        res(d);
      } else {
        rej(e);
      }
    });
  });
}

/** @namespace */
export const ttfMeta = { ttfInfo, ttfInfoSync, promise };
export default ttfMeta;
