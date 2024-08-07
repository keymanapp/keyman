"use strict";
export function stripBOM(str) {
  if (str[0] === '\uFEFF') {
    return str.substring(1);
  } else {
    return str;
  }
};
