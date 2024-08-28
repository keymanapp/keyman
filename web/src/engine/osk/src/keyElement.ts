import { ActiveSubKey } from 'keyman/engine/keyboard';
import OSKKey from "./keyboard-layout/oskKey.js";

export class KeyData {
  ['key']: OSKKey;
  ['keyId']: string;
  ['subKeys']?: ActiveSubKey[];

  constructor(keyData: OSKKey, keyId: string) {
    this['key'] = keyData;
    this['keyId'] = keyId;
  }
}

export type KeyElement = HTMLDivElement & KeyData;

// Many thanks to https://www.typescriptlang.org/docs/handbook/advanced-types.html for this.
export function link(elem: HTMLDivElement, data: KeyData): KeyElement {
  let e = <KeyElement> elem;

  // Merges all properties and methods of KeyData onto the underlying HTMLDivElement, creating a merged class.
  for(let id in data) {
    if(!e.hasOwnProperty(id)) {
      (<any>e)[id] = (<any>data)[id];
    }
  }

  return e;
}

export function isKey(elem: Node): boolean {
  return elem && ('key' in elem) && ((<any> elem['key']) instanceof OSKKey);
}

export function getKeyFrom(elem: Node): KeyElement {
  if(isKey(elem)) {
    return <KeyElement> elem;
  } else {
    return null;
  }
}