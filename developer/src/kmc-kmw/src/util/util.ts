import { STORE, GROUP, KEYBOARD, KMXFile } from "../../../../../common/web/types/src/kmx/kmx.js";

/**
 * Verifies that value is an item in the enumeration.
 */
export function isValidEnumValue<T extends {[key: number]: string | number}>(enu: T, value: string) {
  return (Object.values(enu) as string[]).includes(value);
}

export interface TSentinelRecord {
  IsSentinel: boolean;
  Code?: number;

  Any?: {
    StoreIndex: number;
    Store: STORE;
  };
  Index?: {
    StoreIndex: number;
    Store: STORE;
    Index: number;
  };
  DeadKey?: {
    DeadKey: number;
  };
  Use?: {
    GroupIndex: number;
    Group: GROUP;
  };
  Call?: {
    StoreIndex: number;
    Store: STORE;
  };
  ContextEx?: {
    Index: number;
  };
  IfOpt?: {
    StoreIndex1: number;
    Store1: STORE;
    StoreIndex2: number;
    Store2: STORE;
    IsNot: number;
  };
  IfSystemStore?: {
    dwSystemID: number;
    SystemStore: STORE;
    StoreIndex: number;
    Store: STORE;
    IsNot: number;
  };
  SetOpt?: {
    StoreIndex1: number;
    Store1: STORE;
    StoreIndex2: number;
    Store2: STORE;
  };
  SetSystemStore?: {
    dwSystemID: number;
    SystemStore: STORE;
    StoreIndex: number;
    Store: STORE;
  };
  ResetOpt?: {
    StoreIndex: number;
    Store: STORE;
  };
  SaveOpt?: {
    StoreIndex: number;
    Store: STORE;
  };
  ChrVal?: number;
};


export function ExpandSentinel(fk: KEYBOARD, pwsz: string, x: number): TSentinelRecord {
  let result: TSentinelRecord = {
    IsSentinel: false
  };
  if(pwsz.charCodeAt(x) == KMXFile.UC_SENTINEL) {
    result.IsSentinel = true;
    x++;
    result.Code = pwsz.charCodeAt(x);
    x++;
    switch(result.Code) {
    case KMXFile.CODE_ANY:
    case KMXFile.CODE_NOTANY:      // I3981
      result.Any.StoreIndex = pwsz.charCodeAt(x) - 1;
      result.Any.Store = fk.stores[result.Any.StoreIndex];
      break;
    case KMXFile.CODE_INDEX:
      result.Index.StoreIndex = pwsz.charCodeAt(x) - 1;
      result.Index.Store = fk.stores[result.Index.StoreIndex];
      x++;
      result.Index.Index = pwsz.charCodeAt(x);
      break;
    case KMXFile.CODE_DEADKEY:
      result.DeadKey.DeadKey = pwsz.charCodeAt(x) - 1;
      break;
    case KMXFile.CODE_USE:
      result.Use.GroupIndex = pwsz.charCodeAt(x) - 1;
      result.Use.Group = fk.groups[result.Use.GroupIndex];
      break;
    case KMXFile.CODE_CALL:
      result.Call.StoreIndex = pwsz.charCodeAt(x) - 1;
      result.Call.Store = fk.stores[result.Call.StoreIndex];
      break;
    case KMXFile.CODE_CONTEXTEX:
      result.ContextEx.Index = pwsz.charCodeAt(x);
      break;
    case KMXFile.CODE_SETOPT:    // I3429
      result.SetOpt.StoreIndex1 = pwsz.charCodeAt(x) - 1;
      result.SetOpt.Store1 = fk.stores[result.SetOpt.StoreIndex1];
      x++;
      result.SetOpt.StoreIndex2 = pwsz.charCodeAt(x) - 1;
      result.SetOpt.Store2 = fk.stores[result.SetOpt.StoreIndex2];
      break;
    case KMXFile.CODE_SETSYSTEMSTORE:  // I3437
      result.SetSystemStore.dwSystemID = pwsz.charCodeAt(x) - 1;
      result.SetSystemStore.SystemStore = fk.stores.find(s => s.dwSystemID == result.SetSystemStore.dwSystemID) || null;
      x++;
      result.SetSystemStore.StoreIndex = pwsz.charCodeAt(x) - 1;
      result.SetSystemStore.Store = fk.stores[result.SetSystemStore.StoreIndex];
      break;
    case KMXFile.CODE_RESETOPT:  // I3429
      result.ResetOpt.StoreIndex = pwsz.charCodeAt(x) - 1;
      result.ResetOpt.Store = fk.stores[result.ResetOpt.StoreIndex];
      break;
    case KMXFile.CODE_SAVEOPT:  // I3429
      result.SaveOpt.StoreIndex = pwsz.charCodeAt(x) - 1;
      result.SaveOpt.Store = fk.stores[result.SaveOpt.StoreIndex];
      break;
    case KMXFile.CODE_IFOPT:  // I3429
      result.IfOpt.StoreIndex1 = pwsz.charCodeAt(x) - 1;
      result.IfOpt.Store1 = fk.stores[result.IfOpt.StoreIndex1];
      x++;
      result.IfOpt.IsNot = pwsz.charCodeAt(x) - 1;  // I3429
      x++;
      result.IfOpt.StoreIndex2 = pwsz.charCodeAt(x) - 1;
      result.IfOpt.Store2 = fk.stores[result.IfOpt.StoreIndex2];
      break;
    case KMXFile.CODE_IFSYSTEMSTORE:  // I3430
      result.IfSystemStore.dwSystemID = pwsz.charCodeAt(x) - 1;
      result.IfSystemStore.SystemStore = fk.stores.find(s => s.dwSystemID == result.IfSystemStore.dwSystemID) || null;
      x++;
      result.IfSystemStore.IsNot = pwsz.charCodeAt(x) - 1;  // I3430
      x++;
      result.IfSystemStore.StoreIndex = pwsz.charCodeAt(x) - 1;
      result.IfSystemStore.Store = fk.stores[result.IfSystemStore.StoreIndex];
      break;
    default:
      throw new Error(`Unrecognized system store value ${result.Code}`);
    }
  }
  else {
    result.ChrVal = GetSuppChar(pwsz, x);
  }
  return result;
}

export function GetSuppChar(p: string, x: number): number {
  return p.codePointAt(x);
}

export function incxstr(p: string, x: number): number {
  if(x >= p.length) {
    return p.length;
  }

  let ch = p.charCodeAt(x);
  if(ch != KMXFile.UC_SENTINEL) {
		if(ch >= 0xD800 && ch <= 0xDBFF) {
      x++;
      if(x == p.length) {
        return x;
      }
      ch = p.charCodeAt(x);
      if (ch >= 0xDC00 && ch <= 0xDFFF) {
        x++;
      }
    }
    else {
      x++;
    }
    return x;
  }

  x++;
  if(x == p.length) {
    return x;
  }

  switch(p.charCodeAt(x)) {
    case KMXFile.CODE_ANY:      x += 2; break;
    case KMXFile.CODE_INDEX:    x += 3; break;
    case KMXFile.CODE_USE:      x += 2; break;
    case KMXFile.CODE_DEADKEY:  x += 2; break;
    case KMXFile.CODE_EXTENDED:
      x += 3;
      while(x <= p.length && p.charCodeAt(x) != KMXFile.UC_SENTINEL_EXTENDEDEND) {
        x++;
      }
      break;
    case KMXFile.CODE_CALL:     x += 2; break;
    case KMXFile.CODE_CONTEXTEX: x += 2; break;
    case KMXFile.CODE_NOTANY:   x += 2; break;

    case KMXFile.CODE_CLEARCONTEXT: x += 2; break;
    case KMXFile.CODE_IFOPT:    x += 4; break;
    case KMXFile.CODE_IFSYSTEMSTORE: x += 4; break;
    case KMXFile.CODE_SETOPT:   x += 3; break;
    case KMXFile.CODE_SETSYSTEMSTORE: x += 3; break;
    case KMXFile.CODE_RESETOPT: x += 2; break;
    case KMXFile.CODE_SAVEOPT:  x += 2; break;

    default: x++;
  }

  if(x >= p.length) {
    return p.length;
  }

  return x;
}

export function xstrlen(p: string): number {
  let result = 0, x = 0;
  while(x < p.length) {
    x = incxstr(p, x);
    result++;
  }
  return result;
}

export function xstrlen_printing(p: string): number {
  let result = 0, x = 0;
  while(x < p.length) {
    if(x + 1 < p.length && p.charCodeAt(x) == KMXFile.UC_SENTINEL) {
      switch(p.charCodeAt(x+1)) {
        case KMXFile.CODE_DEADKEY:
        case KMXFile.CODE_NUL:
        case KMXFile.CODE_IFOPT:
        case KMXFile.CODE_IFSYSTEMSTORE:
          result--;
          break;
      }
    }
    x = incxstr(p, x);
    result++;
  }
  return result;
}
