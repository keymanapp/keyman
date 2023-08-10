import { KMX } from "@keymanapp/common-types";

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
    Store: KMX.STORE;
  };
  Index?: {
    StoreIndex: number;
    Store: KMX.STORE;
    Index: number;
  };
  DeadKey?: {
    DeadKey: number;
  };
  Use?: {
    GroupIndex: number;
    Group: KMX.GROUP;
  };
  Call?: {
    StoreIndex: number;
    Store: KMX.STORE;
  };
  ContextEx?: {
    Index: number;
  };
  IfOpt?: {
    StoreIndex1: number;
    Store1: KMX.STORE;
    StoreIndex2: number;
    Store2: KMX.STORE;
    IsNot: number;
  };
  IfSystemStore?: {
    dwSystemID: number;
    SystemStore: KMX.STORE;
    StoreIndex: number;
    Store: KMX.STORE;
    IsNot: number;
  };
  SetOpt?: {
    StoreIndex1: number;
    Store1: KMX.STORE;
    StoreIndex2: number;
    Store2: KMX.STORE;
  };
  SetSystemStore?: {
    dwSystemID: number;
    SystemStore: KMX.STORE;
    StoreIndex: number;
    Store: KMX.STORE;
  };
  ResetOpt?: {
    StoreIndex: number;
    Store: KMX.STORE;
  };
  SaveOpt?: {
    StoreIndex: number;
    Store: KMX.STORE;
  };
  ChrVal?: number;
};


export function ExpandSentinel(fk: KMX.KEYBOARD, pwsz: string, x: number): TSentinelRecord {
  let result: TSentinelRecord = {
    IsSentinel: false
  };
  if(pwsz.charCodeAt(x) == KMX.KMXFile.UC_SENTINEL) {
    result.IsSentinel = true;
    x++;
    result.Code = pwsz.charCodeAt(x);
    x++;
    switch(result.Code) {
    case KMX.KMXFile.CODE_ANY:
    case KMX.KMXFile.CODE_NOTANY:      // I3981
      let anyIdx = pwsz.charCodeAt(x) - 1;
      result.Any = {
        StoreIndex: anyIdx,
        Store: fk.stores[anyIdx]
      }
      break;
    case KMX.KMXFile.CODE_INDEX:
      let indexIdx = pwsz.charCodeAt(x) - 1;
      x++;
      result.Index = {
        StoreIndex: indexIdx,
        Store: fk.stores[indexIdx],
        Index: pwsz.charCodeAt(x)
      }
      break;
    case KMX.KMXFile.CODE_DEADKEY:
      result.DeadKey = { DeadKey: pwsz.charCodeAt(x) - 1 };
      break;
    case KMX.KMXFile.CODE_USE:
      let useIdx =  pwsz.charCodeAt(x) - 1;
      result.Use = {
        GroupIndex: useIdx,
        Group: fk.groups[useIdx]
      };
      break;
    case KMX.KMXFile.CODE_CALL:
      let callIdx = pwsz.charCodeAt(x) - 1;
      result.Call = {
        StoreIndex: callIdx,
        Store: fk.stores[callIdx]
      }
      break;
    case KMX.KMXFile.CODE_CONTEXTEX:
      result.ContextEx = { Index: pwsz.charCodeAt(x) - 1 };
      break;
    case KMX.KMXFile.CODE_SETOPT:    // I3429
      let setIdx1 = pwsz.charCodeAt(x) - 1;
      x++;
      let setIdx2 = pwsz.charCodeAt(x) - 1;
      result.SetOpt = {
        StoreIndex1: setIdx1,
        Store1: fk.stores[setIdx1],
        StoreIndex2: setIdx2,
        Store2: fk.stores[setIdx2]
      };
      break;
    case KMX.KMXFile.CODE_SETSYSTEMSTORE:  // I3437
      let setsIdx1 = pwsz.charCodeAt(x) - 1;
      x++;
      let setsIdx2 = pwsz.charCodeAt(x) - 1;
      result.SetSystemStore = {
        dwSystemID: setsIdx1,
        SystemStore: fk.stores.find(s => s.dwSystemID == setsIdx1) || null,
        StoreIndex: setsIdx2,
        Store: fk.stores[setsIdx2]
      };
      break;
    case KMX.KMXFile.CODE_RESETOPT:  // I3429
      const resetOptIdx = pwsz.charCodeAt(x) - 1;
      result.ResetOpt = {
          StoreIndex: resetOptIdx,
          Store: fk.stores[resetOptIdx]
      };
      break;
    case KMX.KMXFile.CODE_SAVEOPT:  // I3429
      const saveOptIdx = pwsz.charCodeAt(x) - 1;
      result.SaveOpt = {
        StoreIndex: saveOptIdx,
        Store: fk.stores[saveOptIdx]
      };
      break;
    case KMX.KMXFile.CODE_IFOPT:  // I3429
      let ifIdx1 = pwsz.charCodeAt(x) - 1;
      x++;
      let ifNot = pwsz.charCodeAt(x) - 1;
      x++;
      let ifIdx2 = pwsz.charCodeAt(x) - 1;
      result.IfOpt = {
        StoreIndex1: ifIdx1,
        Store1: fk.stores[ifIdx1],
        IsNot: ifNot,  // I3429
        StoreIndex2: ifIdx2,
        Store2: fk.stores[ifIdx2]
      };
      break;
    case KMX.KMXFile.CODE_IFSYSTEMSTORE:  // I3430
      let ifsSystemID = pwsz.charCodeAt(x) - 1;
      x++;
      let ifsNot = pwsz.charCodeAt(x) - 1;
      x++;
      let ifsIdx2 = pwsz.charCodeAt(x) - 1;
      result.IfSystemStore = {
        dwSystemID: ifsSystemID,
        SystemStore: fk.stores.find(s => s.dwSystemID == ifsSystemID) || null,
        IsNot: ifsNot,
        StoreIndex: ifsIdx2,
        Store: fk.stores[ifsIdx2]
      };
      break;
    case KMX.KMXFile.CODE_NUL:
    case KMX.KMXFile.CODE_CONTEXT:
    case KMX.KMXFile.CODE_RETURN:
    case KMX.KMXFile.CODE_BEEP:
    case KMX.KMXFile.CODE_EXTENDED:
      // No additional detail needed, or outside scope
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
  if(ch != KMX.KMXFile.UC_SENTINEL) {
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
    case KMX.KMXFile.CODE_ANY:      x += 2; break;
    case KMX.KMXFile.CODE_INDEX:    x += 3; break;
    case KMX.KMXFile.CODE_USE:      x += 2; break;
    case KMX.KMXFile.CODE_DEADKEY:  x += 2; break;
    case KMX.KMXFile.CODE_EXTENDED:
      x += 3;
      while(x <= p.length && p.charCodeAt(x) != KMX.KMXFile.UC_SENTINEL_EXTENDEDEND) {
        x++;
      }
      x++;
      break;
    case KMX.KMXFile.CODE_CALL:     x += 2; break;
    case KMX.KMXFile.CODE_CONTEXTEX: x += 2; break;
    case KMX.KMXFile.CODE_NOTANY:   x += 2; break;

    case KMX.KMXFile.CODE_CLEARCONTEXT: x += 2; break;
    case KMX.KMXFile.CODE_IFOPT:    x += 4; break;
    case KMX.KMXFile.CODE_IFSYSTEMSTORE: x += 4; break;
    case KMX.KMXFile.CODE_SETOPT:   x += 3; break;
    case KMX.KMXFile.CODE_SETSYSTEMSTORE: x += 3; break;
    case KMX.KMXFile.CODE_RESETOPT: x += 2; break;
    case KMX.KMXFile.CODE_SAVEOPT:  x += 2; break;

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
    if(x + 1 < p.length && p.charCodeAt(x) == KMX.KMXFile.UC_SENTINEL) {
      switch(p.charCodeAt(x+1)) {
        case KMX.KMXFile.CODE_DEADKEY:
        case KMX.KMXFile.CODE_NUL:
        case KMX.KMXFile.CODE_IFOPT:
        case KMX.KMXFile.CODE_IFSYSTEMSTORE:
          result--;
          break;
      }
    }
    x = incxstr(p, x);
    result++;
  }
  return result;
}
