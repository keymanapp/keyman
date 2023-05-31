import { expect } from 'chai';
import { Constants } from '@keymanapp/common-types';

import LdmlVkeyNames = Constants.LdmlVkeyNames;

describe('virtual key constants', function () {

  it('should map the CLDR VKey Enum values to the right constants', function () {

    // These are copied-and-pasted from the table in TR35
    // We want to check that our constants match the ones in that table!
    const CLDRVKeyEnumValues: Record<string, number> = {
      'SPACE': 0x20,
      '0': 0x30,
      '1': 0x31,
      '2': 0x32,
      '3': 0x33,
      '4': 0x34,
      '5': 0x35,
      '6': 0x36,
      '7': 0x37,
      '8': 0x38,
      '9': 0x39,
      'A': 0x41,
      'B': 0x42,
      'C': 0x43,
      'D': 0x44,
      'E': 0x45,
      'F': 0x46,
      'G': 0x47,
      'H': 0x48,
      'I': 0x49,
      'J': 0x4A,
      'K': 0x4B,
      'L': 0x4C,
      'M': 0x4D,
      'N': 0x4E,
      'O': 0x4F,
      'P': 0x50,
      'Q': 0x51,
      'R': 0x52,
      'S': 0x53,
      'T': 0x54,
      'U': 0x55,
      'V': 0x56,
      'W': 0x57,
      'X': 0x58,
      'Y': 0x59,
      'Z': 0x5A,
      'SEMICOLON': 0xBA,
      'EQUAL': 0xBB,
      'COMMA': 0xBC,
      'HYPHEN': 0xBD,
      'PERIOD': 0xBE,
      'SLASH': 0xBF,
      'GRAVE': 0xC0,
      'LBRACKET': 0xDB,
      'BACKSLASH': 0xDC,
      'RBRACKET': 0xDD,
      'QUOTE': 0xDE,
      'LESS-THAN': 0xE2,
      'ABNT2': 0xC1
    };

    const keys = Object.keys(CLDRVKeyEnumValues);
    for (let key of keys) {
      expect(CLDRVKeyEnumValues[key]).to.be.equal(LdmlVkeyNames[key]);
    }
  });
});
