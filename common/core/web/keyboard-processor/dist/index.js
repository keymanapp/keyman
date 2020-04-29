/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/
/**
 * Constructs a string from one or more Unicode character codepoint values
 * passed as integer parameters.
 *
 * @param  {number} cp0,...   1 or more Unicode codepoints, e.g. 0x0065, 0x10000
 * @return {string|null}      The new String object.
 */
String.kmwFromCharCode = function (cp0) {
    var chars = [], i;
    for (i = 0; i < arguments.length; i++) {
        var c = Number(arguments[i]);
        if (!isFinite(c) || c < 0 || c > 0x10FFFF || Math.floor(c) !== c) {
            throw new RangeError("Invalid code point " + c);
        }
        if (c < 0x10000) {
            chars.push(c);
        }
        else {
            c -= 0x10000;
            chars.push((c >> 10) + 0xD800);
            chars.push((c % 0x400) + 0xDC00);
        }
    }
    return String.fromCharCode.apply(undefined, chars);
};
/**
 * Returns a number indicating the Unicode value of the character at the given
 * code point index, with support for supplementary plane characters.
 *
 * @param  {number}  codePointIndex  The code point index into the string (not
                                     the code unit index) to return
 * @return {number}                  The Unicode character value
 */
String.prototype.kmwCharCodeAt = function (codePointIndex) {
    var str = String(this);
    var codeUnitIndex = 0;
    if (codePointIndex < 0 || codePointIndex >= str.length) {
        return NaN;
    }
    for (var i = 0; i < codePointIndex; i++) {
        codeUnitIndex = str.kmwNextChar(codeUnitIndex);
        if (codeUnitIndex === null)
            return NaN;
    }
    var first = str.charCodeAt(codeUnitIndex);
    if (first >= 0xD800 && first <= 0xDBFF && str.length > codeUnitIndex + 1) {
        var second = str.charCodeAt(codeUnitIndex + 1);
        if (second >= 0xDC00 && second <= 0xDFFF) {
            return ((first - 0xD800) << 10) + (second - 0xDC00) + 0x10000;
        }
    }
    return first;
};
/**
 * Returns the code point index within the calling String object of the first occurrence
 * of the specified value, or -1 if not found.
 *
 * @param  {string}  searchValue    The value to search for
 * @param  {number}  [fromIndex]    Optional code point index to start searching from
 * @return {number}                 The code point index of the specified search value
 */
String.prototype.kmwIndexOf = function (searchValue, fromIndex) {
    var str = String(this);
    var codeUnitIndex = str.indexOf(searchValue, fromIndex);
    if (codeUnitIndex < 0) {
        return codeUnitIndex;
    }
    var codePointIndex = 0;
    for (var i = 0; i !== null && i < codeUnitIndex; i = str.kmwNextChar(i))
        codePointIndex++;
    return codePointIndex;
};
/**
 * Returns the code point index within the calling String object of the last occurrence
 * of the specified value, or -1 if not found.
 *
 * @param  {string}  searchValue    The value to search for
 * @param  {number}  fromIndex      Optional code point index to start searching from
 * @return {number}                 The code point index of the specified search value
 */
String.prototype.kmwLastIndexOf = function (searchValue, fromIndex) {
    var str = String(this);
    var codeUnitIndex = str.lastIndexOf(searchValue, fromIndex);
    if (codeUnitIndex < 0) {
        return codeUnitIndex;
    }
    var codePointIndex = 0;
    for (var i = 0; i !== null && i < codeUnitIndex; i = str.kmwNextChar(i))
        codePointIndex++;
    return codePointIndex;
};
/**
 * Returns the length of the string in code points, as opposed to code units.
 *
 * @return {number}                 The length of the string in code points
 */
String.prototype.kmwLength = function () {
    var str = String(this);
    if (str.length == 0)
        return 0;
    for (var i = 0, codeUnitIndex = 0; codeUnitIndex !== null; i++)
        codeUnitIndex = str.kmwNextChar(codeUnitIndex);
    return i;
};
/**
 * Extracts a section of a string and returns a new string.
 *
 * @param  {number}  beginSlice    The start code point index in the string to
 *                                 extract from
 * @param  {number}  endSlice      Optional end code point index in the string
 *                                 to extract to
 * @return {string}                The substring as selected by beginSlice and
 *                                 endSlice
 */
String.prototype.kmwSlice = function (beginSlice, endSlice) {
    var str = String(this);
    var beginSliceCodeUnit = str.kmwCodePointToCodeUnit(beginSlice);
    var endSliceCodeUnit = str.kmwCodePointToCodeUnit(endSlice);
    if (beginSliceCodeUnit === null || endSliceCodeUnit === null)
        return '';
    else
        return str.slice(beginSliceCodeUnit, endSliceCodeUnit);
};
/**
 * Returns the characters in a string beginning at the specified location through
 * the specified number of characters.
 *
 * @param  {number}  start         The start code point index in the string to
 *                                 extract from
 * @param  {number=}  length        Optional length to extract
 * @return {string}                The substring as selected by start and length
 */
String.prototype.kmwSubstr = function (start, length) {
    var str = String(this);
    if (start < 0) {
        start = str.kmwLength() + start;
    }
    if (start < 0)
        start = 0;
    var startCodeUnit = str.kmwCodePointToCodeUnit(start);
    var endCodeUnit = startCodeUnit;
    if (startCodeUnit === null)
        return '';
    if (arguments.length < 2) {
        endCodeUnit = str.length;
    }
    else {
        for (var i = 0; i < length; i++)
            endCodeUnit = str.kmwNextChar(endCodeUnit);
    }
    if (endCodeUnit === null)
        return str.substring(startCodeUnit);
    else
        return str.substring(startCodeUnit, endCodeUnit);
};
/**
 * Returns the characters in a string between two indexes into the string.
 *
 * @param  {number}  indexA        The start code point index in the string to
 *                                 extract from
 * @param  {number}  indexB        The end code point index in the string to
 *                                 extract to
 * @return {string}                The substring as selected by indexA and indexB
 */
String.prototype.kmwSubstring = function (indexA, indexB) {
    var str = String(this), indexACodeUnit, indexBCodeUnit;
    if (typeof (indexB) == 'undefined') {
        indexACodeUnit = str.kmwCodePointToCodeUnit(indexA);
        indexBCodeUnit = str.length;
    }
    else {
        if (indexA > indexB) {
            var c = indexA;
            indexA = indexB;
            indexB = c;
        }
        indexACodeUnit = str.kmwCodePointToCodeUnit(indexA);
        indexBCodeUnit = str.kmwCodePointToCodeUnit(indexB);
    }
    if (isNaN(indexACodeUnit) || indexACodeUnit === null)
        indexACodeUnit = 0;
    if (isNaN(indexBCodeUnit) || indexBCodeUnit === null)
        indexBCodeUnit = str.length;
    return str.substring(indexACodeUnit, indexBCodeUnit);
};
/*
  Helper functions
*/
/**
 * Returns the code unit index for the next code point in the string, accounting for
 * supplementary pairs
 *
 * @param  {number|null}  codeUnitIndex  The code unit position to increment
 * @return {number|null}                 The index of the next code point in the string,
 *                                       in code units
 */
String.prototype.kmwNextChar = function (codeUnitIndex) {
    var str = String(this);
    if (codeUnitIndex === null || codeUnitIndex < 0 || codeUnitIndex >= str.length - 1) {
        return null;
    }
    var first = str.charCodeAt(codeUnitIndex);
    if (first >= 0xD800 && first <= 0xDBFF && str.length > codeUnitIndex + 1) {
        var second = str.charCodeAt(codeUnitIndex + 1);
        if (second >= 0xDC00 && second <= 0xDFFF) {
            if (codeUnitIndex == str.length - 2) {
                return null;
            }
            return codeUnitIndex + 2;
        }
    }
    return codeUnitIndex + 1;
};
/**
 * Returns the code unit index for the previous code point in the string, accounting
 * for supplementary pairs
 *
 * @param  {number|null}  codeUnitIndex  The code unit position to decrement
 * @return {number|null}                 The index of the previous code point in the
 *                                       string, in code units
*/
String.prototype.kmwPrevChar = function (codeUnitIndex) {
    var str = String(this);
    if (codeUnitIndex == null || codeUnitIndex <= 0 || codeUnitIndex > str.length) {
        return null;
    }
    var second = str.charCodeAt(codeUnitIndex - 1);
    if (second >= 0xDC00 && second <= 0xDFFF && codeUnitIndex > 1) {
        var first = str.charCodeAt(codeUnitIndex - 2);
        if (first >= 0xD800 && first <= 0xDBFF) {
            return codeUnitIndex - 2;
        }
    }
    return codeUnitIndex - 1;
};
/**
 * Returns the corresponding code unit index to the code point index passed
 *
 * @param  {number|null} codePointIndex  A code point index in the string
 * @return {number|null}                 The corresponding code unit index
 */
String.prototype.kmwCodePointToCodeUnit = function (codePointIndex) {
    if (codePointIndex === null)
        return null;
    var str = String(this);
    var codeUnitIndex = 0;
    if (codePointIndex < 0) {
        codeUnitIndex = str.length;
        for (var i = 0; i > codePointIndex; i--)
            codeUnitIndex = str.kmwPrevChar(codeUnitIndex);
        return codeUnitIndex;
    }
    if (codePointIndex == str.kmwLength())
        return str.length;
    for (var i = 0; i < codePointIndex; i++)
        codeUnitIndex = str.kmwNextChar(codeUnitIndex);
    return codeUnitIndex;
};
/**
 * Returns the corresponding code point index to the code unit index passed
 *
 * @param  {number|null}  codeUnitIndex  A code unit index in the string
 * @return {number|null}                 The corresponding code point index
 */
String.prototype.kmwCodeUnitToCodePoint = function (codeUnitIndex) {
    var str = String(this);
    if (codeUnitIndex === null)
        return null;
    else if (codeUnitIndex == 0)
        return 0;
    else if (codeUnitIndex < 0)
        return str.substr(codeUnitIndex).kmwLength();
    else
        return str.substr(0, codeUnitIndex).kmwLength();
};
/**
 * Returns the character at a the code point index passed
 *
 * @param  {number}  codePointIndex  A code point index in the string
 * @return {string}                  The corresponding character
 */
String.prototype.kmwCharAt = function (codePointIndex) {
    var str = String(this);
    if (codePointIndex >= 0)
        return str.kmwSubstr(codePointIndex, 1);
    else
        return '';
};
/**
 * String prototype library extensions for basic plane characters,
 * to simplify enabling or disabling supplementary plane functionality (I3319)
 */
/**
 * Returns the code unit index for the next code point in the string
 *
 * @param  {number}  codeUnitIndex    A code point index in the string
 * @return {number|null}                   The corresponding character
 */
String.prototype.kmwBMPNextChar = function (codeUnitIndex) {
    var str = String(this);
    if (codeUnitIndex < 0 || codeUnitIndex >= str.length - 1) {
        return null;
    }
    return codeUnitIndex + 1;
};
/**
 * Returns the code unit index for the previous code point in the string
 *
 * @param  {number}  codeUnitIndex    A code unit index in the string
 * @return {number|null}                   The corresponding character
 */
String.prototype.kmwBMPPrevChar = function (codeUnitIndex) {
    var str = String(this);
    if (codeUnitIndex <= 0 || codeUnitIndex > str.length) {
        return null;
    }
    return codeUnitIndex - 1;
};
/**
 * Returns the code unit index for a code point index
 *
 * @param  {number}  codePointIndex   A code point index in the string
 * @return {number}                   The corresponding character
 */
String.prototype.kmwBMPCodePointToCodeUnit = function (codePointIndex) {
    return codePointIndex;
};
/**
 * Returns the code point index for a code unit index
 *
 * @param  {number}  codeUnitIndex    A code point index in the string
 * @return {number}                   The corresponding character
 */
String.prototype.kmwBMPCodeUnitToCodePoint = function (codeUnitIndex) {
    return codeUnitIndex;
};
/**
 * Returns the length of a BMP string
 *
 * @return {number}                   The length in code points
 */
String.prototype.kmwBMPLength = function () {
    var str = String(this);
    return str.length;
};
/**
 * Returns a substring
 *
 * @param  {number}  n
 * @param  {number=}  ln
 * @return {string}
 */
String.prototype.kmwBMPSubstr = function (n, ln) {
    var str = String(this);
    if (n > -1)
        return str.substr(n, ln);
    else
        return str.substr(str.length + n, -n);
};
/**
 * Enable or disable supplementary plane string handling
 *
 * @param  {boolean}  bEnable
 */
String.kmwEnableSupplementaryPlane = function (bEnable) {
    var p = String.prototype;
    String._kmwFromCharCode = bEnable ? String.kmwFromCharCode : String.fromCharCode;
    p._kmwCharAt = bEnable ? p.kmwCharAt : p.charAt;
    p._kmwCharCodeAt = bEnable ? p.kmwCharCodeAt : p.charCodeAt;
    p._kmwIndexOf = bEnable ? p.kmwIndexOf : p.indexOf;
    p._kmwLastIndexOf = bEnable ? p.kmwLastIndexOf : p.lastIndexOf;
    p._kmwSlice = bEnable ? p.kmwSlice : p.slice;
    p._kmwSubstring = bEnable ? p.kmwSubstring : p.substring;
    p._kmwSubstr = bEnable ? p.kmwSubstr : p.kmwBMPSubstr;
    p._kmwLength = bEnable ? p.kmwLength : p.kmwBMPLength;
    p._kmwNextChar = bEnable ? p.kmwNextChar : p.kmwBMPNextChar;
    p._kmwPrevChar = bEnable ? p.kmwPrevChar : p.kmwBMPPrevChar;
    p._kmwCodePointToCodeUnit = bEnable ? p.kmwCodePointToCodeUnit : p.kmwBMPCodePointToCodeUnit;
    p._kmwCodeUnitToCodePoint = bEnable ? p.kmwCodeUnitToCodePoint : p.kmwBMPCodeUnitToCodePoint;
};
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            text.Codes = {
                // Define Keyman Developer modifier bit-flags (exposed for use by other modules)
                modifierCodes: {
                    "LCTRL": 0x0001,
                    "RCTRL": 0x0002,
                    "LALT": 0x0004,
                    "RALT": 0x0008,
                    "SHIFT": 0x0010,
                    "CTRL": 0x0020,
                    "ALT": 0x0040,
                    "CAPS": 0x0100,
                    "NO_CAPS": 0x0200,
                    "NUM_LOCK": 0x0400,
                    "NO_NUM_LOCK": 0x0800,
                    "SCROLL_LOCK": 0x1000,
                    "NO_SCROLL_LOCK": 0x2000,
                    "VIRTUAL_KEY": 0x4000
                },
                modifierBitmasks: {
                    "ALL": 0x007F,
                    "ALT_GR_SIM": (0x0001 | 0x0004),
                    "CHIRAL": 0x001F,
                    "IS_CHIRAL": 0x000F,
                    "NON_CHIRAL": 0x0070 // The default bitmask, for non-chiral keyboards
                },
                stateBitmasks: {
                    "ALL": 0x3F00,
                    "CAPS": 0x0300,
                    "NUM_LOCK": 0x0C00,
                    "SCROLL_LOCK": 0x3000
                },
                // Define standard keycode numbers (exposed for use by other modules)
                keyCodes: {
                    "K_BKSP": 8, "K_TAB": 9, "K_ENTER": 13,
                    "K_SHIFT": 16, "K_CONTROL": 17, "K_ALT": 18, "K_PAUSE": 19, "K_CAPS": 20,
                    "K_ESC": 27, "K_SPACE": 32, "K_PGUP": 33,
                    "K_PGDN": 34, "K_END": 35, "K_HOME": 36, "K_LEFT": 37, "K_UP": 38,
                    "K_RIGHT": 39, "K_DOWN": 40, "K_SEL": 41, "K_PRINT": 42, "K_EXEC": 43,
                    "K_INS": 45, "K_DEL": 46, "K_HELP": 47, "K_0": 48,
                    "K_1": 49, "K_2": 50, "K_3": 51, "K_4": 52, "K_5": 53, "K_6": 54, "K_7": 55,
                    "K_8": 56, "K_9": 57, "K_A": 65, "K_B": 66, "K_C": 67, "K_D": 68, "K_E": 69,
                    "K_F": 70, "K_G": 71, "K_H": 72, "K_I": 73, "K_J": 74, "K_K": 75, "K_L": 76,
                    "K_M": 77, "K_N": 78, "K_O": 79, "K_P": 80, "K_Q": 81, "K_R": 82, "K_S": 83,
                    "K_T": 84, "K_U": 85, "K_V": 86, "K_W": 87, "K_X": 88, "K_Y": 89, "K_Z": 90,
                    "K_NP0": 96, "K_NP1": 97, "K_NP2": 98,
                    "K_NP3": 99, "K_NP4": 100, "K_NP5": 101, "K_NP6": 102,
                    "K_NP7": 103, "K_NP8": 104, "K_NP9": 105, "K_NPSTAR": 106,
                    "K_NPPLUS": 107, "K_SEPARATOR": 108, "K_NPMINUS": 109, "K_NPDOT": 110,
                    "K_NPSLASH": 111, "K_F1": 112, "K_F2": 113, "K_F3": 114, "K_F4": 115,
                    "K_F5": 116, "K_F6": 117, "K_F7": 118, "K_F8": 119, "K_F9": 120,
                    "K_F10": 121, "K_F11": 122, "K_F12": 123, "K_NUMLOCK": 144, "K_SCROLL": 145,
                    "K_LSHIFT": 160, "K_RSHIFT": 161, "K_LCONTROL": 162, "K_RCONTROL": 163,
                    "K_LALT": 164, "K_RALT": 165,
                    "K_COLON": 186, "K_EQUAL": 187, "K_COMMA": 188, "K_HYPHEN": 189,
                    "K_PERIOD": 190, "K_SLASH": 191, "K_BKQUOTE": 192,
                    "K_LBRKT": 219, "K_BKSLASH": 220, "K_RBRKT": 221,
                    "K_QUOTE": 222, "K_oE2": 226, "K_OE2": 226,
                    "K_LOPT": 50001, "K_ROPT": 50002,
                    "K_NUMERALS": 50003, "K_SYMBOLS": 50004, "K_CURRENCIES": 50005,
                    "K_UPPER": 50006, "K_LOWER": 50007, "K_ALPHA": 50008,
                    "K_SHIFTED": 50009, "K_ALTGR": 50010,
                    "K_TABBACK": 50011, "K_TABFWD": 50012
                },
                codesUS: [
                    ['0123456789', ';=,-./`', '[\\]\''],
                    [')!@#$%^&*(', ':+<_>?~', '{|}"']
                ]
            };
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            var Browser;
            (function (Browser) {
                Browser["Chrome"] = "chrome";
                Browser["Edge"] = "edge";
                Browser["Firefox"] = "firefox";
                Browser["Native"] = "native";
                Browser["Opera"] = "opera";
                Browser["Safari"] = "safari";
                Browser["Other"] = "other";
            })(Browser = text.Browser || (text.Browser = {}));
            var OperatingSystem;
            (function (OperatingSystem) {
                OperatingSystem["Windows"] = "windows";
                OperatingSystem["macOS"] = "macosx";
                OperatingSystem["Linux"] = "linux";
                OperatingSystem["Android"] = "android";
                OperatingSystem["iOS"] = "ios";
                OperatingSystem["Other"] = "other";
            })(OperatingSystem = text.OperatingSystem || (text.OperatingSystem = {}));
            var FormFactor;
            (function (FormFactor) {
                FormFactor["Desktop"] = "desktop";
                FormFactor["Phone"] = "phone";
                FormFactor["Tablet"] = "tablet";
            })(FormFactor = text.FormFactor || (text.FormFactor = {}));
            /**
             * This class provides an abstract version of com.keyman.Device that is core-friendly,
             * containing only the information needed by web-core for text processing use, devoid
             * of any direct references to the DOM.
             */
            var EngineDeviceSpec = /** @class */ (function () {
                function EngineDeviceSpec(browser, formFactor, OS, touchable) {
                    switch (browser.toLowerCase()) {
                        case Browser.Chrome:
                        case Browser.Edge:
                        case Browser.Firefox:
                        case Browser.Native:
                        case Browser.Opera:
                        case Browser.Safari:
                            this.browser = browser.toLowerCase();
                            break;
                        default:
                            this.browser = Browser.Other;
                    }
                    switch (formFactor.toLowerCase()) {
                        case FormFactor.Desktop:
                        case FormFactor.Phone:
                        case FormFactor.Tablet:
                            this.formFactor = formFactor.toLowerCase();
                            break;
                        default:
                            throw ("Invalid form factor specified for device: " + formFactor);
                    }
                    switch (OS.toLowerCase()) {
                        case OperatingSystem.Windows.toLowerCase():
                        case OperatingSystem.macOS.toLowerCase():
                        case OperatingSystem.Linux.toLowerCase():
                        case OperatingSystem.Android.toLowerCase():
                        case OperatingSystem.iOS.toLowerCase():
                            this.OS = OS.toLowerCase();
                            break;
                        default:
                            this.OS = OperatingSystem.Other;
                    }
                    this.touchable = touchable;
                }
                return EngineDeviceSpec;
            }());
            text.EngineDeviceSpec = EngineDeviceSpec;
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            // Defines the base Deadkey-tracking object.
            var Deadkey = /** @class */ (function () {
                function Deadkey(pos, id) {
                    this.p = pos;
                    this.d = id;
                    this.o = Deadkey.ordinalSeed++;
                }
                Deadkey.prototype.match = function (p, d) {
                    var result = (this.p == p && this.d == d);
                    return result;
                };
                Deadkey.prototype.set = function () {
                    this.matched = 1;
                };
                Deadkey.prototype.reset = function () {
                    this.matched = 0;
                };
                Deadkey.prototype.before = function (other) {
                    return this.o < other.o;
                };
                Deadkey.prototype.clone = function () {
                    var dk = new Deadkey(this.p, this.d);
                    dk.o = this.o;
                    return dk;
                };
                Deadkey.ordinalSeed = 0;
                /**
                 * Sorts the deadkeys in reverse order.
                 */
                Deadkey.sortFunc = function (a, b) {
                    // We want descending order, so we want 'later' deadkeys first.
                    if (a.p != b.p) {
                        return b.p - a.p;
                    }
                    else {
                        return b.o - a.o;
                    }
                };
                return Deadkey;
            }());
            text.Deadkey = Deadkey;
            // Object-orients deadkey management.
            var DeadkeyTracker = /** @class */ (function () {
                function DeadkeyTracker() {
                    this.dks = [];
                }
                DeadkeyTracker.prototype.toSortedArray = function () {
                    this.dks = this.dks.sort(Deadkey.sortFunc);
                    return [].concat(this.dks);
                };
                DeadkeyTracker.prototype.clone = function () {
                    var dkt = new DeadkeyTracker();
                    var dks = this.toSortedArray();
                    // Make sure to clone the deadkeys themselves - the Deadkey object is mutable.
                    dkt.dks = [];
                    dks.forEach(function (value) {
                        dkt.dks.push(value.clone());
                    });
                    return dkt;
                };
                /**
                 * Function     isMatch
                 * Scope        Public
                 * @param       {number}      caretPos  current cursor position
                 * @param       {number}      n         expected offset of deadkey from cursor
                 * @param       {number}      d         deadkey
                 * @return      {boolean}               True if deadkey found selected context matches val
                 * Description  Match deadkey at current cursor position
                 */
                DeadkeyTracker.prototype.isMatch = function (caretPos, n, d) {
                    if (this.dks.length == 0) {
                        return false; // I3318
                    }
                    var sp = caretPos;
                    n = sp - n;
                    for (var i = 0; i < this.dks.length; i++) {
                        // Don't re-match an already-matched deadkey.  It's possible to have two identical 
                        // entries, and they should be kept separately.
                        if (this.dks[i].match(n, d) && !this.dks[i].matched) {
                            this.dks[i].set();
                            // Assumption:  since we match the first possible entry in the array, we
                            // match the entry with the lower ordinal - the 'first' deadkey in the position.
                            return true; // I3318
                        }
                    }
                    this.resetMatched(); // I3318
                    return false;
                };
                DeadkeyTracker.prototype.add = function (dk) {
                    this.dks = this.dks.concat(dk);
                };
                DeadkeyTracker.prototype.remove = function (dk) {
                    var index = this.dks.indexOf(dk);
                    this.dks.splice(index, 1);
                };
                DeadkeyTracker.prototype.clear = function () {
                    this.dks = [];
                };
                DeadkeyTracker.prototype.resetMatched = function () {
                    for (var _i = 0, _a = this.dks; _i < _a.length; _i++) {
                        var dk = _a[_i];
                        dk.reset();
                    }
                };
                DeadkeyTracker.prototype.deleteMatched = function () {
                    for (var Li = 0; Li < this.dks.length; Li++) {
                        if (this.dks[Li].matched) {
                            this.dks.splice(Li--, 1); // Don't forget to decrement!
                        }
                    }
                };
                /**
                 * Function     adjustPositions (formerly _DeadkeyAdjustPos)
                 * Scope        Private
                 * @param       {number}      Lstart      start position in context
                 * @param       {number}      Ldelta      characters to adjust by
                 * Description  Adjust saved positions of deadkeys in context
                 */
                DeadkeyTracker.prototype.adjustPositions = function (Lstart, Ldelta) {
                    if (Ldelta == 0) {
                        return;
                    }
                    for (var _i = 0, _a = this.dks; _i < _a.length; _i++) {
                        var dk = _a[_i];
                        if (dk.p > Lstart) {
                            dk.p += Ldelta;
                        }
                    }
                };
                DeadkeyTracker.prototype.count = function () {
                    return this.dks.length;
                };
                return DeadkeyTracker;
            }());
            text.DeadkeyTracker = DeadkeyTracker;
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
// Defines KMW's string extension functions.
///<reference path="../text/kmwstring.ts" />
// Defines deadkey management in a manner attachable to each element interface.
///<reference path="../text/deadkeys.ts" />
// Defines the KeyEvent type.
///<reference path="keyEvent.ts" />
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text_1) {
            var TextTransform = /** @class */ (function () {
                function TextTransform(insert, deleteLeft, deleteRight) {
                    this.insert = insert;
                    this.deleteLeft = deleteLeft;
                    this.deleteRight = deleteRight || 0;
                }
                TextTransform.nil = new TextTransform('', 0, 0);
                return TextTransform;
            }());
            text_1.TextTransform = TextTransform;
            var Transcription = /** @class */ (function () {
                function Transcription(keystroke, transform, preInput, alternates /*, removedDks: Deadkey[], insertedDks: Deadkey[]*/) {
                    var token = this.token = Transcription.tokenSeed++;
                    this.keystroke = keystroke;
                    this.transform = transform;
                    this.alternates = alternates;
                    this.preInput = preInput;
                    this.transform.id = this.token;
                    // Assign the ID to each alternate, as well.
                    if (alternates) {
                        alternates.forEach(function (alt) {
                            alt.sample.id = token;
                        });
                    }
                }
                Transcription.tokenSeed = 0;
                return Transcription;
            }());
            text_1.Transcription = Transcription;
            var OutputTarget = /** @class */ (function () {
                function OutputTarget() {
                    this._dks = new text.DeadkeyTracker();
                }
                Object.defineProperty(OutputTarget.prototype, "isSynthetic", {
                    /**
                     * Signifies that this OutputTarget has no default key processing behaviors.  This should be false
                     * for OutputTargets backed by web elements like HTMLInputElement or HTMLTextAreaElement.
                     */
                    get: function () {
                        return true;
                    },
                    enumerable: true,
                    configurable: true
                });
                OutputTarget.prototype.resetContext = function () {
                    this.deadkeys().clear();
                };
                OutputTarget.prototype.deadkeys = function () {
                    return this._dks;
                };
                OutputTarget.prototype.hasDeadkeyMatch = function (n, d) {
                    return this.deadkeys().isMatch(this.getDeadkeyCaret(), n, d);
                };
                OutputTarget.prototype.insertDeadkeyBeforeCaret = function (d) {
                    var dk = new text_1.Deadkey(this.getDeadkeyCaret(), d);
                    this.deadkeys().add(dk);
                };
                /**
                 * Should be called by each output target immediately before text mutation operations occur.
                 *
                 * Maintains solutions to old issues:  I3318,I3319
                 * @param {number} delta  Use negative values if characters were deleted, positive if characters were added.
                 */
                OutputTarget.prototype.adjustDeadkeys = function (delta) {
                    this.deadkeys().adjustPositions(this.getDeadkeyCaret(), delta);
                };
                /**
                 * Needed to properly clone deadkeys for use with Mock element interfaces toward predictive text purposes.
                 * @param {object}  dks   An existing set of deadkeys to deep-copy for use by this element interface.
                 */
                OutputTarget.prototype.setDeadkeys = function (dks) {
                    this._dks = dks.clone();
                };
                /**
                 * Determines the basic operations needed to reconstruct the current OutputTarget's text from the prior state specified
                 * by another OutputTarget based on their text and caret positions.
                 *
                 * This is designed for use as a "before and after" comparison to determine the effect of a single keyboard rule at a time.
                 * As such, it assumes that the caret is immediately after any inserted text.
                 * @param from An output target (preferably a Mock) representing the prior state of the input/output system.
                 */
                OutputTarget.prototype.buildTransformFrom = function (original) {
                    var to = this.getText();
                    var from = original.getText();
                    var fromCaret = original.getDeadkeyCaret();
                    var toCaret = this.getDeadkeyCaret();
                    // Step 1:  Determine the number of left-deletions.
                    for (var newCaret = 0; newCaret < fromCaret; newCaret++) {
                        if (from._kmwCharAt(newCaret) != to._kmwCharAt(newCaret)) {
                            break;
                        }
                    }
                    var deletedLeft = fromCaret - newCaret;
                    // Step 2:  Determine the other properties.
                    // Since the 'after' OutputTarget's caret indicates the end of any inserted text, we
                    // can easily calculate the rest.
                    var insertedLength = toCaret - newCaret;
                    var delta = to._kmwSubstr(newCaret, insertedLength);
                    var undeletedRight = to._kmwLength() - toCaret;
                    var originalRight = from._kmwLength() - fromCaret;
                    return new TextTransform(delta, deletedLeft, originalRight - undeletedRight);
                };
                OutputTarget.prototype.buildTranscriptionFrom = function (original, keyEvent, alternates) {
                    var transform = this.buildTransformFrom(original);
                    // If we ever decide to re-add deadkey tracking, this is the place for it.
                    return new Transcription(keyEvent, transform, Mock.from(original), alternates);
                };
                /**
                 * Restores the `OutputTarget` to the indicated state.  Designed for use with `Transcription.preInput`.
                 * @param original An `OutputTarget` (usually a `Mock`).
                 */
                OutputTarget.prototype.restoreTo = function (original) {
                    //
                    this.setTextBeforeCaret(original.getTextBeforeCaret());
                    this.setTextAfterCaret(original.getTextAfterCaret());
                    // Also, restore the deadkeys!
                    this._dks = original._dks.clone();
                };
                OutputTarget.prototype.apply = function (transform) {
                    if (transform.deleteRight) {
                        this.setTextAfterCaret(this.getTextAfterCaret()._kmwSubstr(transform.deleteRight));
                    }
                    if (transform.deleteLeft) {
                        this.deleteCharsBeforeCaret(transform.deleteLeft);
                    }
                    if (transform.insert) {
                        this.insertTextBeforeCaret(transform.insert);
                    }
                    // We assume that all deadkeys are invalidated after applying a Transform, since
                    // prediction implies we'll be completing a word, post-deadkeys.
                    this._dks.clear();
                };
                /**
                 * Helper to `restoreTo` - allows directly setting the 'before' context to that of another
                 * `OutputTarget`.
                 * @param s
                 */
                OutputTarget.prototype.setTextBeforeCaret = function (s) {
                    // This one's easy enough to provide a default implementation for.
                    this.deleteCharsBeforeCaret(this.getTextBeforeCaret()._kmwLength());
                    this.insertTextBeforeCaret(s);
                };
                /**
                 * Saves element-specific state properties prone to mutation, enabling restoration after
                 * text-output operations.
                 */
                OutputTarget.prototype.saveProperties = function () {
                    // Most element interfaces won't need anything here.
                };
                /**
                 * Restores previously-saved element-specific state properties.  Designed for use after text-output
                 * ops to facilitate more-seamless web-dev and user interactions.
                 */
                OutputTarget.prototype.restoreProperties = function () {
                    // Most element interfaces won't need anything here. 
                };
                return OutputTarget;
            }());
            text_1.OutputTarget = OutputTarget;
            // Due to some interesting requirements on compile ordering in TS,
            // this needs to be in the same file as OutputTarget now.
            var Mock = /** @class */ (function (_super) {
                __extends(Mock, _super);
                function Mock(text, caretPos) {
                    var _this = _super.call(this) || this;
                    _this.text = text ? text : "";
                    var defaultLength = _this.text._kmwLength();
                    _this.caretIndex = caretPos ? caretPos : defaultLength;
                    return _this;
                }
                // Clones the state of an existing EditableElement, creating a Mock version of its state.
                Mock.from = function (outputTarget) {
                    var preText = outputTarget.getTextBeforeCaret();
                    var caretIndex = preText._kmwLength();
                    // We choose to ignore (rather, pre-emptively remove) any actively-selected text,
                    // as since it's always removed instantly during any text mutation operations.
                    var clone = new Mock(preText + outputTarget.getTextAfterCaret(), caretIndex);
                    clone.setDeadkeys(outputTarget.deadkeys());
                    return clone;
                };
                Mock.prototype.clearSelection = function () {
                    return;
                };
                Mock.prototype.invalidateSelection = function () {
                    return;
                };
                Mock.prototype.hasSelection = function () {
                    return true;
                };
                Mock.prototype.getDeadkeyCaret = function () {
                    return this.caretIndex;
                };
                Mock.prototype.setDeadkeyCaret = function (index) {
                    if (index < 0 || index > this.text._kmwLength()) {
                        throw new Error("Provided caret index is out of range.");
                    }
                    this.caretIndex = index;
                };
                Mock.prototype.getTextBeforeCaret = function () {
                    return this.text.kmwSubstr(0, this.caretIndex);
                };
                Mock.prototype.getTextAfterCaret = function () {
                    return this.text.kmwSubstr(this.caretIndex);
                };
                Mock.prototype.getText = function () {
                    return this.text;
                };
                Mock.prototype.deleteCharsBeforeCaret = function (dn) {
                    if (dn >= 0) {
                        if (dn > this.caretIndex) {
                            dn = this.caretIndex;
                        }
                        this.text = this.text.kmwSubstr(0, this.caretIndex - dn) + this.getTextAfterCaret();
                        this.caretIndex -= dn;
                    }
                };
                Mock.prototype.insertTextBeforeCaret = function (s) {
                    this.text = this.getTextBeforeCaret() + s + this.getTextAfterCaret();
                    this.caretIndex += s.kmwLength();
                };
                Mock.prototype.handleNewlineAtCaret = function () {
                    this.insertTextBeforeCaret('\n');
                };
                Mock.prototype.setTextAfterCaret = function (s) {
                    this.text = this.getTextBeforeCaret() + s;
                };
                Mock.prototype.doInputEvent = function () {
                    // Mock isn't backed by an element, so it won't have any event listeners.
                };
                return Mock;
            }(OutputTarget));
            text_1.Mock = Mock;
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
/// <reference path="engineDeviceSpec.ts" />
/// <reference path="outputTarget.ts" />
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            /**
             * This class is defined within its own file so that it can be loaded by code outside of KMW without
             * having to actually load the entirety of KMW.
             */
            var KeyEvent = /** @class */ (function () {
                function KeyEvent() {
                    /**
                     * `true` if this event was produced by sources other than a DOM-based KeyboardEvent.
                     */
                    this.isSynthetic = true;
                }
                return KeyEvent;
            }());
            text.KeyEvent = KeyEvent;
            ;
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            /**
             * Represents the commands and state changes that result from a matched keyboard rule.
             */
            var RuleBehavior = /** @class */ (function () {
                function RuleBehavior() {
                    /**
                     * A set of changed store values triggered by the matched keyboard rule.
                     */
                    this.setStore = {};
                    /**
                     * A set of variable stores with save requests triggered by the matched keyboard rule
                     */
                    this.saveStore = {};
                }
                RuleBehavior.prototype.finalize = function (processor) {
                    var outputTarget = this.transcription.keystroke.Ltarg;
                    if (processor.beepHandler && this.beep) {
                        processor.beepHandler(outputTarget);
                    }
                    for (var storeID in this.setStore) {
                        var sysStore = processor.keyboardInterface.systemStores[storeID];
                        if (sysStore) {
                            try {
                                sysStore.set(this.setStore[storeID]);
                            }
                            catch (error) {
                                if (processor.errorLogger) {
                                    processor.errorLogger("Rule attempted to perform illegal operation - 'platform' may not be changed.");
                                }
                            }
                        }
                        else if (processor.warningLogger) {
                            processor.warningLogger("Unknown store affected by keyboard rule: " + storeID);
                        }
                    }
                    if (processor.keyboardInterface.variableStoreSerializer) {
                        for (var storeID in this.saveStore) {
                            processor.keyboardInterface.variableStoreSerializer.saveStore(processor.activeKeyboard.id, storeID, this.saveStore[storeID]);
                        }
                    }
                    if (this.triggersDefaultCommand) {
                        var keyEvent = this.transcription.keystroke;
                        text.DefaultOutput.applyCommand(keyEvent);
                    }
                    if (processor.warningLogger && this.warningLog) {
                        processor.warningLogger(this.warningLog);
                    }
                    else if (processor.errorLogger && this.errorLog) {
                        processor.errorLogger(this.errorLog);
                    }
                };
                return RuleBehavior;
            }());
            text.RuleBehavior = RuleBehavior;
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
// Establishes key-code definitions.
/// <reference path="codes.ts" />
// Defines our generalized "KeyEvent" class.
/// <reference path="keyEvent.ts" />
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            var EmulationKeystrokes;
            (function (EmulationKeystrokes) {
                EmulationKeystrokes["Space"] = " ";
                EmulationKeystrokes["Enter"] = "\n";
                EmulationKeystrokes["Backspace"] = "\b";
            })(EmulationKeystrokes = text.EmulationKeystrokes || (text.EmulationKeystrokes = {}));
            /**
             * Defines a collection of static library functions that define KeymanWeb's default (implied) keyboard rule behaviors.
             */
            var DefaultOutput = /** @class */ (function () {
                function DefaultOutput() {
                }
                DefaultOutput.codeForEvent = function (Lkc) {
                    return text.Codes.keyCodes[Lkc.kName] || Lkc.Lcode;
                    ;
                };
                /**
                 * Serves as a default keycode lookup table.  This may be referenced safely by mnemonic handling without fear of side-effects.
                 * Also used by Processor.defaultRuleBehavior to generate output after filtering for special cases.
                 */
                DefaultOutput.forAny = function (Lkc, isMnemonic) {
                    var char = '';
                    // A pretty simple table of lookups, corresponding VERY closely to the original defaultKeyOutput.
                    if ((char = DefaultOutput.forSpecialEmulation(Lkc)) != null) {
                        return char;
                    }
                    else if (!isMnemonic && ((char = DefaultOutput.forNumpadKeys(Lkc)) != null)) {
                        return char;
                    }
                    else if ((char = DefaultOutput.forUnicodeKeynames(Lkc)) != null) {
                        return char;
                    }
                    else if ((char = DefaultOutput.forBaseKeys(Lkc)) != null) {
                        return char;
                    }
                    else {
                        // // For headless and embeddded, we may well allow '\t'.  It's DOM mode that has other uses.
                        // // Not originally defined for text output within defaultKeyOutput.
                        // // We can't enable it yet, as it'll cause hardware keystrokes in the DOM to output '\t' rather
                        // // than rely on the browser-default handling.
                        var code = DefaultOutput.codeForEvent(Lkc);
                        switch (code) {
                            //   case Codes.keyCodes['K_TAB']:
                            //   case Codes.keyCodes['K_TABBACK']:
                            //   case Codes.keyCodes['K_TABFWD']:
                            //     return '\t';
                            default:
                                return '';
                        }
                    }
                };
                /**
                 * isCommand - returns a boolean indicating if a non-text event should be triggered by the keystroke.
                 */
                DefaultOutput.isCommand = function (Lkc) {
                    var code = DefaultOutput.codeForEvent(Lkc);
                    switch (code) {
                        // Should we ever implement them:
                        // case Codes.keyCodes['K_LEFT']:  // would not output text, but would alter the caret's position in the context.
                        // case Codes.keyCodes['K_RIGHT']:
                        //   return true;
                        default:
                            return false;
                    }
                };
                /**
                 * Used when a RuleBehavior represents a non-text "command" within the Engine.  This will generally
                 * trigger events that require context reset - often by moving the caret or by moving what OutputTarget
                 * the caret is in.  However, we let those events perform the actual context reset.
                 *
                 * Note:  is extended by DOM-aware KeymanWeb code.
                 */
                DefaultOutput.applyCommand = function (Lkc) {
                    // Notes for potential default-handling extensions:
                    // 
                    // switch(code) {
                    // // Problem:  clusters, and doing them right.
                    // // The commented-out code below should be a decent starting point, but clusters make it complex.
                    // // Mostly based on pre-12.0 code, but the general idea should be relatively clear.
                    //
                    // case Codes.keyCodes['K_LEFT']:
                    //   if(touchAlias) {
                    //     var caretPos = keymanweb.getTextCaret(Lelem);
                    //     keymanweb.setTextCaret(Lelem, caretPos - 1 >= 0 ? caretPos - 1 : 0);
                    //   }
                    //   break;
                    // case Codes.keyCodes['K_RIGHT']:
                    //   if(touchAlias) {
                    //     var caretPos = keymanweb.getTextCaret(Lelem);
                    //     keymanweb.setTextCaret(Lelem, caretPos + 1);
                    //   }
                    //   if(code == VisualKeyboard.keyCodes['K_RIGHT']) {
                    //     break;
                    //   }
                    // }
                    //
                    // Note that these would be useful even outside of a DOM context.
                };
                /**
                 * Codes matched here generally have default implementations when in a browser but require emulation
                 * for 'synthetic' `OutputTarget`s like `Mock`s, which have no default text handling.
                 */
                DefaultOutput.forSpecialEmulation = function (Lkc) {
                    var code = DefaultOutput.codeForEvent(Lkc);
                    switch (code) {
                        case text.Codes.keyCodes['K_BKSP']:
                            return EmulationKeystrokes.Backspace;
                        case text.Codes.keyCodes['K_ENTER']:
                            return EmulationKeystrokes.Enter;
                        // (Probably) only here for legacy reasons; it's always been handled alongside the other two.
                        case text.Codes.keyCodes['K_SPACE']:
                            return EmulationKeystrokes.Space;
                        // case Codes.keyCodes['K_DEL']:
                        //   return '\u007f'; // 127, ASCII / Unicode control code for DEL.
                        default:
                            return null;
                    }
                };
                // Should not be used for mnenomic keyboards.  forAny()'s use of this method checks first.
                DefaultOutput.forNumpadKeys = function (Lkc) {
                    // Translate numpad keystrokes into their non-numpad equivalents
                    if (Lkc.Lcode >= text.Codes.keyCodes["K_NP0"] && Lkc.Lcode <= text.Codes.keyCodes["K_NPSLASH"]) {
                        // Number pad, numlock on
                        if (Lkc.Lcode < 106) {
                            var Lch = Lkc.Lcode - 48;
                        }
                        else {
                            Lch = Lkc.Lcode - 64;
                        }
                        var ch = String._kmwFromCharCode(Lch); //I3319
                        return ch;
                    }
                    else {
                        return null;
                    }
                };
                // Test for fall back to U_xxxxxx key id
                // For this first test, we ignore the keyCode and use the keyName
                DefaultOutput.forUnicodeKeynames = function (Lkc, ruleBehavior) {
                    var keyName = Lkc.kName;
                    // Test for fall back to U_xxxxxx key id
                    // For this first test, we ignore the keyCode and use the keyName
                    if (!keyName || keyName.substr(0, 2) != 'U_') {
                        return null;
                    }
                    var codePoint = parseInt(keyName.substr(2, 6), 16);
                    if (((0x0 <= codePoint) && (codePoint <= 0x1F)) || ((0x80 <= codePoint) && (codePoint <= 0x9F))) {
                        // Code points [U_0000 - U_001F] and [U_0080 - U_009F] refer to Unicode C0 and C1 control codes.
                        // Check the codePoint number and do not allow output of these codes via U_xxxxxx shortcuts.
                        if (ruleBehavior) {
                            ruleBehavior.errorLog = ("Suppressing Unicode control code: U_00" + codePoint.toString(16));
                        }
                        return null;
                    }
                    else {
                        // String.fromCharCode() is inadequate to handle the entire range of Unicode
                        // Someday after upgrading to ES2015, can use String.fromCodePoint()
                        return String.kmwFromCharCode(codePoint);
                    }
                };
                // Test for otherwise unimplemented keys on the the base default & shift layers.
                // Those keys must be blocked by keyboard rules if intentionally unimplemented; otherwise, this function will trigger.
                DefaultOutput.forBaseKeys = function (Lkc, ruleBehavior) {
                    var n = Lkc.Lcode;
                    var keyShiftState = Lkc.Lmodifiers;
                    // check if exact match to SHIFT's code.  Only the 'default' and 'shift' layers should have default key outputs.
                    // TODO:  Extend to allow AltGr as well - better mnemonic support.
                    if (keyShiftState == text.Codes.modifierCodes['SHIFT']) {
                        keyShiftState = 1;
                    }
                    else if (keyShiftState != 0) {
                        if (ruleBehavior) {
                            ruleBehavior.warningLog = "KMW only defines default key output for the 'default' and 'shift' layers!";
                        }
                        return null;
                    }
                    // Now that keyShiftState is either 0 or 1, we can use the following structure to determine the default output.
                    try {
                        if (n >= text.Codes.keyCodes['K_0'] && n <= text.Codes.keyCodes['K_9']) { // The number keys.
                            return text.Codes.codesUS[keyShiftState][0][n - text.Codes.keyCodes['K_0']];
                        }
                        else if (n >= text.Codes.keyCodes['K_A'] && n <= text.Codes.keyCodes['K_Z']) { // The base letter keys
                            return String.fromCharCode(n + (keyShiftState ? 0 : 32)); // 32 is the offset from uppercase to lowercase.
                        }
                        else if (n >= text.Codes.keyCodes['K_COLON'] && n <= text.Codes.keyCodes['K_BKQUOTE']) {
                            return text.Codes.codesUS[keyShiftState][1][n - text.Codes.keyCodes['K_COLON']];
                        }
                        else if (n >= text.Codes.keyCodes['K_LBRKT'] && n <= text.Codes.keyCodes['K_QUOTE']) {
                            return text.Codes.codesUS[keyShiftState][2][n - text.Codes.keyCodes['K_LBRKT']];
                        }
                    }
                    catch (e) {
                        if (ruleBehavior) {
                            ruleBehavior.errorLog = "Error detected with default mapping for key:  code = " + n + ", shift state = " + (keyShiftState == 1 ? 'shift' : 'default');
                        }
                    }
                    return null;
                };
                return DefaultOutput;
            }());
            text.DefaultOutput = DefaultOutput;
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
//Autogenerated file - do not modify!
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var environment;
        (function (environment) {
            environment.VERSION = "14.0";
            environment.BUILD = 52;
        })(environment = keyman.environment || (keyman.environment = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
// Defines build-environment variables, as used for versioning.
/// <reference path="../environment.inc.ts" />
// Ensure that this class contains no reference into core KMW code - it is referenced
// by components intended to be modular and possible to separate from core KMW.
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var utils;
        (function (utils) {
            // Dotted-decimal version
            var Version = /** @class */ (function () {
                /**
                 * Parses version information, preparing it for use in comparisons.
                 * @param text Either a string representing a version number (ex: "9.0.0") or an array representing
                 *             its components (ex: [9, 0, 0]).
                 */
                function Version(text) {
                    // If a keyboard doesn't specify a version, use the DEVELOPER_VERSION_FALLBACK values.
                    if (text === undefined || text === null) {
                        this.components = [].concat(Version.DEVELOPER_VERSION_FALLBACK.components);
                        return;
                    }
                    if (Array.isArray(text)) {
                        var components = text;
                        if (components.length < 2) {
                            throw new Error("Version string must have at least a major and minor component!");
                        }
                        else {
                            this.components = [].concat(components);
                            return;
                        }
                    }
                    // else, standard constructor path.
                    var parts = text.split('.');
                    var componentArray = [];
                    if (parts.length < 2) {
                        throw new Error("Version string must have at least a major and minor component!");
                    }
                    for (var i = 0; i < parts.length; i++) {
                        var value = parseInt(parts[i], 10);
                        if (isNaN(value)) {
                            throw new Error("Version string components must be numerical!");
                        }
                        componentArray.push(value);
                    }
                    this.components = componentArray;
                }
                Object.defineProperty(Version.prototype, "major", {
                    get: function () {
                        return this.components[0];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Version.prototype, "minor", {
                    get: function () {
                        return this.components[1];
                    },
                    enumerable: true,
                    configurable: true
                });
                Version.prototype.toString = function () {
                    return this.components.join('.');
                };
                Version.prototype.toJSON = function () {
                    return this.toString();
                };
                Version.prototype.equals = function (other) {
                    return this.compareTo(other) == 0;
                };
                Version.prototype.precedes = function (other) {
                    return this.compareTo(other) < 0;
                };
                Version.prototype.compareTo = function (other) {
                    // If the version info depth differs, we need a flag to indicate which instance is shorter.
                    var isShorter = this.components.length < other.components.length;
                    var maxDepth = (this.components.length < other.components.length) ? this.components.length : other.components.length;
                    var i;
                    for (i = 0; i < maxDepth; i++) {
                        var delta = this.components[i] - other.components[i];
                        if (delta != 0) {
                            return delta;
                        }
                    }
                    var longList = isShorter ? other.components : this.components;
                    do {
                        if (longList[i] > 0) {
                            return isShorter ? -1 : 1;
                        }
                        i++;
                    } while (i < longList.length);
                    // Equal.
                    return 0;
                };
                Version.CURRENT = new Version(com.keyman.environment.VERSION);
                // Represents a default version value for keyboards compiled before this was compiled into keyboards.
                // The exact version is unknown at this point, but the value is "good enough" for what we need.
                Version.DEVELOPER_VERSION_FALLBACK = new Version([9, 0, 0]);
                // For 12.0, the old default behavior of adding missing keycaps to the default layers was removed,
                // as it results in unexpected, bug-like behavior for keyboard designers when it is unwanted.
                Version.NO_DEFAULT_KEYCAPS = new Version([12, 0]);
                Version.MAC_POSSIBLE_IPAD_ALIAS = new Version([10, 15]);
                return Version;
            }());
            utils.Version = Version;
        })(utils = keyman.utils || (keyman.utils = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var utils;
        (function (utils) {
            /**
             * Function     deepCopy
             * Scope        Private
             * @param       {Object}      p           object to copy
             * @param       {Array=}      c0          array member being copied
             * @return      {Object}                  clone ('deep copy') of object
             * Description  Makes an actual copy (not a reference) of an object, copying simple members,
             *              arrays and member objects but not functions, so use with care!
             */
            function deepCopy(p, c0) {
                var c = c0 || {};
                for (var i in p) {
                    if (typeof p[i] === 'object' && p[i] != null) {
                        c[i] = (p[i].constructor === Array) ? [] : {};
                        deepCopy(p[i], c[i]);
                    }
                    else {
                        c[i] = p[i];
                    }
                }
                return c;
            }
            utils.deepCopy = deepCopy;
        })(utils = keyman.utils || (keyman.utils = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
/***
   KeymanWeb 10.0
   Copyright 2017 SIL International
***/
///<reference path="../utils/version.ts"/>
///<reference path="../utils/deepCopy.ts"/>
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var keyboards;
        (function (keyboards) {
            var Codes = com.keyman.text.Codes;
            // This class manages default layout construction for consumption by OSKs without a specified layout.
            var Layouts = /** @class */ (function () {
                function Layouts() {
                }
                /**
                * Build a default layout for keyboards with no explicit layout
                *
                * @param   {Object}  PVK             keyboard object (as loaded)
                * @param   {Object}  kbdDevVersion   object representing the version of Developer that compiled the keyboard
                * @param   {number}  kbdBitmask      keyboard modifier bitmask
                * @param   {string}  formFactor
                * @return  {Object}
                */
                Layouts.buildDefaultLayout = function (PVK, keyboard, formFactor) {
                    // Build a layout using the default for the device
                    var layoutType = formFactor;
                    if (typeof Layouts.dfltLayout[layoutType] != 'object') {
                        layoutType = 'desktop';
                    }
                    var kbdBitmask = Codes.modifierBitmasks['NON_CHIRAL'];
                    // An unfortunate dependency there.  Should probably also set a version within web-core for use.
                    var kbdDevVersion = keyman.utils.Version.CURRENT;
                    if (keyboard) {
                        kbdBitmask = keyboard.modifierBitmask;
                        kbdDevVersion = keyboard.compilerVersion;
                    }
                    if (!PVK) {
                        PVK = this.DEFAULT_RAW_SPEC;
                    }
                    // Clone the default layout object for this device
                    var layout = keyman.utils.deepCopy(Layouts.dfltLayout[layoutType]);
                    var n, layers = layout['layer'], keyLabels = PVK['KLS'], key102 = PVK['K102'];
                    var i, j, k, m, row, rows, key, keys;
                    var chiral = (kbdBitmask & Codes.modifierBitmasks.IS_CHIRAL) != 0;
                    var kmw10Plus = !(typeof keyLabels == 'undefined' || !keyLabels);
                    if (!kmw10Plus) {
                        // Save the processed key label information to the keyboard's general data.
                        // Makes things more efficient elsewhere and for reloading after keyboard swaps.
                        keyLabels = PVK['KLS'] = Layouts.processLegacyDefinitions(PVK['BK']);
                    }
                    // Identify key labels (e.g. *Shift*) that require the special OSK font
                    var specialLabel = /\*\w+\*/;
                    // *** Step 1:  instantiate the layer objects. ***
                    // Get the list of valid layers, enforcing that the 'default' layer must be the first one processed.
                    var validIdList = Object.getOwnPropertyNames(keyLabels), invalidIdList = [];
                    validIdList.splice(validIdList.indexOf('default'), 1);
                    validIdList = ['default'].concat(validIdList);
                    // Automatic AltGr emulation if the 'leftctrl-leftalt' layer is otherwise undefined.
                    if (keyboard && keyboard.emulatesAltGr) {
                        // We insert only the layers that need to be emulated.
                        if ((validIdList.indexOf('leftctrl-leftalt') == -1) && validIdList.indexOf('rightalt') != -1) {
                            validIdList.push('leftctrl-leftalt');
                            keyLabels['leftctrl-leftalt'] = keyLabels['rightalt'];
                        }
                        if ((validIdList.indexOf('leftctrl-leftalt-shift') == -1) && validIdList.indexOf('rightalt-shift') != -1) {
                            validIdList.push('leftctrl-leftalt-shift');
                            keyLabels['leftctrl-leftalt-shift'] = keyLabels['rightalt-shift'];
                        }
                    }
                    // For desktop devices, we must create all layers, even if invalid.
                    if (formFactor == 'desktop') {
                        invalidIdList = Layouts.generateLayerIds(chiral);
                        // Filter out all ids considered valid.  (We also don't want duplicates in the following list...)
                        for (n = 0; n < invalidIdList.length; n++) {
                            if (validIdList.indexOf(invalidIdList[n]) != -1) {
                                invalidIdList.splice(n--, 1);
                            }
                        }
                    }
                    // This ensures all 'valid' layers are at the front of the layer array and managed by the main loop below.
                    // 'invalid' layers aren't handled by the loop and thus remain blank after it.
                    var idList = validIdList.concat(invalidIdList);
                    if (kmw10Plus && formFactor != 'desktop') { // KLS exists, so we know the exact layer set.
                        // Find the SHIFT key...
                        var shiftKey = null;
                        rows = layers[0]['row'];
                        for (var r = 0; r < rows.length; r++) {
                            keys = rows[r]['key'];
                            for (var c = 0; c < keys.length; c++) {
                                key = keys[c];
                                if (key['id'] == 'K_SHIFT') {
                                    shiftKey = key;
                                }
                            }
                        }
                        if (shiftKey) {
                            // Erase the legacy shifted subkey array.
                            shiftKey['sk'] = [];
                            for (var layerID in keyLabels) {
                                if (layerID == 'default' || layerID == 'shift') {
                                    // These two are accessible from the layer without subkeys.
                                    continue;
                                }
                                // Create a new subkey for the specified layer so that it will be accessible via OSK.
                                var specialChar = Layouts.modifierSpecials[layerID];
                                var subkey = {
                                    id: "K_" + specialChar,
                                    text: specialChar,
                                    sp: "1",
                                    nextlayer: layerID
                                };
                                shiftKey['sk'].push(subkey);
                            }
                        }
                        else {
                            // Seriously, this should never happen.  It's here for the debugging log only.
                            console.warn("Error in default layout - cannot find default Shift key!");
                        }
                    }
                    for (n = 0; n < idList.length; n++) {
                        // Populate non-default (shifted) keygroups
                        if (n > 0) {
                            layers[n] = keyman.utils.deepCopy(layers[0]);
                        }
                        layers[n]['id'] = idList[n];
                        layers[n]['nextlayer'] = idList[n]; // This would only be different for a dynamic keyboard
                        // Extraced into a helper method to improve readability.
                        Layouts.formatDefaultLayer(layers[n], chiral, formFactor, !!key102);
                    }
                    // *** Step 2: Layer objects now exist; time to fill them with the appropriate key labels and key styles ***
                    for (n = 0; n < layers.length; n++) {
                        var layer = layers[n], kx, shiftKey = null, nextKey = null, allText = '';
                        var capsKey = null, numKey = null, scrollKey = null; // null if not in the OSK layout.
                        var layerSpec = keyLabels[layer['id']];
                        var isShift = layer['id'] == 'shift' ? 1 : 0;
                        var isDefault = layer['id'] == 'default' || isShift ? 1 : 0;
                        rows = layer['row'];
                        for (i = 0; i < rows.length; i++) {
                            keys = rows[i]['key'];
                            for (j = 0; j < keys.length; j++) {
                                key = keys[j];
                                kx = Layouts.dfltCodes.indexOf(key['id']);
                                // Only create keys for defined layers.  ('default' and 'shift' are always defined.)
                                if (layerSpec || isDefault) {
                                    // Get keycap text from visual keyboard array, if defined in keyboard
                                    if (layerSpec) {
                                        if (kx >= 0 && kx < layerSpec.length)
                                            key['text'] = layerSpec[kx];
                                    }
                                    // Legacy (pre 12.0) behavior:  fall back to US English keycap text as default for the base two layers
                                    // if a key cap is not otherwise defined. (Any intentional 'ghost' keys must be explicitly defined.)
                                    if (isDefault && kbdDevVersion.precedes(keyman.utils.Version.NO_DEFAULT_KEYCAPS)) {
                                        if (key['id'] != 'K_SPACE' && kx + 65 * isShift < Layouts.dfltText.length && key['text'] !== null) {
                                            key['text'] = key['text'] || Layouts.dfltText[kx + 65 * isShift];
                                        }
                                    }
                                }
                                // Leave any unmarked key caps as null strings
                                if (key['text'] !== null) {
                                    key['text'] = key['text'] || '';
                                }
                                // Detect important tracking keys.
                                switch (key['id']) {
                                    case "K_SHIFT":
                                        shiftKey = key;
                                        break;
                                    case "K_TAB":
                                        nextKey = key;
                                        break;
                                    case "K_CAPS":
                                        capsKey = key;
                                        break;
                                    case "K_NUMLOCK":
                                        numKey = key;
                                        break;
                                    case "K_SCROLL":
                                        scrollKey = key;
                                        break;
                                }
                                // Remove pop-up shift keys referencing invalid layers (Build 349)
                                if (key['sk'] != null) {
                                    for (k = 0; k < key['sk'].length; k++) {
                                        if (validIdList.indexOf(key['sk'][k]['nextlayer']) == -1) {
                                            key['sk'].splice(k--, 1);
                                        }
                                    }
                                    if (key['sk'].length == 0) {
                                        key['sk'] = null;
                                    }
                                }
                            }
                        }
                        // We're done with the layer keys initialization pass.  Time to do post-analysis layer-level init where necessary.
                        layer.shiftKey = shiftKey;
                        layer.capsKey = capsKey;
                        layer.numKey = numKey;
                        layer.scrollKey = scrollKey;
                        // Set modifier key appearance and behaviour for non-desktop devices using the default layout
                        if (formFactor != 'desktop') {
                            if (n > 0 && shiftKey != null) {
                                shiftKey['sp'] = Layouts.buttonClasses['SHIFT-ON'];
                                shiftKey['sk'] = null;
                                shiftKey['text'] = Layouts.modifierSpecials[layers[n].id] ? Layouts.modifierSpecials[layers[n].id] : "*Shift*";
                            }
                        }
                    }
                    return layout;
                };
                /**
             * Function     getLayerId
             * Scope        Private
             * @param       {number}      m     shift modifier code
             * @return      {string}            layer string from shift modifier code (desktop keyboards)
             * Description  Get name of layer from code, where the modifer order is determined by ascending bit-flag value.
             */
                Layouts.getLayerId = function (m) {
                    var modifierCodes = Codes.modifierCodes;
                    var s = '';
                    if (m == 0) {
                        return 'default';
                    }
                    else {
                        if (m & modifierCodes['LCTRL']) {
                            s = (s.length > 0 ? s + '-' : '') + 'leftctrl';
                        }
                        if (m & modifierCodes['RCTRL']) {
                            s = (s.length > 0 ? s + '-' : '') + 'rightctrl';
                        }
                        if (m & modifierCodes['LALT']) {
                            s = (s.length > 0 ? s + '-' : '') + 'leftalt';
                        }
                        if (m & modifierCodes['RALT']) {
                            s = (s.length > 0 ? s + '-' : '') + 'rightalt';
                        }
                        if (m & modifierCodes['SHIFT']) {
                            s = (s.length > 0 ? s + '-' : '') + 'shift';
                        }
                        if (m & modifierCodes['CTRL']) {
                            s = (s.length > 0 ? s + '-' : '') + 'ctrl';
                        }
                        if (m & modifierCodes['ALT']) {
                            s = (s.length > 0 ? s + '-' : '') + 'alt';
                        }
                        return s;
                    }
                };
                /**
                 * Generates a list of potential layer ids for the specified chirality mode.
                 *
                 * @param   {boolean}   chiral    // Does the keyboard use chiral modifiers or not?
                 */
                Layouts.generateLayerIds = function (chiral) {
                    var layerCnt, offset;
                    if (chiral) {
                        layerCnt = 32;
                        offset = 0x01;
                    }
                    else {
                        layerCnt = 8;
                        offset = 0x10;
                    }
                    var layerIds = [];
                    for (var i = 0; i < layerCnt; i++) {
                        layerIds.push(Layouts.getLayerId(i * offset));
                    }
                    return layerIds;
                };
                /**
                 * Sets a formatting property for the modifier keys when constructing a default layout for a keyboard.
                 *
                 * @param   {Object}    layer   // One layer specification
                 * @param   {boolean}   chiral  // Whether or not the keyboard uses chiral modifier information.
                 * @param   {string}    formFactor  // The form factor of the device the layout is being constructed for.
                 * @param   {boolean}   key102      // Whether or not the extended key 102 should be hidden.
                 */
                Layouts.formatDefaultLayer = function (layer, chiral, formFactor, key102) {
                    var layerId = layer['id'];
                    var buttonClasses = Layouts.buttonClasses;
                    // Correct appearance of state-dependent modifier keys according to group
                    for (var i = 0; i < layer['row'].length; i++) {
                        var row = layer['row'][i];
                        var keys = row['key'];
                        for (var j = 0; j < keys.length; j++) {
                            var key = keys[j];
                            switch (key['id']) {
                                case 'K_SHIFT':
                                case 'K_LSHIFT':
                                case 'K_RSHIFT':
                                    if (layerId.indexOf('shift') != -1) {
                                        key['sp'] = buttonClasses['SHIFT-ON'];
                                    }
                                    if ((formFactor != 'desktop') && (layerId != 'default')) {
                                        key['nextlayer'] = 'default';
                                    }
                                    break;
                                case 'K_LCTRL':
                                case 'K_LCONTROL':
                                    if (chiral) {
                                        if (layerId.indexOf('leftctrl') != -1) {
                                            key['sp'] = buttonClasses['SHIFT-ON'];
                                        }
                                        break;
                                    }
                                case 'K_RCTRL':
                                case 'K_RCONTROL':
                                    if (chiral) {
                                        if (layerId.indexOf('rightctrl') != -1) {
                                            key['sp'] = buttonClasses['SHIFT-ON'];
                                        }
                                        break;
                                    }
                                case 'K_CONTROL':
                                    if (layerId.indexOf('ctrl') != -1) {
                                        if (!chiral || (layerId.indexOf('leftctrl') != -1 && layerId.indexOf('rightctrl') != -1)) {
                                            key['sp'] = buttonClasses['SHIFT-ON'];
                                        }
                                    }
                                    break;
                                case 'K_LALT':
                                    if (chiral) {
                                        if (layerId.indexOf('leftalt') != -1) {
                                            key['sp'] = buttonClasses['SHIFT-ON'];
                                        }
                                        break;
                                    }
                                case 'K_RALT':
                                    if (chiral) {
                                        if (layerId.indexOf('rightalt') != -1) {
                                            key['sp'] = buttonClasses['SHIFT-ON'];
                                        }
                                        break;
                                    }
                                case 'K_ALT':
                                    if (layerId.indexOf('alt') != -1) {
                                        if (!chiral || (layerId.indexOf('leftalt') != -1 && layerId.indexOf('rightalt') != -1)) {
                                            key['sp'] = buttonClasses['SHIFT-ON'];
                                        }
                                    }
                                    break;
                                case 'K_oE2':
                                    if (typeof key102 == 'undefined' || !key102) {
                                        if (formFactor == 'desktop') {
                                            keys.splice(j--, 1);
                                            keys[0]['width'] = '200';
                                        }
                                        else {
                                            keys[j]['sp'] = buttonClasses['HIDDEN'];
                                        }
                                    }
                                    break;
                            }
                        }
                    }
                };
                /**
                 * Converts the legacy BK property from pre 10.0 into the KLS keyboard layer spec format,
                 * sparsifying it as possible to pre-emptively check invalid layers.
                 *
                 * @param   {Array}   BK      keyboard object (as loaded)
                 * @return  {Object}
                 */
                Layouts.processLegacyDefinitions = function (BK) {
                    //['default','shift','ctrl','shiftctrl','alt','shiftalt','ctrlalt','shiftctrlalt'];
                    var idList = Layouts.generateLayerIds(false); // Non-chiral.
                    var KLS = {};
                    // The old default:  eight auto-managed layers...
                    for (var n = 0; n < idList.length; n++) {
                        var id = idList[n], arr = [], valid = false;
                        // ... with keycode mappings in blocks of 65.
                        for (var k = 0; k < 65; k++) {
                            var index = k + 65 * n;
                            arr.push(BK[index]);
                            // The entry for K_SPACE's keycode tends to hold ' ' instead of '', which causes
                            // the whole layer to be treated as 'valid' if not included in the conditional.
                            if (index < BK.length && BK[index] != '' && k != Layouts.dfltCodes.indexOf('K_SPACE')) {
                                valid = true;
                            }
                        }
                        if (valid) {
                            KLS[id] = arr;
                        }
                    }
                    // There must always be at least a plain 'default' layer.  Array(65).fill('') would be preferable but isn't supported on IE, 
                    // but buildDefaultLayer will set the defaults for these layers if no entry exists for them in the array due to length.
                    if (typeof KLS['default'] == 'undefined' || !KLS['default']) {
                        KLS['default'] = [''];
                    }
                    // There must always be at least a plain 'shift' layer.
                    if (typeof KLS['shift'] == 'undefined' || !KLS['shift']) {
                        KLS['shift'] = [''];
                    }
                    return KLS;
                };
                Layouts.dfltCodes = [
                    "K_BKQUOTE", "K_1", "K_2", "K_3", "K_4", "K_5", "K_6", "K_7", "K_8", "K_9", "K_0",
                    "K_HYPHEN", "K_EQUAL", "K_*", "K_*", "K_*", "K_Q", "K_W", "K_E", "K_R", "K_T",
                    "K_Y", "K_U", "K_I", "K_O", "K_P", "K_LBRKT", "K_RBRKT", "K_BKSLASH", "K_*",
                    "K_*", "K_*", "K_A", "K_S", "K_D", "K_F", "K_G", "K_H", "K_J", "K_K", "K_L",
                    "K_COLON", "K_QUOTE", "K_*", "K_*", "K_*", "K_*", "K_*", "K_oE2",
                    "K_Z", "K_X", "K_C", "K_V", "K_B", "K_N", "K_M", "K_COMMA", "K_PERIOD",
                    "K_SLASH", "K_*", "K_*", "K_*", "K_*", "K_*", "K_SPACE"
                ];
                Layouts.dfltText = '`1234567890-=\xA7~~qwertyuiop[]\\~~~asdfghjkl;\'~~~~~?zxcvbnm,./~~~~~ '
                    + '~!@#$%^&*()_+\xA7~~QWERTYUIOP{}\\~~~ASDFGHJKL:"~~~~~?ZXCVBNM<>?~~~~~ ';
                Layouts.DEFAULT_RAW_SPEC = { 'F': 'Tahoma', 'BK': Layouts.dfltText };
                // Cross-reference with the ids in osk.setButtonClass.
                Layouts.buttonClasses = {
                    'DEFAULT': '0',
                    'SHIFT': '1',
                    'SHIFT-ON': '2',
                    'SPECIAL': '3',
                    'SPECIAL-ON': '4',
                    'DEADKEY': '8',
                    'BLANK': '9',
                    'HIDDEN': '10'
                };
                Layouts.modifierSpecials = {
                    'leftalt': '*LAlt*',
                    'rightalt': '*RAlt*',
                    'alt': '*Alt*',
                    'leftctrl': '*LCtrl*',
                    'rightctrl': '*RCtrl*',
                    'ctrl': '*Ctrl*',
                    'ctrl-alt': '*AltGr*',
                    'leftctrl-leftalt': '*LAltCtrl*',
                    'rightctrl-rightalt': '*RAltCtrl*',
                    'leftctrl-leftalt-shift': '*LAltCtrlShift*',
                    'rightctrl-rightalt-shift': '*RAltCtrlShift*',
                    'shift': '*Shift*',
                    'shift-alt': '*AltShift*',
                    'shift-ctrl': '*CtrlShift*',
                    'shift-ctrl-alt': '*AltCtrlShift*',
                    'leftalt-shift': '*LAltShift*',
                    'rightalt-shift': '*RAltShift*',
                    'leftctrl-shift': '*LCtrlShift*',
                    'rightctrl-shift': '*RCtrlShift*'
                };
                // Defines the default visual layout for a keyboard.
                Layouts.dfltLayout = {
                    "desktop": {
                        "font": "Tahoma,Helvetica",
                        "layer": [
                            {
                                "id": "default",
                                "row": [
                                    {
                                        "id": "1",
                                        "key": [
                                            { "id": "K_BKQUOTE" },
                                            { "id": "K_1" },
                                            { "id": "K_2" },
                                            { "id": "K_3" },
                                            { "id": "K_4" },
                                            { "id": "K_5" },
                                            { "id": "K_6" },
                                            { "id": "K_7" },
                                            { "id": "K_8" },
                                            { "id": "K_9" },
                                            { "id": "K_0" },
                                            { "id": "K_HYPHEN" },
                                            { "id": "K_EQUAL" },
                                            { "id": "K_BKSP", "text": "*BkSp*", "sp": "1", "width": "130" }
                                        ]
                                    },
                                    {
                                        "id": "2",
                                        "key": [
                                            { "id": "K_TAB", "text": "*Tab*", "sp": "1", "width": "130" },
                                            { "id": "K_Q" },
                                            { "id": "K_W" },
                                            { "id": "K_E" },
                                            { "id": "K_R" },
                                            { "id": "K_T" },
                                            { "id": "K_Y" },
                                            { "id": "K_U" },
                                            { "id": "K_I" },
                                            { "id": "K_O" },
                                            { "id": "K_P" },
                                            { "id": "K_LBRKT" },
                                            { "id": "K_RBRKT" },
                                            { "id": "K_BKSLASH" }
                                        ]
                                    },
                                    {
                                        "id": "3",
                                        "key": [
                                            { "id": "K_CAPS", "text": "*Caps*", "sp": "1", "width": "165" },
                                            { "id": "K_A" },
                                            { "id": "K_S" },
                                            { "id": "K_D" },
                                            { "id": "K_F" },
                                            { "id": "K_G" },
                                            { "id": "K_H" },
                                            { "id": "K_J" },
                                            { "id": "K_K" },
                                            { "id": "K_L" },
                                            { "id": "K_COLON" },
                                            { "id": "K_QUOTE" },
                                            { "id": "K_ENTER", "text": "*Enter*", "sp": "1", "width": "165" }
                                        ]
                                    },
                                    {
                                        "id": "4",
                                        "key": [
                                            { "id": "K_SHIFT", "text": "*Shift*", "sp": "1", "width": "130" },
                                            { "id": "K_oE2" },
                                            { "id": "K_Z" },
                                            { "id": "K_X" },
                                            { "id": "K_C" },
                                            { "id": "K_V" },
                                            { "id": "K_B" },
                                            { "id": "K_N" },
                                            { "id": "K_M" },
                                            { "id": "K_COMMA" },
                                            { "id": "K_PERIOD" },
                                            { "id": "K_SLASH" },
                                            { "id": "K_RSHIFT", "text": "*Shift*", "sp": "1", "width": "130" }
                                        ]
                                    },
                                    {
                                        "id": "5",
                                        "key": [
                                            { "id": "K_LCONTROL", "text": "*Ctrl*", "sp": "1", "width": "170" },
                                            { "id": "K_LALT", "text": "*Alt*", "sp": "1", "width": "160" },
                                            { "id": "K_SPACE", "text": "", "width": "770" },
                                            { "id": "K_RALT", "text": "*Alt*", "sp": "1", "width": "160" },
                                            { "id": "K_RCONTROL", "text": "*Ctrl*", "sp": "1", "width": "170" }
                                        ]
                                    }
                                ]
                            }
                        ]
                    },
                    "tablet": {
                        "font": "Tahoma,Helvetica",
                        "layer": [
                            {
                                "id": "default",
                                "row": [
                                    {
                                        "id": "0",
                                        "key": [
                                            { "id": "K_1" },
                                            { "id": "K_2" },
                                            { "id": "K_3" },
                                            { "id": "K_4" },
                                            { "id": "K_5" },
                                            { "id": "K_6" },
                                            { "id": "K_7" },
                                            { "id": "K_8" },
                                            { "id": "K_9" },
                                            { "id": "K_0" },
                                            { "id": "K_HYPHEN" },
                                            { "id": "K_EQUAL" },
                                            { "sp": "10", "width": "1" }
                                        ]
                                    },
                                    {
                                        "id": "1",
                                        "key": [
                                            { "id": "K_Q", "pad": "25" },
                                            { "id": "K_W" },
                                            { "id": "K_E" },
                                            { "id": "K_R" },
                                            { "id": "K_T" },
                                            { "id": "K_Y" },
                                            { "id": "K_U" },
                                            { "id": "K_I" },
                                            { "id": "K_O" },
                                            { "id": "K_P" },
                                            { "id": "K_LBRKT" },
                                            { "id": "K_RBRKT" },
                                            { "sp": "10", "width": "1" }
                                        ]
                                    },
                                    {
                                        "id": "2",
                                        "key": [
                                            { "id": "K_A", "pad": "50" },
                                            { "id": "K_S" },
                                            { "id": "K_D" },
                                            { "id": "K_F" },
                                            { "id": "K_G" },
                                            { "id": "K_H" },
                                            { "id": "K_J" },
                                            { "id": "K_K" },
                                            { "id": "K_L" },
                                            { "id": "K_COLON" },
                                            { "id": "K_QUOTE" },
                                            { "id": "K_BKSLASH", "width": "90" }
                                        ]
                                    },
                                    {
                                        "id": "3",
                                        "key": [
                                            { "id": "K_oE2", "width": "90" },
                                            { "id": "K_Z" },
                                            { "id": "K_X" },
                                            { "id": "K_C" },
                                            { "id": "K_V" },
                                            { "id": "K_B" },
                                            { "id": "K_N" },
                                            { "id": "K_M" },
                                            { "id": "K_COMMA" },
                                            { "id": "K_PERIOD" },
                                            { "id": "K_SLASH" },
                                            { "id": "K_BKQUOTE" },
                                            { "sp": "10", "width": "1" }
                                        ]
                                    },
                                    {
                                        "id": "4",
                                        "key": [
                                            {
                                                "id": "K_SHIFT", "text": "*Shift*", "sp": "1", "width": "200", "sk": [
                                                    { "id": "K_LCONTROL", "text": "*Ctrl*", "sp": "1", "width": "50", "nextlayer": "ctrl" },
                                                    { "id": "K_LCONTROL", "text": "*LCtrl*", "sp": "1", "width": "50", "nextlayer": "leftctrl" },
                                                    { "id": "K_RCONTROL", "text": "*RCtrl*", "sp": "1", "width": "50", "nextlayer": "rightctrl" },
                                                    { "id": "K_LALT", "text": "*Alt*", "sp": "1", "width": "50", "nextlayer": "alt" },
                                                    { "id": "K_LALT", "text": "*LAlt*", "sp": "1", "width": "50", "nextlayer": "leftalt" },
                                                    { "id": "K_RALT", "text": "*RAlt*", "sp": "1", "width": "50", "nextlayer": "rightalt" },
                                                    { "id": "K_ALTGR", "text": "*AltGr*", "sp": "1", "width": "50", "nextlayer": "ctrl-alt" }
                                                ]
                                            },
                                            { "id": "K_LOPT", "text": "*Menu*", "sp": "1", "width": "150" },
                                            { "id": "K_SPACE", "text": "", "width": "570" },
                                            { "id": "K_BKSP", "text": "*BkSp*", "sp": "1", "width": "150" },
                                            { "id": "K_ENTER", "text": "*Enter*", "sp": "1", "width": "200" }
                                        ]
                                    }
                                ]
                            }
                        ]
                    },
                    "phone": {
                        "font": "Tahoma,Helvetica",
                        "layer": [
                            {
                                "id": "default",
                                "row": [
                                    {
                                        "id": "0",
                                        "key": [
                                            { "id": "K_1" },
                                            { "id": "K_2" },
                                            { "id": "K_3" },
                                            { "id": "K_4" },
                                            { "id": "K_5" },
                                            { "id": "K_6" },
                                            { "id": "K_7" },
                                            { "id": "K_8" },
                                            { "id": "K_9" },
                                            { "id": "K_0" },
                                            { "id": "K_HYPHEN" },
                                            { "id": "K_EQUAL" },
                                            { "sp": "10", "width": "1" }
                                        ]
                                    },
                                    {
                                        "id": "1",
                                        "key": [
                                            { "id": "K_Q", "pad": "25" },
                                            { "id": "K_W" },
                                            { "id": "K_E" },
                                            { "id": "K_R" },
                                            { "id": "K_T" },
                                            { "id": "K_Y" },
                                            { "id": "K_U" },
                                            { "id": "K_I" },
                                            { "id": "K_O" },
                                            { "id": "K_P" },
                                            { "id": "K_LBRKT" },
                                            { "id": "K_RBRKT" },
                                            { "sp": "10", "width": "1" }
                                        ]
                                    },
                                    {
                                        "id": "2",
                                        "key": [
                                            { "id": "K_A", "pad": "50" },
                                            { "id": "K_S" },
                                            { "id": "K_D" },
                                            { "id": "K_F" },
                                            { "id": "K_G" },
                                            { "id": "K_H" },
                                            { "id": "K_J" },
                                            { "id": "K_K" },
                                            { "id": "K_L" },
                                            { "id": "K_COLON" },
                                            { "id": "K_QUOTE" },
                                            { "id": "K_BKSLASH", "width": "90" }
                                        ]
                                    },
                                    {
                                        "id": "3",
                                        "key": [
                                            { "id": "K_oE2", "width": "90" },
                                            { "id": "K_Z" },
                                            { "id": "K_X" },
                                            { "id": "K_C" },
                                            { "id": "K_V" },
                                            { "id": "K_B" },
                                            { "id": "K_N" },
                                            { "id": "K_M" },
                                            { "id": "K_COMMA" },
                                            { "id": "K_PERIOD" },
                                            { "id": "K_SLASH" },
                                            { "id": "K_BKQUOTE" },
                                            { "sp": "10", "width": "1" }
                                        ]
                                    },
                                    {
                                        "id": "4",
                                        "key": [
                                            {
                                                "id": "K_SHIFT", "text": "*Shift*", "sp": "1", "width": "200", "sk": [
                                                    { "id": "K_LCONTROL", "text": "*Ctrl*", "sp": "1", "width": "50", "nextlayer": "ctrl" },
                                                    { "id": "K_LCONTROL", "text": "*LCtrl*", "sp": "1", "width": "50", "nextlayer": "leftctrl" },
                                                    { "id": "K_RCONTROL", "text": "*RCtrl*", "sp": "1", "width": "50", "nextlayer": "rightctrl" },
                                                    { "id": "K_LALT", "text": "*Alt*", "sp": "1", "width": "50", "nextlayer": "alt" },
                                                    { "id": "K_LALT", "text": "*LAlt*", "sp": "1", "width": "50", "nextlayer": "leftalt" },
                                                    { "id": "K_RALT", "text": "*RAlt*", "sp": "1", "width": "50", "nextlayer": "rightalt" },
                                                    { "id": "K_ALTGR", "text": "*AltGr*", "sp": "1", "width": "50", "nextlayer": "ctrl-alt" }
                                                ]
                                            },
                                            { "id": "K_LOPT", "text": "*Menu*", "width": "150", "sp": "1" },
                                            { "id": "K_SPACE", "width": "570", "text": "" },
                                            { "id": "K_BKSP", "text": "*BkSp*", "width": "150", "sp": "1" },
                                            { "id": "K_ENTER", "text": "*Enter*", "width": "200", "sp": "1" }
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                };
                return Layouts;
            }());
            keyboards.Layouts = Layouts;
        })(keyboards = keyman.keyboards || (keyman.keyboards = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var keyboards;
        (function (keyboards) {
            var ActiveKey = /** @class */ (function () {
                function ActiveKey() {
                    this.isMnemonic = false;
                }
                ActiveKey.polyfill = function (key, layout, displayLayer) {
                    // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
                    var dummy = new ActiveKey();
                    for (var prop in dummy) {
                        if (!key.hasOwnProperty(prop)) {
                            key[prop] = dummy[prop];
                        }
                    }
                    // Ensure subkeys are also properly extended.
                    if (key.sk) {
                        for (var _i = 0, _a = key.sk; _i < _a.length; _i++) {
                            var subkey = _a[_i];
                            ActiveKey.polyfill(subkey, layout, displayLayer);
                        }
                    }
                    var aKey = key;
                    aKey.displayLayer = displayLayer;
                    aKey.layer = aKey.layer || displayLayer;
                    // Compute the key's base KeyEvent properties for use in future event generation
                    aKey.constructBaseKeyEvent(layout, displayLayer);
                };
                ActiveKey.prototype.constructBaseKeyEvent = function (layout, displayLayer) {
                    // Get key name and keyboard shift state (needed only for default layouts and physical keyboard handling)
                    // Note - virtual keys should be treated case-insensitive, so we force uppercasing here.
                    var layer = this.layer || displayLayer || '';
                    var keyName = this.id ? this.id.toUpperCase() : null;
                    // Start:  mirrors _GetKeyEventProperties
                    // Override key shift state if specified for key in layout (corrected for popup keys KMEW-93)
                    var keyShiftState = keyman.text.KeyboardProcessor.getModifierState(layer);
                    // First check the virtual key, and process shift, control, alt or function keys
                    var Lkc = {
                        Ltarg: null,
                        Lmodifiers: keyShiftState,
                        Lstates: 0,
                        Lcode: keyName ? keyman.text.Codes.keyCodes[keyName] : 0,
                        LisVirtualKey: true,
                        vkCode: 0,
                        kName: keyName,
                        kLayer: layer,
                        kbdLayer: displayLayer,
                        kNextLayer: this.nextlayer,
                        device: null,
                        isSynthetic: true
                    };
                    if (layout.keyboard) {
                        var keyboard = layout.keyboard;
                        // Include *limited* support for mnemonic keyboards (Sept 2012)
                        // If a touch layout has been defined for a mnemonic keyout, do not perform mnemonic mapping for rules on touch devices.
                        if (keyboard.isMnemonic && !(layout.isDefault && layout.formFactor != 'desktop')) {
                            if (Lkc.Lcode != keyman.text.Codes.keyCodes['K_SPACE']) { // exception required, March 2013
                                // Jan 2019 - interesting that 'K_SPACE' also affects the caps-state check...
                                Lkc.vkCode = Lkc.Lcode;
                                this.isMnemonic = true;
                            }
                        }
                        else {
                            Lkc.vkCode = Lkc.Lcode;
                        }
                        // Support version 1.0 KeymanWeb keyboards that do not define positional vs mnemonic
                        if (!keyboard.definesPositionalOrMnemonic) {
                            // Not the best pattern, but currently safe - we don't look up any properties of any of the
                            // arguments in this use case, and the object's scope is extremely limited.
                            Lkc.Lcode = keyman.KeyMapping._USKeyCodeToCharCode(this.constructKeyEvent(null, null, null));
                            Lkc.LisVirtualKey = false;
                        }
                    }
                    this.baseKeyEvent = Lkc;
                };
                ActiveKey.prototype.constructKeyEvent = function (keyboardProcessor, target, device) {
                    // Make a deep copy of our preconstructed key event, filling it out from there.
                    var Lkc = keyman.utils.deepCopy(this.baseKeyEvent);
                    Lkc.Ltarg = target;
                    Lkc.device = device;
                    if (this.isMnemonic) {
                        keyman.text.KeyboardProcessor.setMnemonicCode(Lkc, this.layer.indexOf('shift') != -1, keyboardProcessor ? keyboardProcessor.stateKeys['K_CAPS'] : false);
                    }
                    // Performs common pre-analysis for both 'native' and 'embedded' OSK key & subkey input events.
                    // This part depends on the keyboard processor's active state.
                    if (keyboardProcessor) {
                        keyboardProcessor.setSyntheticEventDefaults(Lkc);
                    }
                    return Lkc;
                };
                ActiveKey.DEFAULT_PAD = 15; // Padding to left of key, in virtual units
                ActiveKey.DEFAULT_RIGHT_MARGIN = 15; // Padding to right of right-most key, in virtual units
                ActiveKey.DEFAULT_KEY_WIDTH = 100; // Width of a key, if not specified, in virtual units
                // Defines key defaults
                ActiveKey.DEFAULT_KEY = {
                    text: '',
                    width: ActiveKey.DEFAULT_KEY_WIDTH.toString(),
                    sp: '0',
                    pad: ActiveKey.DEFAULT_PAD.toString()
                };
                return ActiveKey;
            }());
            keyboards.ActiveKey = ActiveKey;
            var ActiveRow = /** @class */ (function () {
                function ActiveRow() {
                }
                ActiveRow.polyfill = function (row, layout, displayLayer, totalWidth, proportionalY) {
                    // Apply defaults, setting the width and other undefined properties for each key
                    var keys = row['key'];
                    for (var j = 0; j < keys.length; j++) {
                        var key = keys[j];
                        for (var tp in ActiveKey.DEFAULT_KEY) {
                            if (typeof key[tp] != 'string') {
                                key[tp] = ActiveKey.DEFAULT_KEY[tp];
                            }
                        }
                        // Modify the key type for special keys with non-standard labels
                        // to allow the keyboard font to ovveride the SpecialOSK font.
                        // Blank keys are no longer reclassed - can use before/after CSS to add text
                        switch (key['sp']) {
                            case '1':
                                if (!ActiveRow.SPECIAL_LABEL.test(key['text']) && key['text'] != '') {
                                    key['sp'] = '3';
                                }
                                break;
                            case '2':
                                if (!ActiveRow.SPECIAL_LABEL.test(key['text']) && key['text'] != '') {
                                    key['sp'] = '4';
                                }
                                break;
                        }
                        ActiveKey.polyfill(key, layout, displayLayer);
                    }
                    /* The calculations here are effectively 'virtualized'.  When used with the OSK, the VisualKeyboard
                     * will overwrite these values with their true runtime geometry.
                     *
                     * These calculations approximate those of the actual OSK (without fitting to a specific resolution)
                     * and are intended for use with layout testing (while headless) in the future.
                     */
                    // Calculate percentage-based scalings by summing defined widths and scaling each key to %.
                    // Save each percentage key width as a separate member (do *not* overwrite layout specified width!)
                    var keyPercent, padPercent, totalPercent = 0;
                    for (var j = 0; j < keys.length - 1; j++) {
                        keyPercent = parseInt(keys[j]['width'], 10) / totalWidth;
                        keys[j]['widthpc'] = keyPercent;
                        padPercent = parseInt(keys[j]['pad'], 10) / totalWidth;
                        keys[j]['padpc'] = padPercent;
                        // compute center's default x-coord (used in headless modes)
                        keys[j].proportionalX = (totalPercent + padPercent + (keyPercent / 2));
                        keys[j].proportionalWidth = keyPercent;
                        totalPercent += padPercent + keyPercent;
                    }
                    // Allow for right OSK margin (15 layout units)
                    var rightMargin = ActiveKey.DEFAULT_RIGHT_MARGIN / totalWidth;
                    totalPercent += rightMargin;
                    // If a single key, and padding is negative, add padding to right align the key
                    if (keys.length == 1 && parseInt(keys[0]['pad'], 10) < 0) {
                        keyPercent = parseInt(keys[0]['width'], 10) / totalWidth;
                        keys[0]['widthpc'] = keyPercent;
                        totalPercent += keyPercent;
                        keys[0]['padpc'] = 1 - totalPercent;
                        // compute center's default x-coord (used in headless modes)
                        keys[0].proportionalX = ((totalPercent - rightMargin) - keyPercent / 2);
                        keys[0].proportionalWidth = keyPercent;
                    }
                    else if (keys.length > 0) {
                        var j = keys.length - 1;
                        padPercent = parseInt(keys[j]['pad'], 10) / totalWidth;
                        keys[j]['padpc'] = padPercent;
                        totalPercent += padPercent;
                        keys[j]['widthpc'] = keyPercent = 1 - totalPercent;
                        // compute center's default x-coord (used in headless modes)
                        keys[j].proportionalX = (1 - rightMargin) - keyPercent / 2;
                        keys[j].proportionalWidth = keyPercent;
                    }
                    // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
                    var dummy = new ActiveRow();
                    for (var key in dummy) {
                        if (!row.hasOwnProperty(key)) {
                            row[key] = dummy[key];
                        }
                    }
                    var aRow = row;
                    aRow.proportionalY = proportionalY;
                };
                ActiveRow.prototype.populateKeyMap = function (map) {
                    this.key.forEach(function (key) {
                        if (key.id) {
                            map[key.id] = key;
                        }
                    });
                };
                // Identify key labels (e.g. *Shift*) that require the special OSK font
                ActiveRow.SPECIAL_LABEL = /\*\w+\*/;
                return ActiveRow;
            }());
            var ActiveLayer = /** @class */ (function () {
                function ActiveLayer() {
                }
                ActiveLayer.polyfill = function (layer, layout) {
                    layer.aligned = false;
                    // Create a DIV for each row of the group
                    var rows = layer['row'];
                    // Calculate the maximum row width (in layout units)
                    var totalWidth = 0;
                    for (var i = 0; i < layer['row'].length; i++) {
                        var width = 0;
                        var row = rows[i];
                        var keys = row['key'];
                        for (var j = 0; j < keys.length; j++) {
                            var key = keys[j];
                            // Test for a trailing comma included in spec, added as null object by IE
                            if (key == null) {
                                keys.length = keys.length - 1;
                            }
                            else {
                                var kw, kp;
                                kw = (typeof key['width'] == 'string' && key['width'] != '') ? parseInt(key['width'], 10) : ActiveKey.DEFAULT_KEY_WIDTH;
                                if (isNaN(kw) || kw == 0)
                                    kw = ActiveKey.DEFAULT_KEY_WIDTH;
                                key['width'] = kw.toString();
                                kp = (typeof key['pad'] == 'string' && key['pad'] != '') ? parseInt(key['pad'], 10) : ActiveKey.DEFAULT_PAD;
                                if (isNaN(kp) || kp == 0)
                                    kp = ActiveKey.DEFAULT_PAD; // KMEW-119
                                key['pad'] = kp.toString();
                                width += kw + kp;
                                //if(typeof key['width'] == 'string' && key['width'] != '') width += parseInt(key['width'],10); else width += DEFAULT_KEY_WIDTH;
                                //if(typeof key['pad'] == 'string' && key['pad'] != '') width += parseInt(key['pad'],10); else width += 5;
                            }
                        }
                        if (width > totalWidth) {
                            totalWidth = width;
                        }
                    }
                    // Add default right margin
                    if (layout.formFactor == 'desktop') {
                        totalWidth += 5; // TODO: resolve difference between touch and desktop; why don't we use ActiveKey.DEFAULT_RIGHT_MARGIN?
                    }
                    else {
                        totalWidth += ActiveKey.DEFAULT_RIGHT_MARGIN;
                    }
                    var rowCount = layer.row.length;
                    for (var i = 0; i < rowCount; i++) {
                        // Calculate proportional y-coord of row.  0 is at top with highest y-coord.
                        var rowProportionalY = (i + 0.5) / rowCount;
                        ActiveRow.polyfill(layer.row[i], layout, layer.id, totalWidth, rowProportionalY);
                    }
                    // Add class functions and properties to the existing layout object, allowing it to act as an ActiveLayout.
                    var dummy = new ActiveLayer();
                    for (var key in dummy) {
                        if (!layer.hasOwnProperty(key)) {
                            layer[key] = dummy[key];
                        }
                    }
                    var aLayer = layer;
                    aLayer.totalWidth = totalWidth;
                    aLayer.defaultKeyProportionalWidth = parseInt(ActiveKey.DEFAULT_KEY.width, 10) / totalWidth;
                    aLayer.rowProportionalHeight = 1.0 / rowCount;
                    aLayer.keyMap = aLayer.constructKeyMap();
                };
                ActiveLayer.prototype.constructKeyMap = function () {
                    var map = {};
                    this.row.forEach(function (row) {
                        row.populateKeyMap(map);
                    });
                    return map;
                };
                /**
                 * Builds a sorted-order array of most likely keys to be intended for a given touch.
                 * @param touchCoords A proportional (x, y) coordinate of the touch within the keyboard's geometry.
                 *                           Should be within [0, 0] to [1, 1].
                 * @param kbdScaleRatio The ratio of the keyboard's horizontal scale to its vertical scale.
                 *                           For a 400 x 200 keyboard, should be 2.
                 */
                ActiveLayer.prototype.getTouchProbabilities = function (touchCoords, kbdScaleRatio) {
                    var distribution = this.simpleTouchDistribution(touchCoords, kbdScaleRatio);
                    var list = [];
                    for (var key in distribution) {
                        list.push({ keyId: key, p: distribution[key] });
                    }
                    return list.sort(function (a, b) {
                        return b.p - a.p; // Largest probability keys should be listed first.
                    });
                };
                /**
                 * Computes a probability distribution regarding the likelihood of a touch command being intended
                 * for each of the layout's keys.
                 * @param touchCoords A proportional (x, y) coordinate of the touch within the keyboard's geometry.
                 *                           Should be within [0, 0] to [1, 1].
                 * @param kbdScaleRatio The ratio of the keyboard's horizontal scale to its vertical scale.
                 *                           For a 400 x 200 keyboard, should be 2.
                 */
                ActiveLayer.prototype.simpleTouchDistribution = function (touchCoords, kbdScaleRatio) {
                    var keyDists = this.keyTouchDistances(touchCoords, kbdScaleRatio);
                    var keyProbs = {};
                    var totalMass = 0;
                    // Should we wish to allow multiple different transforms for distance -> probability, use a function parameter in place
                    // of the formula in the loop below.
                    for (var key in keyDists) {
                        totalMass += keyProbs[key] = 1 / (keyDists[key] + 1e-6); // Prevent div-by-0 errors.
                    }
                    for (var key in keyProbs) {
                        keyProbs[key] /= totalMass;
                    }
                    return keyProbs;
                };
                /**
                 * Computes a squared 'pseudo-distance' for the touch from each key.  (Not a proper metric.)
                 * Intended for use in generating a probability distribution over the keys based on the touch input.
                 * @param touchCoords A proportional (x, y) coordinate of the touch within the keyboard's geometry.
                 *                           Should be within [0, 0] to [1, 1].
                 * @param kbdScaleRatio The ratio of the keyboard's horizontal scale to its vertical scale.
                 *                           For a 400 x 200 keyboard, should be 2.
                 */
                ActiveLayer.prototype.keyTouchDistances = function (touchCoords, kbdScaleRatio) {
                    var layer = this;
                    var keyDists = {};
                    // This double-nested loop computes a pseudo-distance for the touch from each key.  Quite useful for
                    // generating a probability distribution.
                    this.row.forEach(function (row) {
                        row.key.forEach(function (key) {
                            // If the key lacks an ID, just skip it.  Sometimes used for padding.
                            if (!key.id) {
                                return;
                            }
                            // These represent the within-key distance of the touch from the key's center.
                            // Both should be on the interval [0, 0.5].
                            var dx = Math.abs(touchCoords.x - key.proportionalX);
                            var dy = Math.abs(touchCoords.y - row.proportionalY);
                            // If the touch isn't within the key, these store the out-of-key distance
                            // from the closest point on the key being checked.
                            var distX, distY;
                            if (dx > 0.5 * key.proportionalWidth) {
                                distX = (dx - 0.5 * key.proportionalWidth);
                                dx = 0.5;
                            }
                            else {
                                distX = 0;
                                dx /= key.proportionalWidth;
                            }
                            if (dy > 0.5 * layer.rowProportionalHeight) {
                                distY = (dy - 0.5 * layer.rowProportionalHeight);
                                dy = 0.5;
                            }
                            else {
                                distY = 0;
                                dy /= layer.rowProportionalHeight;
                            }
                            // Now that the differentials are computed, it's time to do distance scaling.
                            //
                            // For out-of-key distance, we scale the X component by the keyboard's aspect ratio
                            // to get the actual out-of-key distance rather than proportional.
                            distX *= kbdScaleRatio;
                            // While the keys are rarely perfect squares, we map all within-key distance
                            // to a square shape.  (ALT/CMD should seem as close to SPACE as a 'B'.)
                            //
                            // For that square, we take the rowHeight as its edge lengths.
                            distX += dx * layer.rowProportionalHeight;
                            distY += dy * layer.rowProportionalHeight;
                            var distance = distX * distX + distY * distY;
                            keyDists[key.id] = distance;
                        });
                    });
                    return keyDists;
                };
                ActiveLayer.prototype.getKey = function (keyId) {
                    // Keys usually are specified in a "long form" prefixed with their layer's ID.
                    if (keyId.indexOf(this.id + '-') == 0) {
                        keyId = keyId.replace(this.id + '-', '');
                    }
                    return this.keyMap[keyId];
                };
                return ActiveLayer;
            }());
            keyboards.ActiveLayer = ActiveLayer;
            var ActiveLayout = /** @class */ (function () {
                function ActiveLayout() {
                }
                ActiveLayout.prototype.getLayer = function (layerId) {
                    return this.layerMap[layerId];
                };
                /**
                 *
                 * @param layout
                 * @param formFactor
                 */
                ActiveLayout.polyfill = function (layout, keyboard, formFactor) {
                    if (layout == null) {
                        throw new Error("Cannot build an ActiveLayout for a null specification.");
                    }
                    // Create a separate OSK div for each OSK layer, only one of which will ever be visible
                    var n, i;
                    var layers, layer;
                    var layerMap = {};
                    var rows;
                    layers = layout['layer'];
                    // ***Delete any empty rows at the end added by compiler bug...
                    for (n = 0; n < layers.length; n++) {
                        layer = layers[n];
                        rows = layer['row'];
                        for (i = rows.length; i > 0; i--) {
                            if (rows[i - 1]['key'].length > 0) {
                                break;
                            }
                        }
                        if (i < rows.length) {
                            rows.splice(i - rows.length, rows.length - i);
                        }
                    }
                    // ...remove to here when compiler bug fixed ***
                    // Add class functions to the existing layout object, allowing it to act as an ActiveLayout.
                    var dummy = new ActiveLayout();
                    for (var key in dummy) {
                        if (!layout.hasOwnProperty(key)) {
                            layout[key] = dummy[key];
                        }
                    }
                    var aLayout = layout;
                    aLayout.keyboard = keyboard;
                    aLayout.formFactor = formFactor;
                    for (n = 0; n < layers.length; n++) {
                        ActiveLayer.polyfill(layers[n], aLayout);
                        layerMap[layers[n].id] = layers[n];
                    }
                    aLayout.layerMap = layerMap;
                    return aLayout;
                };
                return ActiveLayout;
            }());
            keyboards.ActiveLayout = ActiveLayout;
        })(keyboards = keyman.keyboards || (keyman.keyboards = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
/// <reference path="engineDeviceSpec.ts" />
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            /**
             * Defines common behaviors associated with system stores.
             */
            var SystemStore = /** @class */ (function () {
                function SystemStore(id) {
                    this.id = id;
                }
                SystemStore.prototype.set = function (value) {
                    throw new Error("System store with ID " + this.id + " may not be directly set.");
                };
                return SystemStore;
            }());
            text.SystemStore = SystemStore;
            var MutableSystemStore = /** @class */ (function (_super) {
                __extends(MutableSystemStore, _super);
                function MutableSystemStore(id, defaultValue) {
                    var _this = _super.call(this, id) || this;
                    _this.handler = null;
                    _this._value = defaultValue;
                    return _this;
                }
                Object.defineProperty(MutableSystemStore.prototype, "value", {
                    get: function () {
                        return this._value;
                    },
                    enumerable: true,
                    configurable: true
                });
                MutableSystemStore.prototype.matches = function (value) {
                    return this._value == value;
                };
                MutableSystemStore.prototype.set = function (value) {
                    if (this.handler) {
                        if (this.handler(this, value)) {
                            return;
                        }
                    }
                    this._value = value;
                };
                return MutableSystemStore;
            }(SystemStore));
            text.MutableSystemStore = MutableSystemStore;
            /**
             * Handles checks against the current platform.
             */
            var PlatformSystemStore = /** @class */ (function (_super) {
                __extends(PlatformSystemStore, _super);
                function PlatformSystemStore(keyboardInterface) {
                    var _this = _super.call(this, text.KeyboardInterface.TSS_PLATFORM) || this;
                    _this.kbdInterface = keyboardInterface;
                    return _this;
                }
                PlatformSystemStore.prototype.matches = function (value) {
                    var i, constraint, constraints = value.split(' ');
                    var device = this.kbdInterface.activeDevice;
                    for (i = 0; i < constraints.length; i++) {
                        constraint = constraints[i].toLowerCase();
                        switch (constraint) {
                            case 'touch':
                            case 'hardware':
                                if (device.touchable != (constraint == 'touch')) {
                                    return false;
                                }
                                break;
                            case 'macos':
                            case 'mac':
                                constraint = 'macosx';
                            // fall through
                            case 'macosx':
                            case 'windows':
                            case 'android':
                            case 'ios':
                            case 'linux':
                                if (device.OS != constraint) {
                                    return false;
                                }
                                break;
                            case 'tablet':
                            case 'phone':
                            case 'desktop':
                                if (device.formFactor != constraint) {
                                    return false;
                                }
                                break;
                            case 'web':
                                if (device.browser == 'native') {
                                    return false; // web matches anything other than 'native'
                                }
                                break;
                            case 'native':
                            // This will return true for embedded KeymanWeb
                            case 'ie':
                            case 'chrome':
                            case 'firefox':
                            case 'safari':
                            case 'edge':
                            case 'opera':
                                if (device.browser != constraint) {
                                    return false;
                                }
                                break;
                            default:
                                return false;
                        }
                    }
                    // Everything we checked against was valid and had matches - it's a match!
                    return true;
                };
                return PlatformSystemStore;
            }(SystemStore));
            text.PlatformSystemStore = PlatformSystemStore;
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
/// <reference path="deadkeys.ts" />
/// <reference path="ruleBehavior.ts" />
// Defines classes for handling system stores
/// <reference path="systemStores.ts" />
/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            //#region Helper type definitions
            var KeyInformation = /** @class */ (function () {
                function KeyInformation() {
                }
                return KeyInformation;
            }());
            text.KeyInformation = KeyInformation;
            var RuleDeadkey = /** @class */ (function () {
                function RuleDeadkey() {
                }
                return RuleDeadkey;
            }());
            var ContextAny = /** @class */ (function () {
                function ContextAny() {
                }
                return ContextAny;
            }());
            var RuleIndex = /** @class */ (function () {
                function RuleIndex() {
                }
                return RuleIndex;
            }());
            var ContextEx = /** @class */ (function () {
                function ContextEx() {
                }
                return ContextEx;
            }());
            var ContextNul = /** @class */ (function () {
                function ContextNul() {
                }
                return ContextNul;
            }());
            var StoreBeep = /** @class */ (function () {
                function StoreBeep() {
                }
                return StoreBeep;
            }());
            /**
             * Cache of context storing and retrieving return values from KC
             * Must be reset prior to each keystroke and after any text changes
             * MCD 3/1/14
             **/
            var CachedContext = /** @class */ (function () {
                function CachedContext() {
                }
                CachedContext.prototype.reset = function () {
                    this._cache = [];
                };
                CachedContext.prototype.get = function (n, ln) {
                    // return null; // uncomment this line to disable context caching
                    if (typeof this._cache[n] == 'undefined') {
                        return null;
                    }
                    else if (typeof this._cache[n][ln] == 'undefined') {
                        return null;
                    }
                    return this._cache[n][ln];
                };
                CachedContext.prototype.set = function (n, ln, val) {
                    if (typeof this._cache[n] == 'undefined') {
                        this._cache[n] = [];
                    }
                    this._cache[n][ln] = val;
                };
                return CachedContext;
            }());
            ;
            /**
             * An extended version of cached context storing designed to work with
             * `fullContextMatch` and its helper functions.
             */
            var CachedContextEx = /** @class */ (function () {
                function CachedContextEx() {
                }
                CachedContextEx.prototype.reset = function () {
                    this._cache = [];
                };
                CachedContextEx.prototype.get = function (n, ln) {
                    // return null; // uncomment this line to disable context caching
                    if (typeof this._cache[n] == 'undefined') {
                        return null;
                    }
                    else if (typeof this._cache[n][ln] == 'undefined') {
                        return null;
                    }
                    return this._cache[n][ln];
                };
                CachedContextEx.prototype.set = function (n, ln, val) {
                    if (typeof this._cache[n] == 'undefined') {
                        this._cache[n] = [];
                    }
                    this._cache[n][ln] = val;
                };
                return CachedContextEx;
            }());
            ;
            //#endregion
            var KeyboardInterface = /** @class */ (function () {
                function KeyboardInterface(variableStoreSerializer) {
                    if (variableStoreSerializer === void 0) { variableStoreSerializer = null; }
                    this.cachedContext = new CachedContext();
                    this.cachedContextEx = new CachedContextEx();
                    this._AnyIndices = []; // AnyIndex - array of any/index match indices
                    this.systemStores = {};
                    this.systemStores[KeyboardInterface.TSS_PLATFORM] = new text.PlatformSystemStore(this);
                    this.systemStores[KeyboardInterface.TSS_LAYER] = new text.MutableSystemStore(KeyboardInterface.TSS_LAYER, 'default');
                    this.variableStoreSerializer = variableStoreSerializer;
                }
                /**
                 * Function     KSF
                 * Scope        Public
                 *
                 * Saves the document's current focus settings on behalf of the keyboard.  Often paired with insertText.
                 */
                KeyboardInterface.prototype.saveFocus = function () { };
                /**
                 * Function     registerKeyboard  KR
                 * Scope        Public
                 * @param       {Object}      Pk      Keyboard  object
                 * Description  Registers a keyboard with KeymanWeb once its script has fully loaded.
                 *
                 *              In web-core, this also activates the keyboard; in other modules, this method
                 *              may be replaced with other implementations.
                 */
                KeyboardInterface.prototype.registerKeyboard = function (Pk) {
                    // NOTE:  This implementation is web-core specific and is intentionally replaced, whole-sale, 
                    //        by DOM-aware code.
                    var keyboard = new keyman.keyboards.Keyboard(Pk);
                    this.activeKeyboard = keyboard;
                };
                /**
                 * Get *cached or uncached* keyboard context for a specified range, relative to caret
                 *
                 * @param       {number}      n       Number of characters to move back from caret
                 * @param       {number}      ln      Number of characters to return
                 * @param       {Object}      Pelem   Element to work with (must be currently focused element)
                 * @return      {string}              Context string
                 *
                 * Example     [abcdef|ghi] as INPUT, with the caret position marked by |:
                 *             KC(2,1,Pelem) == "e"
                 *             KC(3,3,Pelem) == "def"
                 *             KC(10,10,Pelem) == "abcdef"  i.e. return as much as possible of the requested string
                 */
                KeyboardInterface.prototype.context = function (n, ln, outputTarget) {
                    var v = this.cachedContext.get(n, ln);
                    if (v !== null) {
                        return v;
                    }
                    var r = this.KC_(n, ln, outputTarget);
                    this.cachedContext.set(n, ln, r);
                    return r;
                };
                /**
                 * Get (uncached) keyboard context for a specified range, relative to caret
                 *
                 * @param       {number}      n       Number of characters to move back from caret
                 * @param       {number}      ln      Number of characters to return
                 * @param       {Object}      Pelem   Element to work with (must be currently focused element)
                 * @return      {string}              Context string
                 *
                 * Example     [abcdef|ghi] as INPUT, with the caret position marked by |:
                 *             KC(2,1,Pelem) == "e"
                 *             KC(3,3,Pelem) == "def"
                 *             KC(10,10,Pelem) == "XXXXabcdef"  i.e. return as much as possible of the requested string, where X = \uFFFE
                 */
                KeyboardInterface.prototype.KC_ = function (n, ln, outputTarget) {
                    var tempContext = '';
                    tempContext = outputTarget.getTextBeforeCaret();
                    if (tempContext._kmwLength() < n) {
                        tempContext = Array(n - tempContext._kmwLength() + 1).join("\uFFFE") + tempContext;
                    }
                    return tempContext._kmwSubstr(-n)._kmwSubstr(0, ln);
                };
                /**
                 * Function     nul           KN
                 * Scope        Public
                 * @param       {number}      n       Length of context to check
                 * @param       {Object}      Ptarg   Element to work with (must be currently focused element)
                 * @return      {boolean}             True if length of context is less than or equal to n
                 * Description  Test length of context, return true if the length of the context is less than or equal to n
                 *
                 * Example     [abc|def] as INPUT, with the caret position marked by |:
                 *             KN(3,Pelem) == TRUE
                 *             KN(2,Pelem) == FALSE
                 *             KN(4,Pelem) == TRUE
                 */
                KeyboardInterface.prototype.nul = function (n, outputTarget) {
                    var cx = this.context(n + 1, 1, outputTarget);
                    // With #31, the result will be a replacement character if context is empty.
                    return cx === "\uFFFE";
                };
                /**
                 * Function     contextMatch  KCM
                 * Scope        Public
                 * @param       {number}      n       Number of characters to move back from caret
                 * @param       {Object}      Ptarg   Focused element
                 * @param       {string}      val     String to match
                 * @param       {number}      ln      Number of characters to return
                 * @return      {boolean}             True if selected context matches val
                 * Description  Test keyboard context for match
                 */
                KeyboardInterface.prototype.contextMatch = function (n, outputTarget, val, ln) {
                    var cx = this.context(n, ln, outputTarget);
                    if (cx === val) {
                        return true; // I3318
                    }
                    outputTarget.deadkeys().resetMatched(); // I3318
                    return false;
                };
                /**
                 * Builds the *cached or uncached* keyboard context for a specified range, relative to caret
                 *
                 * @param       {number}      n       Number of characters to move back from caret
                 * @param       {number}      ln      Number of characters to return
                 * @param       {Object}      Pelem   Element to work with (must be currently focused element)
                 * @return      {Array}               Context array (of strings and numbers)
                 */
                KeyboardInterface.prototype._BuildExtendedContext = function (n, ln, outputTarget) {
                    var cache = this.cachedContextEx.get(n, ln);
                    if (cache !== null) {
                        return cache;
                    }
                    else {
                        // By far the easiest way to correctly build what we want is to start from the right and work to what we need.
                        // We may have done it for a similar cursor position before.
                        cache = this.cachedContextEx.get(n, n);
                        if (cache === null) {
                            // First, let's make sure we have a cloned, sorted copy of the deadkey array.
                            var unmatchedDeadkeys = outputTarget.deadkeys().toSortedArray(); // Is reverse-order sorted for us already.
                            // Time to build from scratch!
                            var index = 0;
                            cache = { valContext: [], deadContext: [] };
                            while (cache.valContext.length < n) {
                                // As adapted from `deadkeyMatch`.
                                var sp = outputTarget.getDeadkeyCaret();
                                var deadPos = sp - index;
                                if (unmatchedDeadkeys.length > 0 && unmatchedDeadkeys[0].p > deadPos) {
                                    // We have deadkeys at the right-hand side of the caret!  They don't belong in the context, so pop 'em off.
                                    unmatchedDeadkeys.splice(0, 1);
                                    continue;
                                }
                                else if (unmatchedDeadkeys.length > 0 && unmatchedDeadkeys[0].p == deadPos) {
                                    // Take the deadkey.
                                    cache.deadContext[n - cache.valContext.length - 1] = unmatchedDeadkeys[0];
                                    cache.valContext = [unmatchedDeadkeys[0].d].concat(cache.valContext);
                                    unmatchedDeadkeys.splice(0, 1);
                                }
                                else {
                                    // Take the character.  We get "\ufffe" if it doesn't exist.
                                    var kc = this.context(++index, 1, outputTarget);
                                    cache.valContext = [kc].concat(cache.valContext);
                                }
                            }
                            this.cachedContextEx.set(n, n, cache);
                        }
                        // Now that we have the cache...
                        var subCache = cache;
                        subCache.valContext = subCache.valContext.slice(0, ln);
                        for (var i = 0; i < subCache.valContext.length; i++) {
                            if (subCache[i] == '\ufffe') {
                                subCache.valContext.splice(0, 1);
                                subCache.deadContext.splice(0, 1);
                            }
                        }
                        if (subCache.valContext.length == 0) {
                            subCache.valContext = ['\ufffe'];
                            subCache.deadContext = [];
                        }
                        this.cachedContextEx.set(n, ln, subCache);
                        return subCache;
                    }
                };
                /**
                 * Function       fullContextMatch    KFCM
                 * Scope          Private
                 * @param         {number}    n       Number of characters to move back from caret
                 * @param         {Object}    Ptarg   Focused element
                 * @param         {Array}     rule    An array of ContextEntries to match.
                 * @return        {boolean}           True if the fully-specified rule context matches the current KMW state.
                 *
                 * A KMW 10+ function designed to bring KMW closer to Keyman Desktop functionality,
                 * near-directly modeling (externally) the compiled form of Desktop rules' context section.
                 */
                KeyboardInterface.prototype.fullContextMatch = function (n, outputTarget, rule) {
                    // Stage one:  build the context index map.
                    var fullContext = this._BuildExtendedContext(n, rule.length, outputTarget);
                    var context = fullContext.valContext;
                    var deadContext = fullContext.deadContext;
                    var mismatch = false;
                    // This symbol internally indicates lack of context in a position.  (See KC_)
                    var NUL_CONTEXT = "\uFFFE";
                    var assertNever = function (x) {
                        // Could be accessed by improperly handwritten calls to `fullContextMatch`.
                        throw new Error("Unexpected object in fullContextMatch specification: " + x);
                    };
                    // Stage two:  time to match against the rule specified.
                    for (var i = 0; i < rule.length; i++) {
                        if (typeof rule[i] == 'string') {
                            var str = rule[i];
                            if (str !== context[i]) {
                                mismatch = true;
                                break;
                            }
                        }
                        else {
                            // TypeScript needs a cast to this intermediate type to do its discriminated union magic.
                            var r = rule[i];
                            switch (r.t) {
                                case 'd':
                                    // We still need to set a flag here; 
                                    if (r['d'] !== context[i]) {
                                        mismatch = true;
                                    }
                                    else {
                                        deadContext[i].set();
                                    }
                                    break;
                                case 'a':
                                    var lookup;
                                    if (typeof context[i] == 'string') {
                                        lookup = context[i];
                                    }
                                    else {
                                        lookup = { 't': 'd', 'd': context[i] };
                                    }
                                    var result = this.any(i, lookup, r.a);
                                    if (!r.n) { // If it's a standard 'any'...
                                        if (!result) {
                                            mismatch = true;
                                        }
                                        else if (deadContext[i] !== undefined) {
                                            // It's a deadkey match, so indicate that.
                                            deadContext[i].set();
                                        }
                                        // 'n' for 'notany'.  If we actually match or if we have nul context (\uFFFE), notany fails.
                                    }
                                    else if (r.n && (result || context[i] !== NUL_CONTEXT)) {
                                        mismatch = true;
                                    }
                                    break;
                                case 'i':
                                    // The context will never hold a 'beep.'
                                    var ch = this._Index(r.i, r.o);
                                    if (ch !== undefined && (typeof (ch) == 'string' ? ch : ch.d) !== context[i]) {
                                        mismatch = true;
                                    }
                                    else if (deadContext[i] !== undefined) {
                                        deadContext[i].set();
                                    }
                                    break;
                                case 'c':
                                    if (context[r.c - 1] !== context[i]) {
                                        mismatch = true;
                                    }
                                    else if (deadContext[i] !== undefined) {
                                        deadContext[i].set();
                                    }
                                    break;
                                case 'n':
                                    // \uFFFE is the internal 'no context here sentinel'.
                                    if (context[i] != NUL_CONTEXT) {
                                        mismatch = true;
                                    }
                                    break;
                                default:
                                    assertNever(r);
                            }
                        }
                    }
                    if (mismatch) {
                        // Reset the matched 'any' indices, if any.
                        outputTarget.deadkeys().resetMatched();
                        this._AnyIndices = [];
                    }
                    return !mismatch;
                };
                /**
                 * Function     KIK
                 * Scope        Public
                 * @param       {Object}  e   keystroke event
                 * @return      {boolean}     true if keypress event
                 * Description  Test if event as a keypress event
                 */
                KeyboardInterface.prototype.isKeypress = function (e) {
                    if (this.activeKeyboard.isMnemonic) { // I1380 - support KIK for positional layouts
                        return !e.LisVirtualKey; // will now return true for U_xxxx keys, but not for T_xxxx keys
                    }
                    else {
                        return keyman.KeyMapping._USKeyCodeToCharCode(e) ? true : false; // I1380 - support KIK for positional layouts
                    }
                };
                /**
                 * Function     keyMatch      KKM
                 * Scope        Public
                 * @param       {Object}      e           keystroke event
                 * @param       {number}      Lruleshift
                 * @param       {number}      Lrulekey
                 * @return      {boolean}                 True if key matches rule
                 * Description  Test keystroke with modifiers against rule
                 */
                KeyboardInterface.prototype.keyMatch = function (e, Lruleshift, Lrulekey) {
                    var retVal = false; // I3318
                    var keyCode = (e.Lcode == 173 ? 189 : e.Lcode); //I3555 (Firefox hyphen issue)
                    var bitmask = this.activeKeyboard.modifierBitmask;
                    var Codes = com.keyman.text.Codes;
                    var modifierBitmask = bitmask & Codes.modifierBitmasks["ALL"];
                    var stateBitmask = bitmask & Codes.stateBitmasks["ALL"];
                    if (e.vkCode > 255) {
                        keyCode = e.vkCode; // added to support extended (touch-hold) keys for mnemonic layouts
                    }
                    if (e.LisVirtualKey || keyCode > 255) {
                        if ((Lruleshift & 0x4000) == 0x4000 || (keyCode > 255)) { // added keyCode test to support extended keys
                            retVal = ((Lrulekey == keyCode) && ((Lruleshift & modifierBitmask) == e.Lmodifiers)); //I3318, I3555
                            retVal = retVal && this.stateMatch(e, Lruleshift & stateBitmask);
                        }
                    }
                    else if ((Lruleshift & 0x4000) == 0) {
                        retVal = (keyCode == Lrulekey); // I3318, I3555
                    }
                    if (!retVal) {
                        this.activeTargetOutput.deadkeys().resetMatched(); // I3318
                    }
                    return retVal; // I3318
                };
                ;
                /**
                 * Function     stateMatch    KSM
                 * Scope        Public
                 * @param       {Object}      e       keystroke event
                 * @param       {number}      Lstate
                 * Description  Test keystroke against state key rules
                 */
                KeyboardInterface.prototype.stateMatch = function (e, Lstate) {
                    return ((Lstate & e.Lstates) == Lstate);
                };
                /**
                 * Function     keyInformation  KKI
                 * Scope        Public
                 * @param       {Object}      e
                 * @return      {Object}              Object with event's virtual key flag, key code, and modifiers
                 * Description  Get object with extended key event information
                 */
                KeyboardInterface.prototype.keyInformation = function (e) {
                    var ei = new KeyInformation();
                    ei['vk'] = e.LisVirtualKey;
                    ei['code'] = e.Lcode;
                    ei['modifiers'] = e.Lmodifiers;
                    return ei;
                };
                ;
                /**
                 * Function     deadkeyMatch  KDM
                 * Scope        Public
                 * @param       {number}      n       offset from current cursor position
                 * @param       {Object}      Ptarg   target element
                 * @param       {number}      d       deadkey
                 * @return      {boolean}             True if deadkey found selected context matches val
                 * Description  Match deadkey at current cursor position
                 */
                KeyboardInterface.prototype.deadkeyMatch = function (n, outputTarget, d) {
                    return outputTarget.hasDeadkeyMatch(n, d);
                };
                /**
                 * Function     beep          KB
                 * Scope        Public
                 * @param       {Object}      Pelem     element to flash
                 * Description  Flash body as substitute for audible beep; notify embedded device to vibrate
                 */
                KeyboardInterface.prototype.beep = function (outputTarget) {
                    this.resetContextCache();
                    // Denote as part of the matched rule's behavior.
                    this.ruleBehavior.beep = true;
                };
                KeyboardInterface.prototype._ExplodeStore = function (store) {
                    if (typeof (store) == 'string') {
                        var cachedStores = this.activeKeyboard.explodedStores;
                        // Is the result cached?
                        if (cachedStores[store]) {
                            return cachedStores[store];
                        }
                        // Nope, so let's build its cache.
                        var result = [];
                        for (var i = 0; i < store._kmwLength(); i++) {
                            result.push(store._kmwCharAt(i));
                        }
                        // Cache the result for later!
                        cachedStores[store] = result;
                        return result;
                    }
                    else {
                        return store;
                    }
                };
                /**
                 * Function     any           KA
                 * Scope        Public
                 * @param       {number}      n     character position (index)
                 * @param       {string}      ch    character to find in string
                 * @param       {string}      s     'any' string
                 * @return      {boolean}           True if character found in 'any' string, sets index accordingly
                 * Description  Test for character matching
                 */
                KeyboardInterface.prototype.any = function (n, ch, s) {
                    if (ch == '') {
                        return false;
                    }
                    s = this._ExplodeStore(s);
                    var Lix = -1;
                    for (var i = 0; i < s.length; i++) {
                        if (typeof (s[i]) == 'string') {
                            if (s[i] == ch) {
                                Lix = i;
                                break;
                            }
                        }
                        else if (s[i]['d'] === ch['d']) {
                            Lix = i;
                            break;
                        }
                    }
                    this._AnyIndices[n] = Lix;
                    return Lix >= 0;
                };
                /**
                 * Function     _Index
                 * Scope        Public
                 * @param       {string}      Ps      string
                 * @param       {number}      Pn      index
                 * Description  Returns the character from a store string according to the offset in the index array
                 */
                KeyboardInterface.prototype._Index = function (Ps, Pn) {
                    Ps = this._ExplodeStore(Ps);
                    if (this._AnyIndices[Pn - 1] < Ps.length) { //I3319
                        return Ps[this._AnyIndices[Pn - 1]];
                    }
                    else {
                        /* Should not be possible for a compiled keyboard, but may arise
                        * during the development of handwritten keyboards.
                        */
                        console.warn("Unmatched contextual index() statement detected in rule with index " + Pn + "!");
                        return "";
                    }
                };
                /**
                 * Function     indexOutput   KIO
                 * Scope        Public
                 * @param       {number}      Pdn     no of character to overwrite (delete)
                 * @param       {string}      Ps      string
                 * @param       {number}      Pn      index
                 * @param       {Object}      Pelem   element to output to
                 * Description  Output a character selected from the string according to the offset in the index array
                 */
                KeyboardInterface.prototype.indexOutput = function (Pdn, Ps, Pn, outputTarget) {
                    this.resetContextCache();
                    var assertNever = function (x) {
                        // Could be accessed by improperly handwritten calls to `fullContextMatch`.
                        throw new Error("Unexpected object in fullContextMatch specification: " + x);
                    };
                    var indexChar = this._Index(Ps, Pn);
                    if (indexChar !== "") {
                        if (typeof indexChar == 'string') {
                            this.output(Pdn, outputTarget, indexChar); //I3319
                        }
                        else if (indexChar['t']) {
                            var storeEntry = indexChar;
                            switch (storeEntry.t) {
                                case 'b': // Beep commands may appear within stores.
                                    this.beep(outputTarget);
                                    break;
                                case 'd':
                                    this.deadkeyOutput(Pdn, outputTarget, indexChar['d']);
                                    break;
                                default:
                                    assertNever(storeEntry);
                            }
                        }
                        else { // For keyboards developed during 10.0's alpha phase - t:'d' was assumed.
                            this.deadkeyOutput(Pdn, outputTarget, indexChar['d']);
                        }
                    }
                };
                /**
                 * Function     deleteContext KDC
                 * Scope        Public
                 * @param       {number}      dn      number of context entries to overwrite
                 * @param       {Object}      Pelem   element to output to
                 * @param       {string}      s       string to output
                 * Description  Keyboard output
                 */
                KeyboardInterface.prototype.deleteContext = function (dn, outputTarget) {
                    var context;
                    // We want to control exactly which deadkeys get removed.
                    if (dn > 0) {
                        context = this._BuildExtendedContext(dn, dn, outputTarget);
                        var nulCount = 0;
                        for (var i = 0; i < context.valContext.length; i++) {
                            var dk = context.deadContext[i];
                            if (dk) {
                                // Remove deadkey in context.
                                outputTarget.deadkeys().remove(dk);
                                // Reduce our reported context size.
                                dn--;
                            }
                            else if (context.valContext[i] == "\uFFFE") {
                                // Count any `nul` sentinels that would contribute to our deletion count.
                                nulCount++;
                            }
                        }
                        // Prevent attempts to delete nul sentinels, as they don't exist in the actual context.
                        // (Addresses regression from KMW v 12.0 paired with Developer bug through same version)
                        var contextLength = context.valContext.length - nulCount;
                        if (dn > contextLength) {
                            dn = contextLength;
                        }
                    }
                    // If a matched deadkey hasn't been deleted, we don't WANT to delete it.
                    outputTarget.deadkeys().resetMatched();
                    // Why reinvent the wheel?  Delete the remaining characters by 'inserting a blank string'.
                    this.output(dn, outputTarget, '');
                };
                /**
                 * Function     output        KO
                 * Scope        Public
                 * @param       {number}      dn      number of characters to overwrite
                 * @param       {Object}      Pelem   element to output to
                 * @param       {string}      s       string to output
                 * Description  Keyboard output
                 */
                KeyboardInterface.prototype.output = function (dn, outputTarget, s) {
                    this.resetContextCache();
                    outputTarget.saveProperties();
                    outputTarget.clearSelection();
                    outputTarget.deadkeys().deleteMatched(); // I3318
                    if (dn >= 0) {
                        // Automatically manages affected deadkey positions.  Does not delete deadkeys b/c legacy behavior support.
                        outputTarget.deleteCharsBeforeCaret(dn);
                    }
                    // Automatically manages affected deadkey positions.
                    outputTarget.insertTextBeforeCaret(s);
                    outputTarget.restoreProperties();
                };
                /**
                 * Function     deadkeyOutput KDO
                 * Scope        Public
                 * @param       {number}      Pdn     no of character to overwrite (delete)
                 * @param       {Object}      Pelem   element to output to
                 * @param       {number}      Pd      deadkey id
                 * Description  Record a deadkey at current cursor position, deleting Pdn characters first
                 */
                KeyboardInterface.prototype.deadkeyOutput = function (Pdn, outputTarget, Pd) {
                    this.resetContextCache();
                    if (Pdn >= 0) {
                        this.output(Pdn, outputTarget, ""); //I3318 corrected to >=
                    }
                    outputTarget.insertDeadkeyBeforeCaret(Pd);
                    //    _DebugDeadKeys(Pelem, 'KDeadKeyOutput: dn='+Pdn+'; deadKey='+Pd);
                };
                /**
                 * KIFS compares the content of a system store with a string value
                 *
                 * @param       {number}      systemId    ID of the system store to test (only TSS_LAYER currently supported)
                 * @param       {string}      strValue    String value to compare to
                 * @param       {Object}      Pelem       Currently active element (may be needed by future tests)
                 * @return      {boolean}                 True if the test succeeds
                 */
                KeyboardInterface.prototype.ifStore = function (systemId, strValue, outputTarget) {
                    var result = true;
                    var store = this.systemStores[systemId];
                    if (store) {
                        result = store.matches(strValue);
                    }
                    return result; //Moved from previous line, now supports layer selection, Build 350 
                };
                /**
                 * KSETS sets the value of a system store to a string
                 *
                 * @param       {number}      systemId    ID of the system store to set (only TSS_LAYER currently supported)
                 * @param       {string}      strValue    String to set as the system store content
                 * @param       {Object}      Pelem       Currently active element (may be needed in future tests)
                 * @return      {boolean}                 True if command succeeds
                 *                                        (i.e. for TSS_LAYER, if the layer is successfully selected)
                 *
                 * Note that option/variable stores are instead set within keyboard script code, as they only
                 * affect keyboard behavior.
                 */
                KeyboardInterface.prototype.setStore = function (systemId, strValue, outputTarget) {
                    this.resetContextCache();
                    if (systemId == KeyboardInterface.TSS_LAYER) {
                        // Denote the changed store as part of the matched rule's behavior.
                        this.ruleBehavior.setStore[systemId] = strValue;
                    }
                    else {
                        return false;
                    }
                };
                /**
                 * Load an option store value from a cookie or default value
                 *
                 * @param       {string}      kbdName     keyboard internal name
                 * @param       {string}      storeName   store (option) name, embedded in cookie name
                 * @param       {string}      dfltValue   default value
                 * @return      {string}                  current or default option value
                 *
                 * This will only ever be called when the keyboard is loaded, as it is used by keyboards
                 * to initialize a store value on the keyboard's script object.
                 */
                KeyboardInterface.prototype.loadStore = function (kbdName, storeName, dfltValue) {
                    this.resetContextCache();
                    if (this.variableStoreSerializer) {
                        var cValue = this.variableStoreSerializer.loadStore(kbdName, storeName);
                        return cValue[storeName] || dfltValue;
                    }
                    else {
                        return dfltValue;
                    }
                };
                /**
                 * Save an option store value to a cookie
                 *
                 * @param       {string}      storeName   store (option) name, embedded in cookie name
                 * @param       {string}      optValue    option value to save
                 * @return      {boolean}                 true if save successful
                 *
                 * Note that a keyboard will freely manipulate the value of its variable stores on the
                 * script object within its own code.  This function's use is merely to _persist_ that
                 * value across sessions, providing a custom user default for later uses of the keyboard.
                 */
                KeyboardInterface.prototype.saveStore = function (storeName, optValue) {
                    this.resetContextCache();
                    var kbd = this.activeKeyboard;
                    if (!kbd || typeof kbd.id == 'undefined' || kbd.id == '') {
                        return false;
                    }
                    // And the lookup under that entry looks for the value under the store name, again.
                    var valueObj = {};
                    valueObj[storeName] = optValue;
                    // Null-check in case of invocation during unit-test
                    if (this.ruleBehavior) {
                        this.ruleBehavior.saveStore[storeName] = valueObj;
                    }
                    else {
                        // We're in a unit-test environment, directly invoking this method from outside of a keyboard.
                        // In this case, we should immediately commit the change.
                        this.variableStoreSerializer.saveStore(this.activeKeyboard.id, storeName, valueObj);
                    }
                    return true;
                };
                KeyboardInterface.prototype.resetContextCache = function () {
                    this.cachedContext.reset();
                    this.cachedContextEx.reset();
                };
                KeyboardInterface.prototype.defaultBackspace = function (outputTarget) {
                    this.output(1, outputTarget, "");
                };
                /**
                 * Function     processKeystroke
                 * Scope        Private
                 * @param       {Object}        element     The page element receiving input
                 * @param       {Object}        keystroke   The input keystroke (with its properties) to be mapped by the keyboard.
                 * Description  Encapsulates calls to keyboard input processing.
                 * @returns     {number}        0 if no match is made, otherwise 1.
                 */
                KeyboardInterface.prototype.processKeystroke = function (outputTarget, keystroke) {
                    // Clear internal state tracking data from prior keystrokes.
                    if (!outputTarget) {
                        throw "No target specified for keyboard output!";
                    }
                    else if (!this.activeKeyboard) {
                        throw "No active keyboard for keystroke processing!";
                    }
                    outputTarget.invalidateSelection();
                    outputTarget.deadkeys().resetMatched(); // I3318    
                    this.resetContextCache();
                    // Capture the initial state of the OutputTarget before any rules are matched.
                    var preInput = text.Mock.from(outputTarget);
                    // Establishes the results object, allowing corresponding commands to set values here as appropriate.
                    this.ruleBehavior = new text.RuleBehavior();
                    // Ensure the settings are in place so that KIFS/ifState activates and deactivates
                    // the appropriate rule(s) for the modeled device.
                    this.activeDevice = keystroke.device;
                    // Calls the start-group of the active keyboard.
                    this.activeTargetOutput = outputTarget;
                    var matched = this.activeKeyboard.process(outputTarget, keystroke);
                    this.activeTargetOutput = null;
                    if (!matched) {
                        return null;
                    }
                    // Finalize the rule's results.
                    this.ruleBehavior.transcription = outputTarget.buildTranscriptionFrom(preInput, keystroke);
                    // Clear our result-tracking variable to prevent any possible pollution for future processing.
                    var behavior = this.ruleBehavior;
                    this.ruleBehavior = null;
                    return behavior;
                };
                // Publishes the KeyboardInterface's shorthand API names.
                // Note that this may need to be called multiple times; a keyboard-processor consumer
                // may extend or overwrite some of the callbacks after this method's initial call.
                KeyboardInterface.__publishShorthandAPI = function () {
                    // Keyboard callbacks
                    var prototype = this.prototype;
                    var exportKBCallback = function (miniName, longName) {
                        prototype[miniName] = prototype[longName];
                    };
                    exportKBCallback('KSF', 'saveFocus');
                    exportKBCallback('KBR', 'beepReset');
                    exportKBCallback('KT', 'insertText');
                    exportKBCallback('KR', 'registerKeyboard');
                    exportKBCallback('KRS', 'registerStub');
                    exportKBCallback('KC', 'context');
                    exportKBCallback('KN', 'nul');
                    exportKBCallback('KCM', 'contextMatch');
                    exportKBCallback('KFCM', 'fullContextMatch');
                    exportKBCallback('KIK', 'isKeypress');
                    exportKBCallback('KKM', 'keyMatch');
                    exportKBCallback('KSM', 'stateMatch');
                    exportKBCallback('KKI', 'keyInformation');
                    exportKBCallback('KDM', 'deadkeyMatch');
                    exportKBCallback('KB', 'beep');
                    exportKBCallback('KA', 'any');
                    exportKBCallback('KDC', 'deleteContext');
                    exportKBCallback('KO', 'output');
                    exportKBCallback('KDO', 'deadkeyOutput');
                    exportKBCallback('KIO', 'indexOutput');
                    exportKBCallback('KIFS', 'ifStore');
                    exportKBCallback('KSETS', 'setStore');
                    exportKBCallback('KLOAD', 'loadStore');
                    exportKBCallback('KSAVE', 'saveStore');
                };
                KeyboardInterface.GLOBAL_NAME = 'KeymanWeb';
                KeyboardInterface.TSS_LAYER = 33;
                KeyboardInterface.TSS_PLATFORM = 31;
                return KeyboardInterface;
            }());
            text.KeyboardInterface = KeyboardInterface;
            (function () {
                // This will be the only call within the keyboard-processor module.
                KeyboardInterface.__publishShorthandAPI();
            }());
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
/// <reference path="defaultLayouts.ts" />
/// <reference path="activeLayout.ts" />
/// <reference path="../text/kbdInterface.ts" />
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var keyboards;
        (function (keyboards) {
            /**
             * Stores preprocessed properties of a keyboard for quick retrieval later.
             */
            var CacheTag = /** @class */ (function () {
                function CacheTag() {
                    this.stores = {};
                }
                return CacheTag;
            }());
            var LayoutState;
            (function (LayoutState) {
                LayoutState[LayoutState["NOT_LOADED"] = undefined] = "NOT_LOADED";
                LayoutState[LayoutState["POLYFILLED"] = 1] = "POLYFILLED";
                LayoutState[LayoutState["CALIBRATED"] = 2] = "CALIBRATED";
            })(LayoutState = keyboards.LayoutState || (keyboards.LayoutState = {}));
            /**
             * Acts as a wrapper class for Keyman keyboards compiled to JS, providing type information
             * and keyboard-centered functionality in an object-oriented way without modifying the
             * wrapped keyboard itself.
             */
            var Keyboard = /** @class */ (function () {
                function Keyboard(keyboardScript) {
                    if (keyboardScript) {
                        this.scriptObject = keyboardScript;
                    }
                    else {
                        this.scriptObject = Keyboard.DEFAULT_SCRIPT_OBJECT;
                    }
                    this.layoutStates = {};
                }
                /**
                 * Calls the keyboard's `gs` function, which represents the keyboard source's group(main).
                 */
                Keyboard.prototype.process = function (outputTarget, keystroke) {
                    return this.scriptObject['gs'](outputTarget, keystroke);
                };
                Object.defineProperty(Keyboard.prototype, "isHollow", {
                    get: function () {
                        return this.scriptObject == Keyboard.DEFAULT_SCRIPT_OBJECT;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "id", {
                    get: function () {
                        return this.scriptObject['KI'];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "name", {
                    get: function () {
                        return this.scriptObject['KN'];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "displaysUnderlyingKeys", {
                    get: function () {
                        // Returns false if undefined or false-like (including 0), true otherwise.
                        return !!this.scriptObject['KDU'];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "_legacyLayoutSpec", {
                    // TODO:  Better typing.
                    get: function () {
                        return this.scriptObject['KV']; // used with buildDefaultLayout; layout must be constructed at runtime.
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "_layouts", {
                    // May return null if no layouts exist or have been initialized.
                    get: function () {
                        return this.scriptObject['KVKL']; // This one is compiled by Developer's visual keyboard layout editor.
                    },
                    set: function (value) {
                        this.scriptObject['KVKL'] = value;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "compilerVersion", {
                    get: function () {
                        return new keyman.utils.Version(this.scriptObject['KVER']);
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "isMnemonic", {
                    get: function () {
                        return !!this.scriptObject['KM'];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "definesPositionalOrMnemonic", {
                    get: function () {
                        return typeof this.scriptObject['KM'] != 'undefined';
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "helpText", {
                    /**
                     * HTML help text which is a one liner intended for the status bar of the desktop OSK originally.
                     *
                     * Reference: https://help.keyman.com/developer/language/reference/kmw_helptext
                     */
                    get: function () {
                        return this.scriptObject['KH'];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "hasHelpHTML", {
                    get: function () {
                        return !!this.scriptObject['KHF'];
                    },
                    enumerable: true,
                    configurable: true
                });
                /**
                 * Replaces the OSK with custom HTML, which may be interactive (like with sil_euro_latin).
                 *
                 * Reference: https://help.keyman.com/developer/language/reference/kmw_helpfile
                 */
                Keyboard.prototype.insertHelpHTML = function (e) {
                    // e:  Expects the OSKManager's _Box element.  We don't add type info here b/c it would
                    //     reference the DOM.
                    this.scriptObject['KHF'](e);
                };
                Object.defineProperty(Keyboard.prototype, "oskStyling", {
                    get: function () {
                        return this.scriptObject['KCSS'];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "isCJK", {
                    /**
                     * true if this keyboard uses a (legacy) pick list (Chinese, Japanese, Korean, etc.)
                     *
                     * TODO:  Make a property on keyboards (say, `isPickList` / `KPL`) to signal this when we
                     *        get around to better, generalized picker-list support.
                     */
                    get: function () {
                        var lg;
                        if (typeof (this.scriptObject['KLC']) != 'undefined') {
                            lg = this.scriptObject['KLC'];
                        }
                        else if (typeof (this.scriptObject['LanguageCode']) != 'undefined') {
                            lg = this.scriptObject['LanguageCode'];
                        }
                        // While some of these aren't proper BCP-47 language codes, the CJK keyboards predate our use of BCP-47.
                        // So, we preserve the old ISO 639-3 codes, as that's what the keyboards are matching against.
                        return ((lg == 'cmn') || (lg == 'jpn') || (lg == 'kor'));
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "isRTL", {
                    get: function () {
                        return !!this.scriptObject['KRTL'];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "modifierBitmask", {
                    /**
                     * Obtains the currently-active modifier bitmask for the active keyboard.
                     */
                    get: function () {
                        // NON_CHIRAL is the default bitmask if KMBM is not defined.
                        // We always need a bitmask to compare against, as seen in `isChiral`.
                        return this.scriptObject['KMBM'] || keyman.text.Codes.modifierBitmasks['NON_CHIRAL'];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "isChiral", {
                    get: function () {
                        return !!(this.modifierBitmask & keyman.text.Codes.modifierBitmasks['IS_CHIRAL']);
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "desktopFont", {
                    get: function () {
                        if (this.scriptObject['KV']) {
                            return this.scriptObject['KV']['F'];
                        }
                        else {
                            return null;
                        }
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "cacheTag", {
                    get: function () {
                        var tag = this.scriptObject['_kmw'];
                        if (!tag) {
                            tag = new CacheTag();
                            this.scriptObject['_kmw'] = tag;
                        }
                        return tag;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "explodedStores", {
                    get: function () {
                        return this.cacheTag.stores;
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(Keyboard.prototype, "emulatesAltGr", {
                    /**
                     * Signifies whether or not a layout or OSK should include AltGr / Right-alt emulation for this keyboard.
                     * @param   {Object=}   keyLabels
                     * @return  {boolean}
                     */
                    get: function () {
                        var modifierCodes = keyman.text.Codes.modifierCodes;
                        // If we're not chiral, we're not emulating.
                        if (!this.isChiral) {
                            return false;
                        }
                        if (this._legacyLayoutSpec == null) {
                            return false;
                        }
                        // Only exists in KMW 10.0+, but before that Web had no chirality support, so... return false.
                        var layers = this._legacyLayoutSpec['KLS'];
                        if (!layers) {
                            return false;
                        }
                        var emulationMask = modifierCodes['LCTRL'] | modifierCodes['LALT'];
                        var unshiftedEmulationLayer = layers[keyboards.Layouts.getLayerId(emulationMask)];
                        var shiftedEmulationLayer = layers[keyboards.Layouts.getLayerId(modifierCodes['SHIFT'] | emulationMask)];
                        // buildDefaultLayout ensures that these are aliased to the original modifier set being emulated.
                        // As a result, we can directly test for reference equality.
                        //
                        // This allows us to still return `true` after creating the layers for emulation; during keyboard
                        // construction, the two layers should be null for AltGr emulation to succeed.
                        if (unshiftedEmulationLayer != null &&
                            unshiftedEmulationLayer != layers[keyboards.Layouts.getLayerId(modifierCodes['RALT'])]) {
                            return false;
                        }
                        if (shiftedEmulationLayer != null &&
                            shiftedEmulationLayer != layers[keyboards.Layouts.getLayerId(modifierCodes['RALT'] | modifierCodes['SHIFT'])]) {
                            return false;
                        }
                        // It's technically possible for the OSK to not specify anything while allowing chiral input.  A last-ditch catch:
                        var bitmask = this.modifierBitmask;
                        if ((bitmask & emulationMask) != emulationMask) {
                            // At least one of the emulation modifiers is never used by the keyboard!  We can confirm everything's safe.
                            return true;
                        }
                        if (unshiftedEmulationLayer == null && shiftedEmulationLayer == null) {
                            // We've run out of things to go on; we can't detect if chiral AltGr emulation is intended or not.
                            // TODO:  handle this again!
                            // if(!osk.altGrWarning) {
                            //   console.warn("Could not detect if AltGr emulation is safe, but defaulting to active emulation!")
                            //   // Avoid spamming the console with warnings on every call of the method.
                            //   osk.altGrWarning = true;
                            // }
                            return true;
                        }
                        return true;
                    },
                    enumerable: true,
                    configurable: true
                });
                Keyboard.prototype.usesDesktopLayoutOnDevice = function (device) {
                    if (this.scriptObject['KVKL']) {
                        // A custom mobile layout is defined... but are we using it?
                        return device.formFactor == keyman.text.FormFactor.Desktop;
                    }
                    else {
                        return true;
                    }
                };
                /**
                 * @param       {number}    _PCommand     event code (16,17,18) or 0
                 * @param       {Object}    _PTarget      target element
                 * @param       {number}    _PData        1 or 0
                 * Notifies keyboard of keystroke or other event
                 */
                Keyboard.prototype.notify = function (_PCommand, _PTarget, _PData) {
                    // Good example use case - the Japanese CJK-picker keyboard
                    if (typeof (this.scriptObject['KNS']) == 'function') {
                        this.scriptObject['KNS'](_PCommand, _PTarget, _PData);
                    }
                };
                Keyboard.prototype.findOrConstructLayout = function (formFactor) {
                    if (this._layouts) {
                        // Search for viable layouts.  `null` is allowed for desktop form factors when help text is available,
                        // so we check explicitly against `undefined`.
                        if (this._layouts[formFactor] !== undefined) {
                            return this._layouts[formFactor];
                        }
                        else if (formFactor == keyman.text.FormFactor.Phone && this._layouts[keyman.text.FormFactor.Tablet]) {
                            return this._layouts[keyman.text.FormFactor.Phone] = this._layouts[keyman.text.FormFactor.Tablet];
                        }
                        else if (formFactor == keyman.text.FormFactor.Tablet && this._layouts[keyman.text.FormFactor.Phone]) {
                            return this._layouts[keyman.text.FormFactor.Tablet] = this._layouts[keyman.text.FormFactor.Phone];
                        }
                    }
                    // No pre-built layout available; time to start constructing it via defaults.
                    // First, if we have non-default keys specified by the ['BK'] array, we've got
                    // enough to work with to build a default layout.
                    var rawSpecifications = null; // TODO:  better typing, same type as this._legacyLayoutSpec.
                    if (this._legacyLayoutSpec != null && this._legacyLayoutSpec['KLS']) { // KLS is only specified whenever there are non-default keys.
                        rawSpecifications = this._legacyLayoutSpec;
                    }
                    else if (this._legacyLayoutSpec != null && this._legacyLayoutSpec['BK'] != null) {
                        var keyCaps = this._legacyLayoutSpec['BK'];
                        for (var i = 0; i < keyCaps.length; i++) {
                            if (keyCaps[i].length > 0) {
                                rawSpecifications = this._legacyLayoutSpec;
                                break;
                            }
                        }
                    }
                    // If we don't have key definitions to use for a layout but also lack help text or are a touch-based layout,
                    // we make a default layout anyway.  We have to show display something usable.
                    if (!rawSpecifications && (this.helpText == '' || formFactor != keyman.text.FormFactor.Desktop)) {
                        rawSpecifications = { 'F': 'Tahoma', 'BK': keyboards.Layouts.dfltText };
                    }
                    // Regardless of success, we'll want to initialize the field that backs the property; 
                    // may as well cache the default layout we just built, or a 'null' if it shouldn't exist..
                    if (!this._layouts) {
                        this._layouts = {};
                    }
                    // Final check - do we construct a layout, or is this a case where helpText / insertHelpHTML should take over?
                    if (rawSpecifications) {
                        // Now to generate a layout from our raw specifications.
                        var layout = this._layouts[formFactor] = keyboards.Layouts.buildDefaultLayout(rawSpecifications, this, formFactor);
                        layout.isDefault = true;
                        return layout;
                    }
                    else {
                        // The fact that it doesn't exist will indicate that help text/HTML should be inserted instead.
                        this._layouts[formFactor] = null; // provides a cached value for the check at the top of this method.
                        return null;
                    }
                };
                /**
                 * Returns an ActiveLayout object representing the keyboard's layout for this form factor.  May return null if a custom desktop "help" OSK is defined, as with sil_euro_latin.
                 *
                 * In such cases, please use either `helpText` or `insertHelpHTML` instead.
                 * @param formFactor {string} The desired form factor for the layout.
                 */
                Keyboard.prototype.layout = function (formFactor) {
                    var rawLayout = this.findOrConstructLayout(formFactor);
                    if (rawLayout) {
                        // Prevents accidentally reprocessing layouts; it's a simple enough check.
                        if (this.layoutStates[formFactor] == LayoutState.NOT_LOADED) {
                            rawLayout = keyboards.ActiveLayout.polyfill(rawLayout, this, formFactor);
                            this.layoutStates[formFactor] = LayoutState.POLYFILLED;
                        }
                        return rawLayout;
                    }
                    else {
                        return null;
                    }
                };
                Keyboard.prototype.markLayoutCalibrated = function (formFactor) {
                    if (this.layoutStates[formFactor] != LayoutState.NOT_LOADED) {
                        this.layoutStates[formFactor] = LayoutState.CALIBRATED;
                    }
                };
                Keyboard.prototype.getLayoutState = function (formFactor) {
                    return this.layoutStates[formFactor];
                };
                Keyboard.DEFAULT_SCRIPT_OBJECT = {
                    'gs': function (outputTarget, keystroke) { return false; },
                    'KI': '',
                    'KN': '',
                    'KV': keyboards.Layouts.DEFAULT_RAW_SPEC,
                    'KM': 0 // May not be the best default, but this matches current behavior when there is no activeKeyboard.
                };
                return Keyboard;
            }());
            keyboards.Keyboard = Keyboard;
        })(keyboards = keyman.keyboards || (keyman.keyboards = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
/***
   KeymanWeb 11.0
   Copyright 2019 SIL International
***/
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var KeyMap = /** @class */ (function () {
            function KeyMap() {
            }
            return KeyMap;
        }());
        var BrowserKeyMaps = /** @class */ (function () {
            function BrowserKeyMaps() {
                this.FF = new KeyMap();
                this.Safari = new KeyMap();
                this.Opera = new KeyMap();
                //ffie['k109'] = 189; // -    // These two number-pad VK rules are *not* correct for more recent FF! JMD 8/11/12
                //ffie['k107'] = 187; // =    // FF 3.0 // I2062
                this.FF['k61'] = 187; // =   // FF 2.0
                this.FF['k59'] = 186; // ;
            }
            return BrowserKeyMaps;
        }());
        var LanguageKeyMaps = /** @class */ (function () {
            // // Here are some old legacy definitions that were no longer referenced but are likely related:
            // static _BaseLayoutEuro: {[code: string]: string} = {
            //   'se': '\u00a71234567890+´~~~QWERTYUIOP\u00c5\u00a8\'~~~ASDFGHJKL\u00d6\u00c4~~~~~<ZXCVBNM,.-~~~~~ ',  // Swedish
            //   'uk': '`1234567890-=~~~QWERTYUIOP[]#~~~ASDFGHJKL;\'~~~~~\\ZXCVBNM,./~~~~~ ' // UK
            function LanguageKeyMaps() {
                /* I732 START - 13/03/2007 MCD: Swedish: Start mapping of keystroke to US keyboard #2 */
                // Swedish key map
                this['se'] = new KeyMap();
                this['se']['k220'] = 192; // `
                this['se']['k187'] = 189; // -
                this['se']['k219'] = 187; // =
                this['se']['k221'] = 219; // [
                this['se']['k186'] = 221; // ]
                this['se']['k191'] = 220; // \
                this['se']['k192'] = 186; // ;
                this['se']['k189'] = 191; // /
                this['uk'] = new KeyMap(); // I1299
                this['uk']['k223'] = 192; // // ` U+00AC (logical not) =>  ` ~
                this['uk']['k192'] = 222; // ' @  =>  ' "
                this['uk']['k222'] = 226; // # ~  => K_oE2     // I1504 - UK keyboard mixup #, \
                this['uk']['k220'] = 220; // \ |  => \ |       // I1504 - UK keyboard mixup #, \
            }
            return LanguageKeyMaps;
        }());
        var KeyMapping = /** @class */ (function () {
            function KeyMapping() {
                // Do not construct this class.
            }
            KeyMapping._usCodeInit = function () {
                var s0 = new KeyMap(), s1 = new KeyMap();
                s0['k192'] = 96;
                s0['k49'] = 49;
                s0['k50'] = 50;
                s0['k51'] = 51;
                s0['k52'] = 52;
                s0['k53'] = 53;
                s0['k54'] = 54;
                s0['k55'] = 55;
                s0['k56'] = 56;
                s0['k57'] = 57;
                s0['k48'] = 48;
                s0['k189'] = 45;
                s0['k187'] = 61;
                s0['k81'] = 113;
                s0['k87'] = 119;
                s0['k69'] = 101;
                s0['k82'] = 114;
                s0['k84'] = 116;
                s0['k89'] = 121;
                s0['k85'] = 117;
                s0['k73'] = 105;
                s0['k79'] = 111;
                s0['k80'] = 112;
                s0['k219'] = 91;
                s0['k221'] = 93;
                s0['k220'] = 92;
                s0['k65'] = 97;
                s0['k83'] = 115;
                s0['k68'] = 100;
                s0['k70'] = 102;
                s0['k71'] = 103;
                s0['k72'] = 104;
                s0['k74'] = 106;
                s0['k75'] = 107;
                s0['k76'] = 108;
                s0['k186'] = 59;
                s0['k222'] = 39;
                s0['k90'] = 122;
                s0['k88'] = 120;
                s0['k67'] = 99;
                s0['k86'] = 118;
                s0['k66'] = 98;
                s0['k78'] = 110;
                s0['k77'] = 109;
                s0['k188'] = 44;
                s0['k190'] = 46;
                s0['k191'] = 47;
                s1['k192'] = 126;
                s1['k49'] = 33;
                s1['k50'] = 64;
                s1['k51'] = 35;
                s1['k52'] = 36;
                s1['k53'] = 37;
                s1['k54'] = 94;
                s1['k55'] = 38;
                s1['k56'] = 42;
                s1['k57'] = 40;
                s1['k48'] = 41;
                s1['k189'] = 95;
                s1['k187'] = 43;
                s1['k81'] = 81;
                s1['k87'] = 87;
                s1['k69'] = 69;
                s1['k82'] = 82;
                s1['k84'] = 84;
                s1['k89'] = 89;
                s1['k85'] = 85;
                s1['k73'] = 73;
                s1['k79'] = 79;
                s1['k80'] = 80;
                s1['k219'] = 123;
                s1['k221'] = 125;
                s1['k220'] = 124;
                s1['k65'] = 65;
                s1['k83'] = 83;
                s1['k68'] = 68;
                s1['k70'] = 70;
                s1['k71'] = 71;
                s1['k72'] = 72;
                s1['k74'] = 74;
                s1['k75'] = 75;
                s1['k76'] = 76;
                s1['k186'] = 58;
                s1['k222'] = 34;
                s1['k90'] = 90;
                s1['k88'] = 88;
                s1['k67'] = 67;
                s1['k86'] = 86;
                s1['k66'] = 66;
                s1['k78'] = 78;
                s1['k77'] = 77;
                s1['k188'] = 60;
                s1['k190'] = 62;
                s1['k191'] = 63;
                KeyMapping._usCharCodes = [s0, s1];
            };
            /**
             * Function     _USKeyCodeToCharCode
             * Scope        Private
             * @param       {Event}     Levent      KMW event object
             * @return      {number}                Character code
             * Description Translate keyboard codes to standard US layout codes
             */
            KeyMapping._USKeyCodeToCharCode = function (Levent) {
                return KeyMapping.usCharCodes[Levent.Lmodifiers & 0x10 ? 1 : 0]['k' + Levent.Lcode];
            };
            ;
            Object.defineProperty(KeyMapping, "usCharCodes", {
                get: function () {
                    if (!KeyMapping._usCharCodes) {
                        KeyMapping._usCodeInit();
                    }
                    return KeyMapping._usCharCodes;
                },
                enumerable: true,
                configurable: true
            });
            KeyMapping.browserMap = new BrowserKeyMaps();
            KeyMapping.languageMap = new LanguageKeyMaps();
            return KeyMapping;
        }());
        keyman.KeyMapping = KeyMapping;
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var utils;
        (function (utils) {
            /**
             * Returns the base global object available to the current JS platform.
             * - In browsers, returns `window`.
             * - In WebWorkers, returns `self`.
             * - In Node, returns `global`.
             */
            function getGlobalObject() {
                // Evergreen browsers have started defining 'globalThis'.  
                // Refer to https://devblogs.microsoft.com/typescript/announcing-typescript-3-4/#type-checking-for-globalthis
                // and its referenced polyfill.  Said polyfill is very complex, so we opt for this far leaner variant.
                if (typeof globalThis != 'undefined') {
                    return globalThis; // Not available in IE or older Edge versions
                    // @ts-ignore (TS will throw errors for whatever platform we're not compiling for.)
                }
                else if (typeof window != 'undefined') {
                    // @ts-ignore
                    return window; // The browser-based classic
                    // @ts-ignore
                }
                else if (typeof self != 'undefined') {
                    // @ts-ignore
                    return self; // WebWorker global
                }
                else {
                    // Assumption - if neither of the above exist, we're in Node, for unit-testing.
                    // Node doesn't have as many methods and properties as the other two, but what 
                    // matters for us is that it's the base global.
                    //
                    // Some other headless JS solutions use 'this' instead, but Node's enough for our needs.
                    // @ts-ignore
                    return global;
                }
            }
            utils.getGlobalObject = getGlobalObject;
        })(utils = keyman.utils || (keyman.utils = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
// Includes KMW string extension declarations.
/// <reference path="kmwstring.ts" />
// Establishes key-code definitions.
/// <reference path="codes.ts" />
// Defines our generalized "KeyEvent" class.
/// <reference path="keyEvent.ts" />
// Defines the RuleBehavior keyboard-processing return object.
/// <reference path="ruleBehavior.ts" />
// Defines default key handling behaviors.
/// <reference path="defaultOutput.ts" />
// Defines the keyboard wrapper object.
/// <reference path="../keyboards/keyboard.ts" />
// Defines built-in keymapping.
/// <reference path="keyMapping.ts" />
// Defines a core-compatible 'Device' analogue for use in keyEvent processing
/// <reference path="engineDeviceSpec.ts" />
// Defines the getGlobalObject() utility method.
/// <reference path="../utils/globalObject.ts" />
var com;
(function (com) {
    var keyman;
    (function (keyman) {
        var text;
        (function (text) {
            var KeyboardProcessor = /** @class */ (function () {
                function KeyboardProcessor(options) {
                    // Tracks the simulated value for supported state keys, allowing the OSK to mirror a physical keyboard for them.
                    // Using the exact keyCode name from the Codes definitions will allow for certain optimizations elsewhere in the code.
                    this.stateKeys = {
                        "K_CAPS": false,
                        "K_NUMLOCK": false,
                        "K_SCROLL": false
                    };
                    // Tracks the most recent modifier state information in order to quickly detect changes
                    // in keyboard state not otherwise captured by the hosting page in the browser.
                    // Needed for AltGr simulation.
                    this.modStateFlags = 0;
                    if (!options) {
                        options = KeyboardProcessor.DEFAULT_OPTIONS;
                    }
                    this.baseLayout = options.baseLayout || KeyboardProcessor.DEFAULT_OPTIONS.baseLayout;
                    this.keyboardInterface = new text.KeyboardInterface(options.variableStoreSerializer);
                    this.installInterface();
                }
                KeyboardProcessor.prototype.installInterface = function () {
                    // TODO:  replace 'window' with a (currently-unwritten) utility call that retrieves 
                    //        the global object (whether browser, Node, WebWorker).
                    //
                    //        We must ensure that the keyboard can find the API functions at the expected place.
                    var globalThis = keyman.utils.getGlobalObject();
                    globalThis[text.KeyboardInterface.GLOBAL_NAME] = this.keyboardInterface;
                    // Ensure that the active keyboard is set on the keyboard interface object.
                    if (this.activeKeyboard) {
                        this.keyboardInterface.activeKeyboard = this.activeKeyboard;
                    }
                };
                Object.defineProperty(KeyboardProcessor.prototype, "activeKeyboard", {
                    get: function () {
                        return this.keyboardInterface.activeKeyboard;
                    },
                    set: function (keyboard) {
                        this.keyboardInterface.activeKeyboard = keyboard;
                        // All old deadkeys and keyboard-specific cache should immediately be invalidated
                        // on a keyboard change.
                        this.resetContext();
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(KeyboardProcessor.prototype, "layerStore", {
                    get: function () {
                        return this.keyboardInterface.systemStores[text.KeyboardInterface.TSS_LAYER];
                    },
                    enumerable: true,
                    configurable: true
                });
                Object.defineProperty(KeyboardProcessor.prototype, "layerId", {
                    get: function () {
                        return this.layerStore.value;
                    },
                    // Note:  will trigger an 'event' callback designed to notify the OSK of layer changes.
                    set: function (value) {
                        this.layerStore.set(value);
                    },
                    enumerable: true,
                    configurable: true
                });
                /**
                 * Get the default RuleBehavior for the specified key, attempting to mimic standard browser defaults
                 * where and when appropriate.
                 *
                 * @param   {object}  Lkc  The pre-analyzed key event object
                 * @param   {boolean} usingOSK
                 * @return  {string}
                 */
                KeyboardProcessor.prototype.defaultRuleBehavior = function (Lkc) {
                    var outputTarget = Lkc.Ltarg;
                    var preInput = text.Mock.from(outputTarget);
                    var ruleBehavior = new text.RuleBehavior();
                    var matched = false;
                    var char = '';
                    var special;
                    if (Lkc.isSynthetic || outputTarget.isSynthetic) {
                        matched = true; // All the conditions below result in matches until the final else, which restores the expected default
                        // if no match occurs.
                        if (text.DefaultOutput.isCommand(Lkc)) {
                            // Note this in the rule behavior, return successfully.  We'll consider applying it later.
                            ruleBehavior.triggersDefaultCommand = true;
                            // We'd rather let the browser handle these keys, but we're using emulated keystrokes, forcing KMW
                            // to emulate default behavior here.
                        }
                        else if ((special = text.DefaultOutput.forSpecialEmulation(Lkc)) != null) {
                            switch (special) {
                                case text.EmulationKeystrokes.Backspace:
                                    this.keyboardInterface.defaultBackspace(outputTarget);
                                    break;
                                case text.EmulationKeystrokes.Enter:
                                    outputTarget.handleNewlineAtCaret();
                                    break;
                                case text.EmulationKeystrokes.Space:
                                    this.keyboardInterface.output(0, outputTarget, ' ');
                                    break;
                                // case '\u007f': // K_DEL
                                // // For (possible) future implementation.
                                // // Would recommend (conceptually) equaling K_RIGHT + K_BKSP, the former of which would technically be a 'command'.
                                default:
                                    // In case we extend the allowed set, but forget to implement its handling case above.
                                    ruleBehavior.errorLog = "Unexpected 'special emulation' character (\\u" + special.kmwCharCodeAt(0).toString(16) + ") went unhandled!";
                            }
                        }
                        else {
                            // Back to the standard default, pending normal matching.
                            matched = false;
                        }
                    }
                    var isMnemonic = this.activeKeyboard && this.activeKeyboard.isMnemonic;
                    if (!matched) {
                        if ((char = text.DefaultOutput.forAny(Lkc, isMnemonic)) != null) {
                            special = text.DefaultOutput.forSpecialEmulation(Lkc);
                            if (special == text.EmulationKeystrokes.Backspace) {
                                // A browser's default backspace may fail to delete both parts of an SMP character.
                                this.keyboardInterface.defaultBackspace(Lkc.Ltarg);
                            }
                            else if (special || text.DefaultOutput.isCommand(Lkc)) { // Filters out 'commands' like TAB.
                                // We only do the "for special emulation" cases under the condition above... aside from backspace
                                // Let the browser handle those.
                                return null;
                            }
                            else {
                                this.keyboardInterface.output(0, outputTarget, char);
                            }
                        }
                        else {
                            // No match, no default RuleBehavior.
                            return null;
                        }
                    }
                    // Shortcut things immediately if there were issues generating this rule behavior.
                    if (ruleBehavior.errorLog) {
                        return ruleBehavior;
                    }
                    var transcription = outputTarget.buildTranscriptionFrom(preInput, Lkc);
                    ruleBehavior.transcription = transcription;
                    return ruleBehavior;
                };
                KeyboardProcessor.prototype.setSyntheticEventDefaults = function (Lkc) {
                    // Set the flags for the state keys.
                    Lkc.Lstates |= this.stateKeys['K_CAPS'] ? text.Codes.modifierCodes['CAPS'] : text.Codes.modifierCodes['NO_CAPS'];
                    Lkc.Lstates |= this.stateKeys['K_NUMLOCK'] ? text.Codes.modifierCodes['NUM_LOCK'] : text.Codes.modifierCodes['NO_NUM_LOCK'];
                    Lkc.Lstates |= this.stateKeys['K_SCROLL'] ? text.Codes.modifierCodes['SCROLL_LOCK'] : text.Codes.modifierCodes['NO_SCROLL_LOCK'];
                    // Set LisVirtualKey to false to ensure that nomatch rule does fire for U_xxxx keys
                    if (Lkc.kName.substr(0, 2) == 'U_') {
                        Lkc.LisVirtualKey = false;
                    }
                    // Get code for non-physical keys (T_KOKAI, U_05AB etc)
                    if (typeof Lkc.Lcode == 'undefined') {
                        Lkc.Lcode = this.getVKDictionaryCode(Lkc.kName); // Updated for Build 347
                        if (!Lkc.Lcode) {
                            // Special case for U_xxxx keys. This vk code will never be used
                            // in a keyboard, so we use this to ensure that keystroke processing
                            // occurs for the key.
                            Lkc.Lcode = 1;
                        }
                    }
                    // Handles modifier states when the OSK is emulating rightalt through the leftctrl-leftalt layer.
                    if ((Lkc.Lmodifiers & text.Codes.modifierBitmasks['ALT_GR_SIM']) == text.Codes.modifierBitmasks['ALT_GR_SIM'] && this.activeKeyboard.emulatesAltGr) {
                        Lkc.Lmodifiers &= ~text.Codes.modifierBitmasks['ALT_GR_SIM'];
                        Lkc.Lmodifiers |= text.Codes.modifierCodes['RALT'];
                    }
                };
                KeyboardProcessor.prototype.processKeystroke = function (keyEvent, outputTarget) {
                    var matchBehavior;
                    // Pass this key code and state to the keyboard program
                    if (this.activeKeyboard && keyEvent.Lcode != 0) {
                        /*
                         * The `this.installInterface()` call is insurance against something I've seen in unit tests when things break a bit.
                         *
                         * Currently, when a KMW shutdown doesn't go through properly or completely, sometimes we end up with parallel
                         * versions of KMW running, and an old, partially-shutdown one will "snipe" a command meant for the most-recent
                         * one's test. So, installing here ensures that the active Processor has its matching KeyboardInterface ready,
                         * even should that occur.
                         */
                        this.installInterface();
                        matchBehavior = this.keyboardInterface.processKeystroke(outputTarget, keyEvent);
                    }
                    if (!matchBehavior) {
                        // Restore the virtual key code if a mnemonic keyboard is being used
                        // If no vkCode value was stored, maintain the original Lcode value.
                        keyEvent.Lcode = keyEvent.vkCode || keyEvent.Lcode;
                        // Handle unmapped keys, including special keys
                        // The following is physical layout dependent, so should be avoided if possible.  All keys should be mapped.
                        this.keyboardInterface.activeTargetOutput = outputTarget;
                        // Match against the 'default keyboard' - rules to mimic the default string output when typing in a browser.
                        // Many keyboards rely upon these 'implied rules'.
                        matchBehavior = this.defaultRuleBehavior(keyEvent);
                        this.keyboardInterface.activeTargetOutput = null;
                    }
                    return matchBehavior;
                };
                // FIXME:  makes some bad assumptions.
                KeyboardProcessor.setMnemonicCode = function (Lkc, shifted, capsActive) {
                    // K_SPACE is not handled by defaultKeyOutput for physical keystrokes unless using touch-aliased elements.
                    // It's also a "exception required, March 2013" for clickKey, so at least they both have this requirement.
                    if (Lkc.Lcode != text.Codes.keyCodes['K_SPACE']) {
                        // So long as the key name isn't prefixed with 'U_', we'll get a default mapping based on the Lcode value.
                        // We need to determine the mnemonic base character - for example, SHIFT + K_PERIOD needs to map to '>'.
                        var mappingEvent = new text.KeyEvent();
                        for (var key in Lkc) {
                            mappingEvent[key] = Lkc[key];
                        }
                        // To facilitate storing relevant commands, we should probably reverse-lookup
                        // the actual keyname instead.
                        mappingEvent.kName = 'K_xxxx';
                        mappingEvent.Ltarg = new text.Mock(); // helps prevent breakage for mnemonics.
                        mappingEvent.Lmodifiers = (shifted ? 0x10 : 0); // mnemonic lookups only exist for default & shift layers.
                        var mappedChar = text.DefaultOutput.forAny(mappingEvent, true);
                        /* First, save a backup of the original code.  This one won't needlessly trigger keyboard
                         * rules, but allows us to replicate/emulate commands after rule processing if needed.
                         * (Like backspaces)
                         */
                        Lkc.vkCode = Lkc.Lcode;
                        if (mappedChar) {
                            // Will return 96 for 'a', which is a keycode corresponding to Codes.keyCodes('K_NP1') - a numpad key.
                            // That stated, we're in mnemonic mode - this keyboard's rules are based on the char codes.
                            Lkc.Lcode = mappedChar.charCodeAt(0);
                        }
                        else {
                            // Don't let command-type keys (like K_DEL, which will output '.' otherwise!)
                            // trigger keyboard rules.
                            delete Lkc.Lcode;
                        }
                    }
                    if (capsActive) {
                        // TODO:  Needs fixing - does not properly mirror physical keystrokes, as Lcode range 96-111 corresponds
                        // to numpad keys!  (Physical keyboard section has its own issues here.)
                        if ((Lkc.Lcode >= 65 && Lkc.Lcode <= 90) /* 'A' - 'Z' */ || (Lkc.Lcode >= 97 && Lkc.Lcode <= 122) /* 'a' - 'z' */) {
                            Lkc.Lmodifiers ^= 0x10; // Flip the 'shifted' bit, so it'll act as the opposite key.
                            Lkc.Lcode ^= 0x20; // Flips the 'upper' vs 'lower' bit for the base 'a'-'z' ASCII alphabetics.
                        }
                    }
                };
                /**
                 * Get modifier key state from layer id
                 *
                 * @param       {string}      layerId       layer id (e.g. ctrlshift)
                 * @return      {number}                    modifier key state (desktop keyboards)
                 */
                KeyboardProcessor.getModifierState = function (layerId) {
                    var modifier = 0;
                    if (layerId.indexOf('shift') >= 0) {
                        modifier |= text.Codes.modifierCodes['SHIFT'];
                    }
                    // The chiral checks must not be directly exclusive due each other to visual OSK feedback.
                    var ctrlMatched = false;
                    if (layerId.indexOf('leftctrl') >= 0) {
                        modifier |= text.Codes.modifierCodes['LCTRL'];
                        ctrlMatched = true;
                    }
                    if (layerId.indexOf('rightctrl') >= 0) {
                        modifier |= text.Codes.modifierCodes['RCTRL'];
                        ctrlMatched = true;
                    }
                    if (layerId.indexOf('ctrl') >= 0 && !ctrlMatched) {
                        modifier |= text.Codes.modifierCodes['CTRL'];
                    }
                    var altMatched = false;
                    if (layerId.indexOf('leftalt') >= 0) {
                        modifier |= text.Codes.modifierCodes['LALT'];
                        altMatched = true;
                    }
                    if (layerId.indexOf('rightalt') >= 0) {
                        modifier |= text.Codes.modifierCodes['RALT'];
                        altMatched = true;
                    }
                    if (layerId.indexOf('alt') >= 0 && !altMatched) {
                        modifier |= text.Codes.modifierCodes['ALT'];
                    }
                    return modifier;
                };
                /**
                 * @summary Look up a custom virtual key code in the virtual key code dictionary KVKD.  On first run, will build the dictionary.
                 *
                 * `VKDictionary` is constructed from the keyboard's `KVKD` member. This list is constructed
                 * at compile-time and is a list of 'additional' virtual key codes, starting at 256 (i.e.
                 * outside the range of standard virtual key codes). These additional codes are both
                 * `[T_xxx]` and `[U_xxxx]` custom key codes from the Keyman keyboard language. However,
                 * `[U_xxxx]` keys only generate an entry in `KVKD` if there is a corresponding rule that
                 * is associated with them in the keyboard rules. If the `[U_xxxx]` key code is only
                 * referenced as the id of a key in the touch layout, then it does not get an entry in
                 * the `KVKD` property.
                 *
                 * @private
                 * @param       {string}      keyName   custom virtual key code to lookup in the dictionary
                 * @return      {number}                key code > 255 on success, or 0 if not found
                 */
                KeyboardProcessor.prototype.getVKDictionaryCode = function (keyName) {
                    var activeKeyboard = this.activeKeyboard;
                    if (!activeKeyboard.scriptObject['VKDictionary']) {
                        var a = [];
                        if (typeof activeKeyboard.scriptObject['KVKD'] == 'string') {
                            // Build the VK dictionary
                            // TODO: Move the dictionary build into the compiler -- so compiler generates code such as following.  
                            // Makes the VKDictionary member unnecessary.
                            //       this.KVKD={"K_ABC":256,"K_DEF":257,...};
                            var s = activeKeyboard.scriptObject['KVKD'].split(' ');
                            for (var i = 0; i < s.length; i++) {
                                a[s[i].toUpperCase()] = i + 256; // We force upper-case since virtual keys should be case-insensitive.
                            }
                        }
                        activeKeyboard.scriptObject['VKDictionary'] = a;
                    }
                    var res = activeKeyboard.scriptObject['VKDictionary'][keyName.toUpperCase()];
                    return res ? res : 0;
                };
                /**
                 * Function     _UpdateVKShift
                 * Scope        Private
                 * @param       {Object}            e     OSK event
                 * @param       {number}            v     keyboard shift state
                 * @param       {(boolean|number)}  d     set (1) or clear(0) shift state bits
                 * @return      {boolean}                 Always true
                 * Description  Updates the current shift state within KMW, updating the OSK's visualization thereof.
                 */
                KeyboardProcessor.prototype._UpdateVKShift = function (e, v, d) {
                    var keyShiftState = 0, lockStates = 0, i;
                    var lockNames = ['CAPS', 'NUM_LOCK', 'SCROLL_LOCK'];
                    var lockKeys = ['K_CAPS', 'K_NUMLOCK', 'K_SCROLL'];
                    if (!this.activeKeyboard) {
                        return true;
                    }
                    if (e) {
                        // read shift states from Pevent
                        keyShiftState = e.Lmodifiers;
                        lockStates = e.Lstates;
                        // Are we simulating AltGr?  If it's a simulation and not real, time to un-simulate for the OSK.
                        if (this.activeKeyboard.isChiral && (this.activeKeyboard.emulatesAltGr) &&
                            (this.modStateFlags & text.Codes.modifierBitmasks['ALT_GR_SIM']) == text.Codes.modifierBitmasks['ALT_GR_SIM']) {
                            keyShiftState |= text.Codes.modifierBitmasks['ALT_GR_SIM'];
                            keyShiftState &= ~text.Codes.modifierCodes['RALT'];
                        }
                        for (i = 0; i < lockNames.length; i++) {
                            if (lockStates & text.Codes.stateBitmasks[lockNames[i]]) {
                                this.stateKeys[lockKeys[i]] = lockStates & text.Codes.modifierCodes[lockNames[i]];
                            }
                        }
                    }
                    else if (d) {
                        keyShiftState |= v;
                        for (i = 0; i < lockNames.length; i++) {
                            if (v & text.Codes.stateBitmasks[lockNames[i]]) {
                                this.stateKeys[lockKeys[i]] = true;
                            }
                        }
                    }
                    else {
                        keyShiftState &= ~v;
                        for (i = 0; i < lockNames.length; i++) {
                            if (v & text.Codes.stateBitmasks[lockNames[i]]) {
                                this.stateKeys[lockKeys[i]] = false;
                            }
                        }
                    }
                    this.layerId = this.getLayerId(keyShiftState);
                    return true;
                };
                KeyboardProcessor.prototype.getLayerId = function (modifier) {
                    return keyman.keyboards.Layouts.getLayerId(modifier);
                };
                /**
                 * Select the OSK's next keyboard layer based upon layer switching keys as a default
                 * The next layer will be determined from the key name unless otherwise specifed
                 *
                 *  @param  {string}                    keyName     key identifier
                 *  @param  {number|string|undefined}   nextLayerIn optional next layer identifier
                 *  @return {boolean}                               return true if keyboard layer changed
                 */
                KeyboardProcessor.prototype.selectLayer = function (keyEvent, fromNameOnly) {
                    if (fromNameOnly === void 0) { fromNameOnly = false; }
                    var keyName = keyEvent.kName;
                    var nextLayer = fromNameOnly ? null : keyEvent.kNextLayer;
                    var isChiral = this.activeKeyboard && this.activeKeyboard.isChiral;
                    // Layer must be identified by name, not number (27/08/2015)
                    if (typeof nextLayer == 'number') {
                        nextLayer = this.getLayerId(nextLayer * 0x10);
                    }
                    // Identify next layer, if required by key
                    if (!nextLayer) {
                        switch (keyName) {
                            case 'K_LSHIFT':
                            case 'K_RSHIFT':
                            case 'K_SHIFT':
                                nextLayer = 'shift';
                                break;
                            case 'K_LCONTROL':
                            case 'K_LCTRL':
                                if (isChiral) {
                                    nextLayer = 'leftctrl';
                                    break;
                                }
                            case 'K_RCONTROL':
                            case 'K_RCTRL':
                                if (isChiral) {
                                    nextLayer = 'rightctrl';
                                    break;
                                }
                            case 'K_CTRL':
                                nextLayer = 'ctrl';
                                break;
                            case 'K_LMENU':
                            case 'K_LALT':
                                if (isChiral) {
                                    nextLayer = 'leftalt';
                                    break;
                                }
                            case 'K_RMENU':
                            case 'K_RALT':
                                if (isChiral) {
                                    nextLayer = 'rightalt';
                                    break;
                                }
                            case 'K_ALT':
                                nextLayer = 'alt';
                                break;
                            case 'K_ALTGR':
                                if (isChiral) {
                                    nextLayer = 'leftctrl-rightalt';
                                }
                                else {
                                    nextLayer = 'ctrl-alt';
                                }
                                break;
                            case 'K_CURRENCIES':
                            case 'K_NUMERALS':
                            case 'K_SHIFTED':
                            case 'K_UPPER':
                            case 'K_LOWER':
                            case 'K_SYMBOLS':
                                nextLayer = 'default';
                                break;
                        }
                    }
                    // If no key corresponding to a layer transition is pressed, maintain the current layer.
                    if (!nextLayer) {
                        return false;
                    }
                    // Change layer and refresh OSK
                    this.updateLayer(keyEvent, nextLayer);
                    return true;
                };
                /**
                 * Sets the new layer id, allowing for toggling shift/ctrl/alt while preserving the remainder
                 * of the modifiers represented by the current layer id (where applicable)
                 *
                 * @param       {string}      id      layer id (e.g. ctrlshift)
                 */
                KeyboardProcessor.prototype.updateLayer = function (keyEvent, id) {
                    var activeLayer = this.layerId;
                    var s = activeLayer;
                    // Do not change layer unless needed (27/08/2015)
                    if (id == activeLayer && keyEvent.device.formFactor != text.FormFactor.Desktop) {
                        return false;
                    }
                    var idx = id;
                    var i;
                    if (keyEvent.device.formFactor == text.FormFactor.Desktop) {
                        // Need to test if target layer is a standard layer (based on the plain 'default')
                        var replacements = ['leftctrl', 'rightctrl', 'ctrl', 'leftalt', 'rightalt', 'alt', 'shift'];
                        for (i = 0; i < replacements.length; i++) {
                            // Don't forget to remove the kebab-case hyphens!
                            idx = idx.replace(replacements[i] + '-', '');
                            idx = idx.replace(replacements[i], '');
                        }
                        // If we are presently on the default layer, drop the 'default' and go straight to the shifted mode.
                        // If on a common symbolic layer, drop out of symbolic mode and go straight to the shifted mode.
                        if (activeLayer == 'default' || activeLayer == 'numeric' || activeLayer == 'symbol' || activeLayer == 'currency' || idx != '') {
                            s = id;
                        }
                        // Otherwise, we are based upon a layer that accepts modifier variations.
                        // Modify the layer according to the current state and key pressed.
                        //
                        // TODO:  Consider:  should this ever be allowed for a base layer other than 'default'?  If not,
                        // if(idx == '') with accompanying if-else structural shift would be a far better test here.
                        else {
                            // Save our current modifier state.
                            var modifier = KeyboardProcessor.getModifierState(s);
                            // Strip down to the base modifiable layer.
                            for (i = 0; i < replacements.length; i++) {
                                // Don't forget to remove the kebab-case hyphens!
                                s = s.replace(replacements[i] + '-', '');
                                s = s.replace(replacements[i], '');
                            }
                            // Toggle the modifier represented by our input argument.
                            switch (id) {
                                case 'shift':
                                    modifier ^= text.Codes.modifierCodes['SHIFT'];
                                    break;
                                case 'leftctrl':
                                    modifier ^= text.Codes.modifierCodes['LCTRL'];
                                    break;
                                case 'rightctrl':
                                    modifier ^= text.Codes.modifierCodes['RCTRL'];
                                    break;
                                case 'ctrl':
                                    modifier ^= text.Codes.modifierCodes['CTRL'];
                                    break;
                                case 'leftalt':
                                    modifier ^= text.Codes.modifierCodes['LALT'];
                                    break;
                                case 'rightalt':
                                    modifier ^= text.Codes.modifierCodes['RALT'];
                                    break;
                                case 'alt':
                                    modifier ^= text.Codes.modifierCodes['ALT'];
                                    break;
                                default:
                                    s = id;
                            }
                            // Combine our base modifiable layer and attach the new modifier variation info to obtain our destination layer.
                            if (s != 'default') {
                                if (s == '') {
                                    s = this.getLayerId(modifier);
                                }
                                else {
                                    s = this.getLayerId(modifier) + '-' + s;
                                }
                            }
                        }
                        if (s == '') {
                            s = 'default';
                        }
                    }
                    else {
                        // Mobile form-factor.  Either the layout is specified by a keyboard developer with direct layer name references
                        // or all layers are accessed via subkey of a single layer-shifting key - no need for modifier-combining logic.
                        s = id;
                    }
                    var layout = this.activeKeyboard.layout(keyEvent.device.formFactor);
                    if (layout.getLayer(s)) {
                        this.layerId = s;
                    }
                    else {
                        this.layerId = 'default';
                    }
                };
                // Returns true if the key event is a modifier press, allowing keyPress to return selectively
                // in those cases.
                KeyboardProcessor.prototype.doModifierPress = function (Levent, isKeyDown) {
                    var outputTarget = Levent.Ltarg;
                    if (!this.activeKeyboard) {
                        return false;
                    }
                    switch (Levent.Lcode) {
                        case 8:
                            outputTarget.deadkeys().clear();
                            break; // I3318 (always clear deadkeys after backspace) 
                        case 16: //"K_SHIFT":16,"K_CONTROL":17,"K_ALT":18
                        case 17:
                        case 18:
                        case 20: //"K_CAPS":20, "K_NUMLOCK":144,"K_SCROLL":145
                        case 144:
                        case 145:
                            // For eventual integration - we bypass an OSK update for physical keystrokes when in touch mode.
                            this.activeKeyboard.notify(Levent.Lcode, outputTarget, isKeyDown ? 1 : 0);
                            if (!Levent.device.touchable) {
                                return this._UpdateVKShift(Levent, Levent.Lcode - 15, 1); // I2187
                            }
                            else {
                                return true;
                            }
                    }
                    if (Levent.LmodifierChange) {
                        this.activeKeyboard.notify(0, outputTarget, 1);
                        this._UpdateVKShift(Levent, 0, 1);
                    }
                    // No modifier keypresses detected.
                    return false;
                };
                KeyboardProcessor.prototype.resetContext = function () {
                    this.layerId = 'default';
                    this.keyboardInterface.resetContextCache();
                    this._UpdateVKShift(null, 15, 0);
                };
                ;
                KeyboardProcessor.prototype.setNumericLayer = function (device) {
                    var layout = this.activeKeyboard.layout(device.formFactor);
                    if (layout.getLayer('numeric')) {
                        this.layerId = 'numeric';
                    }
                };
                ;
                KeyboardProcessor.DEFAULT_OPTIONS = {
                    baseLayout: 'us'
                };
                return KeyboardProcessor;
            }());
            text.KeyboardProcessor = KeyboardProcessor;
        })(text = keyman.text || (keyman.text = {}));
    })(keyman = com.keyman || (com.keyman = {}));
})(com || (com = {}));
(function () {
    var ns = com.keyman.text;
    // Let LMLayer be available both in the browser and in Node.
    if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
        module.exports = ns.KeyboardProcessor;
        //@ts-ignore
        ns.KeyboardProcessor.com = com; // Export the root namespace, while we're at it - just in case.
    }
}());
//# sourceMappingURL=index.js.map