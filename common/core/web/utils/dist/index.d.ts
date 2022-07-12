declare namespace com.keyman.environment {
    var VERSION: string;
    var BUILD: number;
    var ENVIRONMENT: string;
    var SENTRY_RELEASE: string;
}
declare namespace com.keyman.utils {
    /**
     * Function     deepCopy
     * Scope        Private
     * @param       {Object}      p           object to copy
     * @param       {Array=}      c0          array member being copied
     * @return      {Object}                  clone ('deep copy') of object
     * Description  Makes an actual copy (not a reference) of an object, copying simple members,
     *              arrays and member objects but not functions, so use with care!
     */
    function deepCopy<T>(p: T, c0?: any): T;
}
declare namespace com.keyman.utils {
    enum Browser {
        Chrome = "chrome",
        Edge = "edge",
        Firefox = "firefox",
        Native = "native",
        Opera = "opera",
        Safari = "safari",
        Other = "other"
    }
    enum OperatingSystem {
        Windows = "windows",
        macOS = "macosx",
        Linux = "linux",
        Android = "android",
        iOS = "ios",
        Other = "other"
    }
    enum FormFactor {
        Desktop = "desktop",
        Phone = "phone",
        Tablet = "tablet"
    }
    /**
     * This class provides an abstract version of com.keyman.Device that is core-friendly,
     * containing only the information needed by web-core for text processing use, devoid
     * of any direct references to the DOM.
     */
    class DeviceSpec {
        readonly browser: Browser;
        readonly formFactor: FormFactor;
        readonly OS: OperatingSystem;
        readonly touchable: boolean;
        constructor(browser: string, formFactor: string, OS: string, touchable: boolean);
    }
}
declare namespace com.keyman.utils {
    /**
     * Returns the base global object available to the current JS platform.
     * - In browsers, returns `window`.
     * - In WebWorkers, returns `self`.
     * - In Node, returns `global`.
     */
    function getGlobalObject(): typeof globalThis;
}
declare namespace com.keyman.utils {
    class Version {
        static readonly CURRENT: Version;
        static readonly DEVELOPER_VERSION_FALLBACK: Version;
        static readonly NO_DEFAULT_KEYCAPS: Version;
        static readonly MAC_POSSIBLE_IPAD_ALIAS: Version;
        private readonly components;
        /**
         * Parses version information, preparing it for use in comparisons.
         * @param text Either a string representing a version number (ex: "9.0.0") or an array representing
         *             its components (ex: [9, 0, 0]).
         */
        constructor(text: String | number[]);
        get major(): number;
        get minor(): number;
        toString(): string;
        toJSON(): string;
        equals(other: Version): boolean;
        precedes(other: Version): boolean;
        compareTo(other: Version): number;
    }
}
/***
   KeymanWeb 14.0
   Copyright 2020 SIL International
***/
interface StringConstructor {
    kmwFromCharCode(cp0: number): string;
    _kmwFromCharCode(cp0: number): string;
    kmwEnableSupplementaryPlane(bEnable: boolean): any;
}
interface String {
    kmwCharCodeAt(codePointIndex: number): number;
    kmwCharAt(codePointIndex: number): string;
    kmwIndexOf(searchValue: string, fromIndex?: number): number;
    kmwLastIndexOf(searchValue: string, fromIndex?: number): number;
    kmwSlice(beginSlice: number, endSlice: number): string;
    kmwSubstring(start: number, length: number): string;
    kmwSubstr(start: number, length?: number): string;
    kmwBMPSubstr(start: number, length?: number): string;
    kmwLength(): number;
    kmwBMPLength(): number;
    kmwNextChar(codeUnitIndex: number): number;
    kmwBMPNextChar(codeUnitIndex: number): number;
    kmwPrevChar(codeUnitIndex: number): number;
    kmwBMPPrevChar(codeUnitIndex: number): number;
    kmwCodePointToCodeUnit(codePointIndex: number): number;
    kmwBMPCodePointToCodeUnit(codePointIndex: number): number;
    kmwCodeUnitToCodePoint(codeUnitIndex: number): number;
    kmwBMPCodeUnitToCodePoint(codeUnitIndex: number): number;
    _kmwCharCodeAt(codePointIndex: number): number;
    _kmwCharAt(codePointIndex: number): string;
    _kmwIndexOf(searchValue: string, fromIndex?: number): number;
    _kmwLastIndexOf(searchValue: string, fromIndex?: number): number;
    _kmwSlice(beginSlice: number, endSlice: number): string;
    _kmwSubstring(start: number, length?: number): string;
    _kmwSubstr(start: number, length?: number): string;
    _kmwLength(): number;
    _kmwNextChar(codeUnitIndex: number): number;
    _kmwPrevChar(codeUnitIndex: number): number;
    _kmwCodePointToCodeUnit(codePointIndex: number): number;
    _kmwCodeUnitToCodePoint(codeUnitIndex: number): number;
}
