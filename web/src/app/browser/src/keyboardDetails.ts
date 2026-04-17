/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

import { InternalKeyboardFont } from 'keyman/engine/keyboard';

/**
 * Defines keyboard metadata
 */
export interface KeyboardDetails {
    /**
     * true if the keyboard has been loaded, false otherwise
     */
    HasLoaded: boolean,
    /**
     * User-friendly name of the keyboard.
     */
    Name: string,
    /**
     * Internal name of the keyboard.
     */
    InternalName: string,
    /**
     * User-friendly name of the language actively tied to the keyboard.
     */
    LanguageName: string,
    /**
     * The three-letter code used to internally represent the language.
     */
    LanguageCode: string,
    /**
     * The user-friendly name of the region of the world within which
     * the language is predominantly found.
     */
    RegionName: string,
    /**
     * The three-letter code representing the region.
     */
    RegionCode: string,
    /**
     * The user-friendly name of the country in which the language is spoken. (Optional)
     */
    CountryName: string | null,
    /**
     * A three-letter code corresponding to the country. (Optional)
     */
    CountryCode: string| null,
    /**
     * Deprecated. A unique identifier for the keyboard. (Use 'InternalName' instead.)
     */
    KeyboardID: string | null,
    /**
     * The font packaged with the keyboard to support its use. (Optional)
     */
    Font: InternalKeyboardFont | null,
    /**
     * The font packaged with the keyboard to properly display specialized
     * OSK characters. (Optional)
     */
    OskFont: InternalKeyboardFont | null,
    /**
     * Indicates whether the keyboard is designed for right-to-left scripts,
     * null if the keyboard hasn't been loaded yet (and thus the value is unknown).
     */
    IsRTL: boolean | null
};
