/// <reference types="models-types" />
declare namespace wordBreakers {
    /**
     * Splits ASCII words.
     *
     * @param phrase
     */
    function ascii(phrase: string): Span[];
}
declare namespace wordBreakers {
    namespace data {
        /**
         * Valid values for a word break property.
         */
        const enum WordBreakProperty {
            Other = 0,
            LF = 1,
            Newline = 2,
            CR = 3,
            WSegSpace = 4,
            Double_Quote = 5,
            Single_Quote = 6,
            MidNum = 7,
            MidNumLet = 8,
            Numeric = 9,
            MidLetter = 10,
            ALetter = 11,
            ExtendNumLet = 12,
            Format = 13,
            Extend = 14,
            Hebrew_Letter = 15,
            ZWJ = 16,
            Katakana = 17,
            Regional_Indicator = 18,
            sot = 19,
            eot = 20
        }
        /**
         * Constants for indexing values in WORD_BREAK_PROPERTY.
         */
        const enum I {
            Start = 0,
            Value = 1
        }
        const WORD_BREAK_PROPERTY: [number, WordBreakProperty][];
    }
}
declare namespace wordBreakers {
    /**
     * Word breaker based on Unicode Standard Annex #29, Section 4.1:
     * Default Word Boundary Specification.
     *
     * @see http://unicode.org/reports/tr29/#Word_Boundaries
     * @see https://github.com/eddieantonio/unicode-default-word-boundary/tree/v12.0.0
     */
    function default_(text: string): Span[];
}
declare namespace wordBreakers {
    export { default_ as default };
}
declare namespace wordBreakers {
    /**
     * A **VERY** dumb word breaker that simply splits at words. Do not use this
     * word breaker!
     *
     * @param phrase The phrase in which to break words.
     * @deprecated Use a word breaker tailored to your language instead!
     */
    function placeholder(phrase: string): Span[];
}
