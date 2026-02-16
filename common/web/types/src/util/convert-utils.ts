//***************************************************** */

/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Created by S. Schmitt on 2026-01-19
 *
 * util for conversion functions for Keyman
 *
 */


/**
 * @brief  function to convert a character, numeric character reference or a unicode value to a character or unicode Codepoint
 *         if input is a valid single character or Codepoint like 'c','ä', 'ሴ', 'ẘ', '😎',  the same character or Codepoint is returned (e.g. 'c' -> 'c', '😎' -> '😎')
 *         if input is a valid multi character/Codepoint string like 'ab', 'abcde', '😎😆',  the same string is returned (e.g. 'abcde' -> 'abcde' ,  😎😆 ->😎😆 )
 *         if input is a valid Unicode value, numeric character reference in hex or decimal, the corresponding character or unicode Codepointis returned (e.g. &#x1F60E; -> 😎)
 *         if input is a one of the named character reference  &gt; &lt; &amp; &quot; &apos;, the corresponding character is returned ( e.g. '&gt;'  -> '>')
 * @param  inputString the string or stringvalue that will converted
 * @return the input character/Codepoint if input is a valid character/Codepoint
 *         a converted character if input is a valid Unicode value, numeric character reference in hex or decimal, or named character reference
 *         or undefined if input is null or undefined, half a surrogate pair, or not recognized
 */
export function convertToUnicodeCharacter(inputString: string): string | undefined {

    // null, undefined will later be refused for conversion
    if (inputString == null || inputString == undefined) {
        return undefined;
    }

    //  U+ followed by 1.-6. hex digits will later be used for conversion
    const m_uni = /^U\+([0-9a-f]{1,6})$/i.exec(inputString);

    // invalid U+ ( U+ followed by anything) will later be refused for conversion
    const m_uni_inv = /^(U\+)+(.?)+$/i.exec(inputString);

    // &#x followed by 1.-6. hex digits will later be used for conversion
    const m_hex = /^&#x([0-9a-f]{1,6});$/i.exec(inputString);

    // &# followed by 1.-6. decimal digits will later be used for conversion
    const m_dec = /^&#([0-9]{1,7});$/.exec(inputString);

    // & followed by gt, lt, quot, amp, apos will later be used for conversion
    const m_nam = /^&(gt|lt|quot|amp|apos);$/i.exec(inputString);

    //  &# followed by anything will later be refused for conversion
    const m_html_inv = /^(&#)+(.?)+$/i.exec(inputString);

    // one or more characters except starting with U+ or & will later be used for conversion
    const m_chr = /^(?!U\+|&).+$/i.exec(inputString);

    // '&', '&#','&#x', or 'U+' with or without ; will later be refused for conversion
    const m_chr_inv = /^((&;?)+|(&#;?)+|(&#x;?)+|(U\+)+;?)$|^$/i.exec(inputString);

    // valid 'U+xxxx'
    if (m_uni) {
        const codePoint_u = parseInt(m_uni[1], 16);
        // Reject surrogates and invalid codepoints
        if ((codePoint_u >= 0xD800 && codePoint_u <= 0xDFFF) || codePoint_u > 0x10FFFF) {
            return undefined;
        }
        return String.fromCodePoint(codePoint_u);
    }

    // invalid 'U+xxxx'
   else if (m_uni_inv) {
        return undefined;
    }

    // valid '&#x...'
    else if (m_hex) {
        const codePoint_h = parseInt(m_hex[1], 16);
        // Reject surrogates and invalid codepoints
        if ((codePoint_h >= 0xD800 && codePoint_h <= 0xDFFF) || codePoint_h > 0x10FFFF) {
            return undefined;
        }
        return String.fromCodePoint(codePoint_h);
    }
    // valid '&#...'
    else if (m_dec) {
        const codePoint_d = parseInt(m_dec[1], 10);
        // Reject surrogates and invalid codepoints
        if ((codePoint_d >= 0xD800 && codePoint_d <= 0xDFFF) || codePoint_d > 0x10FFFF) {
            return undefined;
        }
        return String.fromCodePoint(codePoint_d);
    }
    // valid '&gt', '&lt',..
    else if (m_nam) {
        switch (m_nam[1].toLowerCase()) {
            case 'gt': return '>';
            case 'lt': return '<';
            case 'quot': return '"';
            case 'amp': return '&';
            case 'apos': return "'";
            default: return undefined;
        }
    }
    // invalid  '&...'
    else if (m_html_inv) {
        return undefined;
    }

    // single 'U+', '&', ''
    else if (m_chr_inv) {
        return inputString;
    }

    // if no matches so far, check for one or more characters ('a','ab', 'ẘ','😎', '😎😎',  )
    else if (m_chr) {
        return inputString;
    }
    return undefined;
}


/**
 * @brief  function to convert a numeric character reference to a unicode Codepoint e.g. &#x1234 -> U+1234; &#4660 -> U+1234;  &#x10F601 -> U+1F60E
 * @param  inputString the value that will converted
 * @return returns a unicode Codepoint like U+0063, U+1234, U+1F60E;
 *                 the input character if a Unicode Codepoint or valid input character is provided (e.g. 'c' -> 'c', '😎' -> '😎')
 *                 undefined if inputString is not valid, null or undefined, or a surrogate codepoint
 */
export function convertControlCharacterToUnicodeCodePoint(inputString: string): string | undefined {
    if ((inputString === null) || (inputString === undefined)) {
        return undefined;
    }

    //  U+ followed by 1.-6. hex digits will later be used for conversion
    const m_uni = /^U\+([0-9a-f]{1,6})$/i.exec(inputString);

    // invalid U+ ( U+ followed by anything) will later be refused for conversion
    const m_uni_inv = /^(U\+)+(.?)+$/i.exec(inputString);

    // &#x followed by 1.-6. hex digits will later be used for conversion
    const m_hex = /^&#x([0-9a-f]{1,6});$/i.exec(inputString);

    // &# followed by 1.-6. decimal digits will later be used for conversion
    const m_dec = /^&#([0-9]{1,7});$/.exec(inputString);

    // &# followed by anything will later be refused for conversion
    const m_html_inv = /^(&#)+(.?)+$/i.exec(inputString);

    // one or more characters except starting with U+ or & will later be used for conversion
    const m_chr = /^(?!U\+|&).+$/i.exec(inputString);

    // '&', '&#','&#x', or 'U+' with or without ; will later be refused for conversion
    const m_chr_inv = /^((&;?)+|(&#;?)+|(&#x;?)+|(U\+)+;?)$|^$/i.exec(inputString);

    // valid U+xxxx
    if (m_uni) {
        const codePoint_u = parseInt(m_uni[1], 16);
        // Reject surrogates and invalid codepoints
        if ((codePoint_u >= 0xD800 && codePoint_u <= 0xDFFF) || codePoint_u > 0x10FFFF) {
            return undefined;
        }
        return inputString;
    }

    // invalid 'U+xxxx'
    else if (m_uni_inv) {
        return undefined;
    }

    // valid '&#x...'
    else if (m_hex) {
        const codePoint_h = parseInt(m_hex[1], 16);
        // Reject surrogates and invalid codepoints
        if ((codePoint_h >= 0xD800 && codePoint_h <= 0xDFFF) || codePoint_h > 0x10FFFF) {
            return undefined;
        }
        return "U+" + m_hex[1].toUpperCase().padStart(4, "0");
    }

    // valid '&#...'
    else if (m_dec) {
        const codePoint_d = parseInt(m_dec[1], 10);
        // Reject surrogates and invalid codepoints
        if ((codePoint_d >= 0xD800 && codePoint_d <= 0xDFFF) || codePoint_d > 0x10FFFF) {
            return undefined;
        }
        return ("U+" + codePoint_d.toString(16).toUpperCase().padStart(4, "0"));
    }

    // invalid  '&...'
    else if (m_html_inv) {
        return undefined;
    }

    // single 'U+', '&', ''
    else if (m_chr_inv) {
        return inputString;
    }

    // no matches so far, check for one or more characters ('a','ab', 'ẘ','😎', '😎😎',  )
    else if (m_chr) {
        return inputString;
    }

    else {
        return undefined;
    }

}
