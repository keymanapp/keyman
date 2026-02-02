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
 * @return the input character/Codepoint ,
 *         a converted character
 *         or undefined if inputString is null or undefined, half a surrogate pair, or not recognized
 */
export function convertToUnicodeCharacter(inputString: string): string | undefined {

    // if null, undefined will be refused for conversion
    if (inputString == null || inputString == undefined) {
        return undefined;
    }

    //  U+  followed by 1.-6. hex digits will be used for conversion
    let m_uni = /^U\+([0-9a-f]{1,6})$/i.exec(inputString);
    // matches also invalid U+ ( U+ followed by anything) will be refused for conversion
    let m_uni_inv = /^(U\+)+(.?)+$/i.exec(inputString);

    // &#x followed by 1.-6. hex digits will be used for conversion
    let m_hex = /^&#x([0-9a-f]{1,6});$/i.exec(inputString);
    // &#  followed by 1.-6. decimal digits will be used for conversion
    let m_dec = /^&#([0-9]{1,7});$/.exec(inputString);
    // &  followed by gt, lt, quot, amp, apos will be used for conversion
    let m_nam = /^&(gt|lt|quot|amp|apos);$/i.exec(inputString);
    // matches also invalid & ( & followed by anything) will be will be refused for conversion for conversion
    let m_html_inv = /^(&#)+(.?)+$/i.exec(inputString);

    // one or more characters except starting with U+ or & will be used for conversion
    let m_chr = /^(?!U\+|&).+$/i.exec(inputString);

    // '&', '&#','&#x', or 'U+' with or without ; will be refused for conversion
    let m_chr_inv = /^((&;?)+|(&#;?)+|(&#x;?)+|(U\+)+;?)$|^$/i.exec(inputString);

    // if valid 'U+xxxx'
    if (m_uni) {
        const codePoint_u = parseInt(m_uni[1], 16);
        if ((codePoint_u >= 0xD800 && codePoint_u <= 0xDFFF) || codePoint_u > 0x10FFFF) {
            return undefined;
        }
        return String.fromCodePoint(codePoint_u);
    }
    // invalid 'U+xxxx'
    if (m_uni_inv) {
        return undefined;
    }
    // else if valid '&#x...'
    else if (m_hex) {
        const codePoint_h = parseInt(m_hex[1], 16);
        if ((codePoint_h >= 0xD800 && codePoint_h <= 0xDFFF) || codePoint_h > 0x10FFFF) {
            return undefined;
        }
        return String.fromCodePoint(codePoint_h);
    }
    // else if valid '&#...'
    else if (m_dec) {
        const codePoint_d = parseInt(m_dec[1], 10);
        if ((codePoint_d >= 0xD800 && codePoint_d <= 0xDFFF) || codePoint_d > 0x10FFFF) {
            return undefined;
        }
        return String.fromCodePoint(codePoint_d);
    }
    // else if '&gt', '&lt',..
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
    if (m_html_inv) {
        return undefined;
    }
    // if single 'U+', '&', ''
    else if (m_chr_inv) {
        return inputString;
    }

    // if no matches so far, check for one or more characters ('a','ab', 'ẘ','😎', '😎😎',  )
    if (m_chr) {
        return inputString;
    }
    return undefined;
}


/**
 * @brief  function to convert a numeric character reference to a unicode Codepoint e.g. &#4660 -> U+1234;  &#x10F601 -> U+1F60E
 * @param  instr the value that will converted
 * @return returns a unicode Codepoint like U+0063, U+1234, U+1F60E; returns the input character if a non-numeric reference is used or returns 'undefined' if instr is not recognized
 */
export function convertToUnicodeCodePoint(instr: string): string {
    if ((instr === null) || (instr === undefined)) {
        return undefined;
    }

    if (instr.substring(0, 3) === "&#x") {
        const num_length = instr.length - instr.indexOf("x") - 1;
        const num_str = instr.substring(instr.indexOf("x") + 1, instr.length - 1);
        return ("U+" + num_str.slice(-num_length).padStart(4, "0"));
    }

    // if not hex: convert to hex
    if ((instr.substring(0, 2) === "&#")) {
        const num_length = instr.length - instr.indexOf("#") - 1;
        const num_str = instr.substring(instr.indexOf("#") + 1, instr.length - 1);
        return "U+" + Number(num_str.slice(-num_length)).toString(16).slice(-6).toUpperCase().padStart(4, "0");
    }
    else
        return instr;
}
