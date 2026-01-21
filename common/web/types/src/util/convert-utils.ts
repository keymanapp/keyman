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
 * @brief  function to convert a numeric character reference or a unicode value to a unicode character e.g. &#x63 -> c;  U+1F60E -> 😎
 * @param  inputString the value that will converted
 * @return a unicode character like 'c', 'ሴ', '😎' or undefined if inputString is not recognized
 */
export function convertToUnicodeCharacter(inputString: string): string | undefined {

    let m_uni = /^U\+([0-9a-f]{2,6})$/i.exec(inputString);      // U+  followed by 2.-6. hex digits
    let m_hex = /^&#x([0-9a-f]{2,6});$/i.exec(inputString);     // &#x followed by 2.-6. hex digits
    let m_dec = /^&#([0-9]{1,6});$/.exec(inputString);          // &#  followed by 1.-6. decimal digits
    let m_nam = /^&(gt|lt|quot|amp|apos);$/i.exec(inputString); // &  followed by 1gt, lt, quot, amp, apos

    // if null, undefined
    if (inputString == null || inputString == undefined) {
        return undefined;
    }
    // if empty string
    else if (inputString === "") {
        return "";
    }
    // if U+xxxx
    else if (m_uni) {
        return String.fromCodePoint(parseInt(m_uni[1], 16));
    }
    // else if &#x...
    else if (m_hex) {
        return String.fromCodePoint(parseInt(m_hex[1], 16));
    }
    // else if &#...
    else if (m_dec) {
        return String.fromCodePoint(parseInt(m_dec[1], 10));
    }
    // else if &gt, &lt,..
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
    // else if a character inside the unicode range ( a, ä, 😎, ሴ )
    // 'A'  or  "B" have length=1 and segment-length=1 and will be used.
    // "ẘ"  or  "😎" have length=2 but segment-length=1 and will be used.
    // "ab" has length=2 and segment-length=2 and will not be used.
    else if ((inputString.codePointAt(0) < 0x10FFFF) && ([...inputString].length === 1)) {
        return inputString;
    }
    // else if a character inside the unicode range with diacritic ( W̊9 )
    // 'A'  or  "B" have length=1 and segment-length=1 and will be used.
    // "ẘ"  or  "😎" have length=2 but segment-length=1 and will be used.
    // "ab" has length=2 and segment-length=2 and will not be used.
    else if ((inputString.codePointAt(0) < 0x10FFFF) && ([...inputString].length > 1) && ([...new Intl.Segmenter().segment(inputString)].length <= 1)) {
        return inputString;
    }
    return undefined;
}


/**
 * @brief  function to convert a numeric character reference to a unicode Code Point e.g. &#4660 -> U+1234;  &#x10F601 -> U+1F60E
 * @param  instr the value that will converted
 * @return returns a unicode Code Point like U+0063, U+1234, U+1F60E; returns the input character if a non-numeric reference is used or returns 'undefined' if instr is not recognized
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
