/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2025-04-03
 *
 * Abstraction for line number processing
 */

/** line number with an optional column number */
export interface LineColumn {
    line: number;
    column?: number;
}

export type LineList = number[];

/** a LineFinder is able to convert from an offset to a line/column */
export class LineFinder {
    private list: LineList;
    constructor(public text: string) {
        this.list = LineFinder.textToLines(text);
    }

    /**
     * Convert an offset into line:column
     * @param offset input offset into the text
     * @returns line:column information
     */
    public findOffset(offset: number): LineColumn {
        return LineFinder.offsetToLineColumn(offset, this.list);
    }

    /** 
     * preprocess text to turn it into arrays of line lengths.
     * This is effectively a 1-based line length, since line 0 has length of
     * 0.
     */
    public static textToLines(text: string): LineList {
        return [
            0, // "line 0" is empty
            ...text.replaceAll("\r\n", "\n").split("\n")
                .map(l => l.length + 1) // line length (counting the trailing newline)
        ];
    }

    /**
     * convert a line number array to a line/column
     */
    public static offsetToLineColumn(offset: number, list: LineList): LineColumn {
        for (let line = 1; line < list.length; line++) { // 1-based (assume the first row is 0)
            if (list[line] > offset) {
                return { line, column: offset };
            }
            offset = offset - (list[line]); // count newline at end
        }
        // default: line 0, error
        return { line: 0 }
    }
}


