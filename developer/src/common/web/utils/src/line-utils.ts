/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by srl on 2025-04-03
 *
 * Abstraction for line number processing
 */

import { CompilerEvent, EventResolver } from "./index.js";

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
    }

    public getLineList(): LineList {
        if (!this.list) {
            this.list = LineFinder.textToLines(this.text);
        }
        return this.list;
    }

    /**
     * Convert an offset into line:column
     * @param offset input offset into the text
     * @returns line:column information
     */
    public findOffset(offset: number): LineColumn {
        return LineFinder.offsetToLineColumn(offset, this.getLineList());
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

/**
 * Interface for a class which can receive file contents,
 * keyed by the filename
 */
export interface FileConsumer {
    /**
     * @param filename name of the file
     * @param contents string contents of the file
     */
    addFile(filename: string, contents: string): void;
}

/** Cache of LineFinder elements, organized by filename */
export class LineFinderCache implements FileConsumer {
    contentsCache: Map<string, LineFinder> = new Map();

    /** add or update a source file */
    addFile(filename: string, contents: string): void {
        this.contentsCache.set(filename, new LineFinder(contents));
    }

    getByFilename(filename: string) : LineFinder | undefined {
        const lf = this.contentsCache.get(filename);
        return lf;
    }
}
export class LineFinderEventResolver implements EventResolver, FileConsumer {
  private lfcache = new LineFinderCache();

  addFile(filename: string, contents: string): void {
    this.lfcache.addFile(filename, contents);
  }

  /** resolve a CompilerEvent by expanding the line numbers */
  resolve(event: CompilerEvent) {
    if (event.offset && !event.line && event.filename) {
      const lf = this.lfcache.getByFilename(event.filename);
      const loc = lf.findOffset(event.offset);
      event.line = loc.line;
      event.column = loc.column;
    }
    return event;
  }
}
