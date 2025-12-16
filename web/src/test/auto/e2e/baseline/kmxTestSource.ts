/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import * as fs from 'fs';
import * as path from 'path';

// Helper types
type KmxOptionType = 'input' | 'output' | 'saved';

interface KmxOption {
  type: KmxOptionType;
  key: string;
  value: string;
}

type KmxOptions = KmxOption[];

export class KmxTestSource {
  public description = '';
  public keys = '';
  public expected = '';
  public expectedContext = '';
  public context = '';
  public options: KmxOptions = [];
  public expectedBeep = false;
  private _capsLockOn = false;

  public constructor() {}

  // String trim functions
  private static ltrim(s: string): string {
    return s.replace(/^\s+/, '');
  }

  private static rtrim(s: string): string {
    return s.replace(/\s+$/, '');
  }

  private static trim(s: string): string {
    return KmxTestSource.ltrim(KmxTestSource.rtrim(s));
  }

  private parseSourceString(s: string): string {
    let t = '';
    for (let i = 0; i < s.length; i++) {
      if (s[i] === '\\') {
        i++;
        if (i >= s.length) throw new Error('Unexpected end of string after \\');
        if (s[i] === 'u' || s[i] === 'U') {
          i++;
          const hex = s.substring(i, i+8).match(/^[0-9a-fA-F]+/);
          if (!hex) throw new Error('Invalid unicode escape');
          const v = parseInt(hex[0], 16);
          if (v < 0x0001 || v > 0x10FFFF) throw new Error('Unicode out of range');
          i += hex[0].length - 1;
          t += String.fromCodePoint(v);
        } else if (s[i] === 'd') {
          throw new Error('Deadkey not supported');
        }
      } else {
        t += s[i];
      }
    }
    return t;
  }

  private parseOptionString(line: string, options: KmxOptions, type: KmxOptionType): boolean {
    const x = line.indexOf('=');
    if (x === -1) return false;
    const key = this.parseSourceString(line.substring(0, x));
    const value = this.parseSourceString(line.substring(x + 1));
    options.push({ type, key, value });
    return true;
  }

  private isToken(token: string, lineObj: { line: string }): boolean {
    const {line} = lineObj;
    if (line.startsWith(token)) {
      lineObj.line = KmxTestSource.trim(line.substring(token.length));
      return true;
    }
    return false;
  }

  public loadSource(filePath: string): number{
    let lines: string[];
    try {
      lines = fs.readFileSync(filePath, 'utf-8').split(/\r?\n/);
    } catch (e) {
      console.error(`could not open file: ${filePath}`);
      return -1;
    }

    let lineNo = 0;
    for (const rawLine of lines) {
      lineNo++;
      const lineObj = { line: KmxTestSource.trim(rawLine) };
      if (!lineObj.line.length) continue;


      if (lineObj.line.startsWith('c Description:')) {
        this.description = KmxTestSource.trim(lineObj.line.substring('c Description:'.length));
      }  else if (lineObj.line.startsWith('c keys:')) {
        this.keys = KmxTestSource.trim(lineObj.line.substring('c keys:'.length));
      } else if (this.isToken('c expected:', lineObj)) {
        if (lineObj.line === '\\b') {
          this.expectedBeep = true;
        } else {
          this.expected = this.parseSourceString(lineObj.line);
        }
      } else if (this.isToken('c expected context:', lineObj)) {
        this.expectedContext = this.parseSourceString(lineObj.line);
      } else if (this.isToken('c context:', lineObj)) {
        this.context = this.parseSourceString(lineObj.line);
      } else if (this.isToken('c option:', lineObj)) {
        if (!this.parseOptionString(lineObj.line, this.options, 'input')) {
          return lineNo;
        }
      } else if (this.isToken('c expected option:', lineObj)) {
        if (!this.parseOptionString(lineObj.line, this.options, 'output')) {
          return lineNo;
        }
      } else if (this.isToken('c saved option:', lineObj)) {
        if (!this.parseOptionString(lineObj.line, this.options, 'saved')) {
          return lineNo;
        }
      } else if (this.isToken('c capsLock:', lineObj)) {
        this._capsLockOn = this.parseSourceString(lineObj.line) === '1';
      }
    }

    if (!this.keys) {
      return lineNo;
    }

    return 0;
  }
}
