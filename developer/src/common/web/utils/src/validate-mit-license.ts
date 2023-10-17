/**
 * Returns an error string for license issues, or null if no issues found
 * @param license
 * @returns
 */
export function validateMITLicense(license: string) {
  // NOTE: this function returns error messages which will not be easily
  // localizable. Given this is only for use with the keyboards and
  // lexical-models repositories, suggest we don't worry about this

  // split input text into paragraphs, trimming whitespace
  const text = license.trim().split('\n').map(line => line.trim()).join('\n').split(/\n\s*\n/);

  // MIT license, cleanup whitespace
  const clauses = [
    `The MIT License (MIT)`,

    `Copyright ...`, // Copyright is tested separately below

    `Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:`,

    `The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.`,

    `THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.`
  ]
  .map(clause => clause.split('\n').map(line => line.trim()).join(' '));

  if(text.length < clauses.length) {
    // not enough clauses in the file
    return 'License is missing clauses from MIT license';
  }

  if(text.length > clauses.length) {
    // too many clauses in the file
    return 'License contains extra text';
  }

  for(let i = 0; i < clauses.length; i++) {
    if(i == 1) {
      // Clause 1 is the only one that can differ, and just with
      // a copyright holder
      if(!text[i].match(/^Copyright/)) {
        // No Copyright clause
        return `Clause 2 does not start with 'Copyright'`;
      }
    } else {
      const t0 = text[i].replaceAll(/\s+/g, ' ').trim();
      const c0 = clauses[i].replaceAll(/\s+/g, ' ').trim();
      if(t0 != c0) {
        // Clause does not match
        return `Clause ${i+1} differs from MIT license`;
      }
    }
  }

  return null;
}
