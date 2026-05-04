/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Low-friction tool to extract javadoc-style comments from Keyman API headers.
 * Could be replaced in future with doxygen/moxygen type of toolchain, but
 * for now this covers our needs with minimal fuss.
 *
 * Notes:
 *  - `@name` command is required for each documented item
 *  - `@param` and `@returns` are the only other supported commands
 *  - this is fairly naive so check outputs to make sure they are
 *    giving what you expect!
 */

import * as fs from 'node:fs';
import * as path from 'node:path';

interface ApiEndpoint {
  filename: string;
  type: string;
  endpoint: string;
};

interface DocFile {
  filename: string;
  lines: string[];
};

type ProcessorMode = keyof typeof processor;

let mode: ProcessorMode;
let currentFilename: string | undefined;
let lines: string[], result: string[], apiEndpoints: ApiEndpoint[], files: DocFile[];

const processor = {
  preamble: (line: string) => {
    if(line.trim() == '---') {
      mode = 'filename';
    }
  },

  filename: (line: string) => {
    if(!line.trim().startsWith('filename:')) {
      throw new Error(`Expecting line to start with "filename:" but instead got "${line}"`);
    }

    cacheResult();

    currentFilename = line.split(':', 2)[1].trim();
    mode = 'title';
    result = [
      '---' // first line of markdown title section
    ];
  },

  title: (line: string) => {
    if(line.trim() == '---') {
      mode = 'content';
    }
    result.push(line);
  },

  content: (line: string) => {
    if(line.trim() == '---') {
      mode = 'filename';
      return;
    }

    if(line.trim() == '$EOF') {
      mode = 'eof';
      cacheResult();
      return;
    }

    if(line.trim() == '/**') {
      // This is start of a new function definition
      cacheLastFunction();
      mode = 'functionComment';
      return;
    }

    if(line.trim().startsWith('//')) {
      // ignore inline comments
      return;
    }

    if(line.trim() == '/*' || line.trim() == '*/') {
      // TODO: is this safe?
      return;
    }

    // if(line.trim() == '```c */') {
    //   result.push('```c');
    //   return;
    // }

    // const m = line.match(/^(#+) (km_core_[a-z0-9_]+)(\(\)| .+)$/i);
    // if(m) {
    //   // API entry, need to capture as a link ref
    //   apiEndpoints.push({filename: currentFilename, endpoint: m[2], type: m[3] == '()' ? 'function' : m[3].trim()});
    //   // Rewrite the heading to include an anchor
    //   line = `${m[1]} ${m[2]}${m[3]} {#${m[2]}}`;
    // }

    result.push(line);
  },

  functionComment: (line: string) => {
    line = line.trim();
    if(isCommentFinisher(line)) return;
    const { command, param } = getCommentCommand(line);
    if(!command) {
      // TODO: do we need to bifurcate further?
      result.push(param);
      return;
    }

    if(command == 'name') {
      const m = param.match(/^([^ ]+)\s*(.*)$/);
      if(!m) {
        console.log('Warning: misformatted @name line');
        return;
      }
      const name=m[1], title=`${m[1]} ${m[2]}`.trim();
      result.push('');
      result.push(`# ${title} {#${name}}`);
       apiEndpoints.push({filename: currentFilename, endpoint: name, type: m[2].trim()});
    } else if(command == 'param') {
      result.push('');
      result.push(`## Parameters`);
      mode = 'functionCommentParam';
      processor.functionCommentParam('* @param '+param);
    } else if(command == 'returns') {
      result.push('');
      result.push(`## Returns`);
      mode = 'functionCommentReturns';
      processor.functionCommentReturns('* '+param);
    }
  },

  functionCommentParam: (line: string) => {
    line = line.trim();
    if(isCommentFinisher(line)) return;
    const { command, param } = getCommentCommand(line);
    if(command != 'param' && command != null) {
      mode = 'functionComment';
      return processor.functionComment(line);
    }
    if(command == 'param') {
      // expect @param <name> [desc]
      const m = param.match(/^([^ ]+)\s*(.*)$/);
      if(!m) {
        console.log('Warning: misformatted @param line');
        return;
      }
      const name=m[1], desc=m[2];
      result.push('');
      result.push(`### ${name}`);
      result.push(desc);
    } else {
      // continuation of `param`
      result.push(param);
    }
  },

  functionCommentReturns: (line: string) => {
    line = line.trim();
    if(isCommentFinisher(line)) return;
    const { command, param } = getCommentCommand(line);
    if(command != null) {
      mode = 'functionComment';
      return processor.functionComment(line);
    }
    // continuation of `returns`
    result.push(param);
  },

  declaration: (line: string) => {
    if(line.trim().startsWith('//')) {
      // Ignore internal comments
      return;
    }

    if(line.trim() == '/**') {
      // This is start of a new function definition
      result.push('```');
      cacheLastFunction();
      mode = 'functionComment';
      return;
    }

    if(line.trim() == '$EOF') {
      mode = 'eof';
      cacheResult();
      return;
    }

    if(line.trim() != '') {
      result.push(line);
    }
  },

  eof: (line: string) => {
  }
}

function getCommentCommand(line: string) {
  const m = line.match(/^\s*\*\s+@(name|param|returns)(\s+.*)?$/);
  if(!m) {
    return { command: null, param: line.substring(2).trim() }; // strips initial '* '
  }
  return {
    command: m[1],
    param: (m[2] ?? '').trim()
  }
}

function isCommentFinisher(line: string) {
  if(line.endsWith('*/')) {
    mode = 'declaration';
    result.push('');
    result.push('```c');
    return true;
  }

  if(line == '---') {
    // Starting a new file
    mode = 'filename';
    return true;
  }

  if(!line.startsWith('*')) {
    // TODO: log a warning
    return true;
  }

  return false;
}

function cacheLastFunction() {
  // TODO
}

function cacheResult() {
  if(currentFilename != undefined && result.length > 1) {
    files.push({
      filename: currentFilename,
      lines: result
    });
  }
}

//---------------------------------------------------------------------------------------------------
// main()
//---------------------------------------------------------------------------------------------------

if(process.argv.length < 4) {
  console.error('Invalid parameters');
  console.log('Usage: node index.js <dstPath> <srcFile...>');
  process.exit(1);
}

const dst = process.argv[2];
const src = process.argv.slice(3);

apiEndpoints = [];
files = [];

for(const s of src) {
  result = [];
  currentFilename = undefined;
  mode = 'preamble';
  lines = fs.readFileSync(s, 'utf-8').replace(/\r/g, '').split('\n');

  for(const line of lines) {
    // console.log(mode+'\t'+line);
    processor[mode](line);
  }

  cacheResult();
}

// console.dir(apiEndpoints);

// Build heading references

const refs = buildRefs();

// Write out

for(const file of files) {
  writeFile(file, refs);
}

function writeFile(file: DocFile, refs: string) {
  fs.writeFileSync(path.join(dst, file.filename), file.lines.join('\n') + '\n\n' + refs, 'utf-8');
}

function buildRefs() {
  let result = [];
  for(let ep of apiEndpoints) {
    const filename = ep.filename.substring(0,ep.filename.length-3); // Strip .md, rough but ready
    result.push(`[${ep.endpoint}]: ${filename}#${ep.endpoint} "${ep.endpoint} ${ep.type}"`);
  }
  return result.join('\n');
}