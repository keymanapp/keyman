import * as fs from 'fs';
import * as path from 'path';

// TODO: support multiple infiles to generate from multiple headers

if(process.argv.length < 4) {
  console.error('Invalid parameters');
  console.log('Usage: node index.js <srcfile> <dstPath>');
  process.exit(1);
}

const src = process.argv[2];
const dst = process.argv[3];

interface ApiEndpoint {
  filename: string;
  type: string;
  endpoint: string;
};

interface DocFile {
  filename: string;
  lines: string[];
};

const lines = fs.readFileSync(src, 'utf-8').replace(/\r/g, '').split('\n');
let result: string[] = [], apiEndpoints: ApiEndpoint[] = [];
let files: DocFile[] = [];

let filename: string = null;

type ProcessorMode = 'preamble' | 'filename' | 'title' | 'content' | 'eof';

let mode: ProcessorMode = 'preamble';

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

    filename = line.split(':', 2)[1].trim();
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

    if(line.trim() == '/*' || line.trim() == '*/') {
      // TODO: is this safe?
      return;
    }

    if(line.trim() == '```c */') {
      result.push('```c');
      return;
    }

    const m = line.match(/^(#+) (km_core_[a-z0-9_]+)(\(\)| .+)$/i);
    if(m) {
      // API entry, need to capture as a link ref
      apiEndpoints.push({filename, endpoint: m[2], type: m[3] == '()' ? 'function' : m[3].trim()});
      // Rewrite the heading to include an anchor
      line = `${m[1]} ${m[2]}${m[3]} {#${m[2]}}`;
    }

    result.push(line);
  },

  eof: (line: string) => {
  }
}

function cacheResult() {
  if(filename != null && result.length > 1) {
    files.push({
      filename,
      lines: result
    });
  }
}

for(const line of lines) {
  // console.log(mode+'\t'+line);
  processor[mode](line);
}

cacheResult();

console.dir(apiEndpoints);

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