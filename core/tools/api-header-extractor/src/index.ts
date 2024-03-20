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

const lines = fs.readFileSync(src, 'utf-8').split('\n');
let result: string[] = [];

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

    writeResult();

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
      writeResult();
      return;
    }

    if(line.trim() == '/*' || line.trim() == '*/') {
      // TODO: is this safe?
      return;
    }

    if(line.trim() == '```c */') {
      result.push('```c');
    } else {
      result.push(line);
    }
  },

  eof: (line: string) => {
  }
}

function writeResult() {
  if(filename != null && result.length > 1) {
    fs.writeFileSync(path.join(dst, filename), result.join('\n'), 'utf-8');
  }
}

for(const line of lines) {
  // console.log(mode+'\t'+line);
  processor[mode](line);
}

writeResult();
