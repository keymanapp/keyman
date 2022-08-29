import * as fs from 'fs';
import * as rd from 'readline';

export default async function hextobin(inputFilename: string, outputFilename?: string, options?: {silent?: boolean}): Promise<Uint8Array> {

  options = {...options};
  options.silent = !!options.silent;

  let reader = rd.createInterface(fs.createReadStream(inputFilename));

  interface HexBlock {
    offset: number;
    hex: string;
  };

  let data: HexBlock[] = [];

  let lineNumber = 0;

  for await (const l of reader) {
    lineNumber++;
    let tokens = l.split(/[ \t]+/);

    while(tokens.length && tokens[0] == '') {
      tokens.shift();
    }

    if(tokens.length == 0) {
      continue;
    }

    if(tokens[0].endsWith(':')) {
      let f = tokens.shift();
      if(!f) {
        continue;
      }
      data.push({offset: parseInt(f), hex:''});
    }

    if(data.length == 0) {
      // no offset given, starting at 0
      data.push({offset: 0, hex: ''});
    }

    for(let token of tokens) {
      if(token == '') {
        continue;
      }
      if(token.startsWith('#')) {
        break;
      }
      if(!token.match(/^[a-fA-F0-9]{2}$/)) {
        if(!options.silent) {
          console.error(`Invalid input file: expected hex for component "${token}" on line #${lineNumber}: "${l}"`);
        }
        return null;
      }
      data[data.length-1].hex += token;
    }
  }

  let total = data.reduce((total: number, item: HexBlock) => Math.max(item.offset + item.hex.length/2, total), 0);

  if(!options.silent) {
    console.log(`${lineNumber} lines read; ${data.length} sections to write. Total file size = ${total} bytes.` );
  }
  let buffer = new Uint8Array(total);
  data.forEach(item => {
    let buf = Buffer.from(item.hex, 'hex');
    buffer.set(buf, item.offset);
  });

  if(outputFilename) {
    fs.writeFileSync(outputFilename, buffer);
  }
  return buffer;
}