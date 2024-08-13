import * as fs from 'fs';
import * as rd from 'readline';

export default async function hextobin(inputFilename: string, outputFilename?: string, options?: {silent?: boolean}): Promise<Uint8Array> {

  options = {...options};
  options.silent = !!options.silent;

  let reader = rd.createInterface(fs.createReadStream(inputFilename));

  type HexBlockRefType =
    'sizeof' |    // gets the size of a block, optionally divided by divisor: sizeof(block,divisor)
    'offset' |    // gets the offset of a block in bytes from start of file: offset(block)
    'diff' |      // gets the offset of a block in bytes from start of a base block: offset(baseBlock,block)
    'index';      // gets the index of a block from a base block, optionally divided by divisor: index(baseBlock,block,divisor)

  interface HexBlockRef {
    type: HexBlockRefType;
    blockName: string;
    blockName2?: string; // used only by diff, index
    divisor?: number; // used only by sizeof, index
    offset: number; // actual byte offset in hex data (i.e. offset in string is * 2)
  };

  interface HexBlock {
    name: string;
    offset: number;   // calculated during reconciliation phase
    hex: string;
    refs: HexBlockRef[];
  };

  let blocks: HexBlock[] = [];

  let currentLine = '';
  let currentLineNumber = 0;

  if(!await load()) {
    return null;
  }

  if(!reconciliation()) {
    return null;
  }

  return save();

  function reportError(message: string) {
    if(!options.silent) {
      console.error(`Invalid input file: ${message} on line #${currentLineNumber}: "${currentLine}"`);
    }
  }

  function currentBlock(): HexBlock {
    return blocks.length ? blocks[blocks.length-1] : null;
  }

  function parseToken(token: string): { command: string, parameters: string[] } {
    let m = /^([a-z]+)\((.+)\)$/.exec(token);
    if(!m) {
      if(!token.match(/^[a-fA-F0-9]{2}$/)) {
        return null;
      }
      // hex byte
      return { command: 'data', parameters: [token] };
    }

    // processing command
    return { command: m[1], parameters: m[2].split(',') };
  }

  function token(token: string) {
    const t = parseToken(token);
    if(!t) {
      reportError(`expected command or hex at token "${token}"`);
      return false;
    }

    if(t.command == 'block') {
      blocks.push({name: t.parameters[0], offset: 0, hex: '', refs: []});
      return true;
    }

    const b = currentBlock();
    if(!b) {
      reportError(`expected block() before data`);
      return false;
    }

    switch(t.command) {
      case 'offset':
        b.refs.push({type: 'offset', blockName: t.parameters[0], offset: b.hex.length / 2});
        b.hex += "_".repeat(8); // always 4 bytes, placeholder will be filled in during reconciliation phase
        break;
      case 'sizeof':
        // sizeof can take a second parameter, divisor
        {
          let divisor = t.parameters.length > 1 ? parseInt(t.parameters[1],10) : 1;
          b.refs.push({type: 'sizeof', blockName: t.parameters[0], divisor: divisor, offset: b.hex.length / 2});
          b.hex += "_".repeat(8); // always 4 bytes, placeholder will be filled in during reconciliation phase
        }
        break;
      case 'diff':
        // diff can also take a divisor, defaults to 1
        {
          const divisor = t.parameters.length > 2 ? parseInt(t.parameters[2],10) : 1;
          b.refs.push({type: 'diff', blockName: t.parameters[0], blockName2: t.parameters[1], divisor: divisor, offset: b.hex.length / 2});
          b.hex += "_".repeat(8); // always 4 bytes, placeholder will be filled in during reconciliation phase
        }
        break;
      case 'index':
        // index can take a third parameter, divisor
        {
          let divisor = t.parameters.length > 2 ? parseInt(t.parameters[2],10) : 1;
          b.refs.push({type: 'index', blockName: t.parameters[0], blockName2: t.parameters[1], divisor: divisor, offset: b.hex.length / 2});
          b.hex += "_".repeat(8); // always 4 bytes, placeholder will be filled in during reconciliation phase
        }
        break;
      case 'data':
        b.hex += t.parameters[0];
        break;
      default:
        reportError(`unknown command ${t.command}`);
        return false;
    }
    return true;
  }

  async function load() {
    for await (const l of reader) {
      // Error reporting variables
      currentLine = l;
      currentLineNumber++;

      const tokens = l.split(/[ \t]+/);
      for(let t of tokens) {
        if(t.startsWith('#')) {
          // comment, ignore all subsequent tokens to EOL
          break;
        }
        if(t == '') {
          continue;
        }
        if(!token(t)) {
          return false;
        }
      }
    }
    return true;
  }

  function dwordLeToHex(v: number): string {
    // hacky but who cares
    let h = v.toString(16);
    h = "0".repeat(8-h.length) + h;
    return h.substring(6,8) + h.substring(4, 6) + h.substring(2, 4) + h.substring(0, 2);
  }

  function fillBlockPlaceholder(block: HexBlock, offset: number, value: number): void {
    const hexvalue = dwordLeToHex(value);
    block.hex = block.hex.substring(0, offset * 2) + hexvalue + block.hex.substring(offset * 2 + 8);
  }

  function reconciliation(): boolean {
    let offset = 0;

    // calculate block sizes
    for(let b of blocks) {
      b.offset = offset;
      offset += b.hex.length / 2;
    }

    // reconcile block offsets and sizes
    for(let b of blocks) {
      for(let r of b.refs) {
        const v = blocks.find(q => q.name == r.blockName);
        if(!v) {
          reportError(`Could not find block ${r.blockName} when reconciling ${b.name}`);
          return false;
        }
        switch(r.type) {
          case 'diff':
            {
              const v2 = blocks.find(q => q.name == r.blockName2);
              if(!v2) {
                reportError(`Could not find block ${r.blockName2} when reconciling ${b.name}`);
                return false;
              }
              fillBlockPlaceholder(b, r.offset, (v2.offset - v.offset) / r.divisor);
            }
            break;
          case 'offset':
            fillBlockPlaceholder(b, r.offset, v.offset);
            break;
          case 'sizeof':
            fillBlockPlaceholder(b, r.offset, v.hex.length / 2 / r.divisor);
            break;
          case 'index':
            {
              const v2 = blocks.find(q => q.name == r.blockName2);
              if(!v2) {
                reportError(`Could not find block ${r.blockName2} when reconciling ${b.name}`);
                return false;
              }
              fillBlockPlaceholder(b, r.offset, (blocks.indexOf(v2) - blocks.indexOf(v)) / r.divisor);
            }
            break;
          default:
            reportError(`Invalid ref ${r.type}`);
            return false;
        }
      }
    }
    return true;
  }

  function save(): Uint8Array {
    let total = blocks.reduce((total: number, item: HexBlock) => Math.max(item.offset + item.hex.length/2, total), 0);

    if(!options.silent) {
      console.log(`${currentLineNumber} lines read; ${blocks.length} sections to write. Total file size = ${total} bytes.` );
    }
    let buffer = new Uint8Array(total);
    blocks.forEach(item => {
      let buf = Buffer.from(item.hex, 'hex');
      buffer.set(buf, item.offset);
    });

    if(outputFilename) {
      fs.writeFileSync(outputFilename, buffer);
    }
    return buffer;
  }
}
