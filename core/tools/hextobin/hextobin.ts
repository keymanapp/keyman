import * as fs from 'fs';
import * as rd from 'readline';
import * as program from 'commander';

let inputFilename: string = "";
let outputFilename: string = "";

program
  .description(
`Will convert the input file which is a hex dump to the output file. Hex dump can contain
blank lines, offsets and comments. File writing will start at zero if no offset given;
any blank spaces in the file will be filled with nul bytes.

0x1234: 11 22 33 44 55 aa bb     # This is a comment
# offset(0xhex|dec): hex bytes            # comment`
  )
  .arguments('<infile>')
  .arguments('<outfile>')
  .action((infile:any, outfile:any) => { inputFilename = infile; outputFilename = outfile; });

program.parse(process.argv);

if(!inputFilename || !outputFilename) {
  exitDueToUsageError('Must provide an input and output filename.');
}

function exitDueToUsageError(message: string): never  {
  console.error(`${program._name}: ${message}`);
  console.error();
  program.outputHelp();
  return process.exit(64); // SysExits.EX_USAGE
}


let reader = rd.createInterface(fs.createReadStream(inputFilename));

interface HexBlock {
  offset: number;
  hex: string;
};

let data: HexBlock[] = [];

let lineNumber = 0;

reader.on("line", (l: string) => {
  lineNumber++;
  let tokens = l.split(/[ \t]+/);

  console.log(`${lineNumber}: ${tokens}`);

  while(tokens.length && tokens[0] == '') {
    tokens.shift();
  }

  if(tokens.length == 0) {
    return;
  }

  if(tokens[0].endsWith(':')) {
    let f = tokens.shift();
    if(!f) return;
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
      return;
    }
    if(!token.match(/^[a-fA-F0-9]{2}$/)) {
      console.error(`Invalid input file: expected hex for component "${token}" on line #${lineNumber}: "${l}"`);
      process.exit(1);
    }
    data[data.length-1].hex += token;
  }
});


reader.on("close", ()=> {
  let total = data.reduce((total: number, item: HexBlock) => Math.max(item.offset + item.hex.length/2, total), 0);
  console.log(`${lineNumber} lines read; ${data.length} sections to write. Total file size = ${total} bytes.` );
  let buffer = new Uint8Array(total);
  data.forEach(item => {
    let buf = Buffer.from(item.hex, 'hex');
    buffer.set(buf, item.offset);
  });

  fs.writeFileSync(outputFilename, buffer);
});