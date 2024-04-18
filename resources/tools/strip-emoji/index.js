
const emojiRegex = require('emoji-regex');

process.stdin.setEncoding('utf-8');

// We will concatenate all strings and assume we are not processing a huge file,
// so we don't break in the middle of a UTF-8 sequence or split emoji sequences
// in half.

let stream = '';
process.stdin.on('readable', () => {
  const data = process.stdin.read();
  if(data) {
    stream += data;
  } else {
    process.stdout.write(stream.replaceAll(emojiRegex(), ''));
  }
});