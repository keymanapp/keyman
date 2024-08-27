
export function escapeMarkdownChar(s: string, whiteSpace: boolean) {
  // note: could replace with a common lib but too much baggage to be worth it for now
  // commonmark 2.4: all punct can be escaped
  // const punct = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~';
  // Per ParsedownExtra https://github.com/erusev/parsedown/blob/f5aa6fd1caf5ffb41af2e85d3ef1a51263a31545/Parsedown.php#L1964
  // '\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '>', '#', '+', '-', '.', '!', '|', '~'
  // and add < for start of HTML tag (hyphen moved to end to prevent range match!)
  s = s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  s = s.replace(/[\\`*_{}\[\]()#+.!|~-]/g, '\\$&');
  // Per commonmark: s = s.replace(/[!"#$%&'()*+,-./:;<=>?@\[\\\]^_`{|}~]/g, '\\$&');
  if(whiteSpace) {
    // replace whitepsace
    s = s.replace(/[\n]/g, '\\n');
    s = s.replace(/[\r]/g, '\\r');
    s = s.replace(/[\t]/g, '\\t');
    s = s.replace(/ /g, '&#x20;');
    s = s.replace(/\u00a0/g, '&#xa0;');
  }
  return s;
}
