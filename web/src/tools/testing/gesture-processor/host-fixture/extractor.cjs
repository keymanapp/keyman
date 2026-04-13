var fs = require('fs');
var os = require('os');

const args = process.argv.slice(2);

// args[0] - destination file path.  If not provided, the text will be dumped to the console.

fs.readFile('host-fixture.html', 'utf8', function(err, text) {
  if(err) {
    throw err;
  }

  let FIXTURE_START = "    <!-- START: recognizer host fixture -->" + os.EOL;
  let FIXTURE_END   = "    <!-- END: recognizer host fixture -->" + os.EOL;

  let startIndex = text.indexOf(FIXTURE_START);
  let endIndex = text.indexOf(FIXTURE_END) + FIXTURE_END.length;

  let desiredText = text.substring(startIndex, endIndex);

  if(args[0]) {
    fs.writeFileSync(args[0], desiredText);
  } else {
    console.log(desiredText);
  }
});