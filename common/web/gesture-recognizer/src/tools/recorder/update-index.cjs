var fs = require('fs');
var os = require('os');
var child_process = require('child_process');

const args = process.argv.slice(2);

// args[0] - destination file path.  If not provided, the text will be dumped to the console.

fs.readFile('src/index.html', 'utf8', function(err, text) {
  if(err) {
    throw err;
  }

  // Runs the fixture-extractor script, receives the fixture's raw HTML.
  let buf = child_process.execSync("node extractor.cjs", {cwd: "../host-fixture", windowsHide: true, shell: true}).toString();

  let INSERTION_POINT = "            <!--INSERT_FIXTURE-->" + os.EOL;
  text = text.replace(INSERTION_POINT, buf);

  fs.writeFileSync(args[0], text);
});