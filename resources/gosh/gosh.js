#!/usr/bin/env node

/*
 gosh.js: launch bash script from any environment from within our package.json builds

 This script will try and locate the bash.exe executable that is included in Git for Windows
 on Windows platforms, and execute it within the current console. On *nix environments, it
 will simply launch /bin/bash.

 Usage:

 Make sure gosh is installed, e.g.:

     npm install '@keymanapp/resources-gosh@*'

 Then, in package.json, define scripts like this:

     "scripts": {
       "foo": "gosh ./foo.sh ..."
     }

 For example, in web/src/engine/common/utils/package.json, npm run build is defined as:

     "build": "gosh ./build.sh -skip-package-install"

 Windows launch strategy:

 * If GIT_BASH_FOR_KEYMAN environment variable is defined, then this script will parse the
   variable to extract the bash executable, and then use the remaining components of the
   variable to launch the script with its parameters.

 * If GIT_BASH_FOR_KEYMAN is *not* defined, then the script looks in the Program Files
   and Program Files (x86) folders, Git/bin/bash.exe, (using the %ProgramFiles% and
   %ProgramFiles(x86)% environment variables) and appends the parameters:
     --init-file .../Git/etc/profile -l <script> <parameters>
*/
let fs = require('fs');
let path = require('path');
let child_process = require('child_process');

// https://stackoverflow.com/a/5767589/1836776
const args = process.argv.slice(2);

const result = process.platform == 'win32' ? launchWin32Bash() : launchNixBash();

if(!result || result.status === null) {
  console.error('Subcommand failed with null exit code');
  process.exit(1);
}

process.exit(result.status);


function splitQuotedString(s) {
  // https://stackoverflow.com/a/18647776/1836776
  // The parenthesis in the regex creates a captured group within the quotes
  const regex = /[^\s"]+|"([^"]*)"/gi;
  const res = [];

  let match = null;
  do {
      //Each call to exec returns the next regex match as an array
      match = regex.exec(s);
      if (match != null)
      {
          //Index 1 in the array is the captured group if it exists
          //Index 0 is the matched text, which we use if no captured group exists
          res.push(match[1] ? match[1] : match[0]);
      }
  } while (match != null);

  return res;
}

function launchWin32Bash() {
  // First, look for GIT_BASH_FOR_KEYMAN variable
  let bash = null, child_args = null;
  if(process.env.GIT_BASH_FOR_KEYMAN) {
    const env = splitQuotedString(process.env.GIT_BASH_FOR_KEYMAN);
    if(!env) {
      console.error('Could not parse GIT_BASH_FOR_KEYMAN');
      process.exit(1);
    }

    bash = env[0];
    child_args = env.slice(1);
  } else {
    // Otherwise, find bash.exe wherever we can!
    const root = [process.env.ProgramFiles, process.env['ProgramFiles(x86)']].find(
      r => fs.existsSync(path.join(r, 'Git', 'bin', 'bash.exe'))
    );

    if(!root) {
      console.error('Could not find bash.exe');
      process.exit(1);
    }

    bash = path.join(root, 'Git', 'bin', 'bash.exe');
    child_args = [
      '--init-file',
      path.join(root, 'Git', 'etc', 'profile'),
      '-l'
    ];
  }

  child_args = child_args.concat(args);
  return child_process.spawnSync(bash, child_args, {shell: false, stdio:'inherit', windowsVerbatimArguments: false});
}

function launchNixBash() {
  const child_args = args.slice(1);
  return child_process.spawnSync(args[0], child_args, {shell: '/bin/bash', stdio:'inherit'});
}
