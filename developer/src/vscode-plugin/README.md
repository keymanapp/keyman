# Keyman Developer for VSCode

This package is a standalone [VSCode](https://code.visualstudio.com) plugin which offers:

- a Build Task for building .kpj files into a package
- when building the .kpj file, all .kmn and .xml (LDML keyboard) files will be compiled as well
- The Build Task assumes that the .kpj will have the same name as the directory.  So, `.../some_keyboard/some_keyboard.kpj`
- The build task will also build .kps files into packages

## Building

From the command line,

- `npm i`
- `npm run compile`

You can use `npm run watch` to keep compilation running, but see below.

There is also a `build.sh` which works the usual way.

## Running/Testing locally

- Open this directory (`vscode-plugin`) in VSCode
- From the Run and Debug toolbar, click Run Extension
- A terminal will automatically open with `npm run watch` recompiling the project.
- Choose a directory containing a .kpj file, and choose the **Terminal > Run Task…** menu item, or the **Tasks: Run Task** command.   Select "kpj" and the name, and the compilation should occur in a terminal window.

- You can use the **Developer: Reload Window** command to reload the `[extension development]` window with a new version of the plugin.

## License

Copyright (c) SIL Global.

Keyman is an open source project distributed under the [MIT license](https://github.com/keymanapp/keyman/blob/master/LICENSE.md).
