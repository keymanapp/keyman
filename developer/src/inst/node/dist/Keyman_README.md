# Bundled node.js

We are bundling node 20.16.1 here as of Keyman 18.0.156-alpha

Note that if we change the node version, we will need to
rebuild the binary modules in /developer/src/server/src/win32/
to match the NAPI version.

# Updating the node version

1. Open https://nodejs.org/en/download/
2. Download Windows Binary (.zip), 32-bit.
3. Extract the zip, and copy the following files from it into this folder:
   * node.exe
   * LICENSE
   * CHANGELOG.md
   * README.md
