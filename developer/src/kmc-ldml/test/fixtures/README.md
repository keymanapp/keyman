# Fixtures for KMX+ / LDML format files and kmc-ldml compiler

## basic-xx.txt fixtures

The set of files named basic-xx.txt describe the expected output of running kmc
against basic.xml with version xx schema and target version. They are used in
the end-to-end test test-compiler-e2e.ts.

Any changes to the compiler or basic.xml will likely result in changes to the
compiled file. Structural differences should be updated manually in the txt
files to ensure that we are getting the expected result for the e2e test. The
following may be helpful for working with these files when updating the binary
format:

  ```sh
  cd developer/src/kmc
  ./build.sh configure build # if needed
  ./build.sh build-fixtures
  ```

This will compile both the .xml and the .txt to build/test/fixtures.

For the format of the files, see “hextobin.ts”.

P.S. surprisingly, the Dart language highlighter in VSCode does a helpful job on these files.
