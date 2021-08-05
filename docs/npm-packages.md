# npm packages

This is a guide for how the various Keyman `npm` packages are developed
and published.

`npm` packages are published on the CI (continuous integration) server.
Currently, we're using TeamCity for this.

Packages are **never** published from a developer's machine.

## List of current npm packages

* `@keymanapp/models-types` -- located at `common/models-types`
* `@keymanapp/lexical-model-compiler` -- located at `developer/js`

## For CI developers

### In general

Before publishing, a package must be **built** and **tested** on the CI
server.

This is typically done with:

```bash
./build.sh -test
```

Once the build succeeds and the tests pass, the version is set in
`package.json`:

```bash
./build.sh -version 12.0.${BUILD_NUMBER} -tier ${tier}
```

Now you can publish! Use the following script for this (again, **only on the CI server**!)

```bash
./build.sh -publish-to-npm -tier ${tier}
```

It is then uploaded to the npm package directory. **Ensure that the
compiled sources are included in the tarball**. For example,

```bash
$ npm publish
npm notice 📦  @keymanapp/lexical-model-compiler@0.0.0
npm notice === Tarball Contents ===
npm notice 12.4kB dist/lexical-model-compiler/build-trie.js
npm notice 1.2kB  dist/kmlmc.js
npm notice 3.0kB  dist/kmlmi.js
npm notice 1.4kB  dist/kmlmp.js
npm notice 5.0kB  dist/package-compiler/kmp-compiler.js
npm notice 55B    dist/package-compiler/kmp-json-file.js
npm notice 552B   dist/package-compiler/kps-file.js
npm notice 4.6kB  dist/lexical-model-compiler/lexical-model-compiler.js
npm notice 315B   dist/lexical-model-compiler/lexical-model.js
npm notice 158B   dist/util/min-keyman-version.js
npm notice 4.5kB  dist/model-info-compiler/model-info-compiler.js
npm notice 57B    dist/lexical-model-compiler/model-info-file.js
npm notice 341B   dist/lexical-model-compiler/stub.js
npm notice 115B   dist/util/sysexits.js
npm notice 2.1kB  dist/util/util.js
npm notice 1.4kB  package.json
npm notice 1.0kB  README.md
```

For every release of **Keyman Developer**, the CI publishes a release of
`@keymanapp/lexical-model-compiler`. The version number is locked with
the particular version of Keyman Developer.

### `set_npm_version`

This helper function sets the version in the `package.json` for the
given package. It must be given a three-part version number, with an
optional tier, "alpha" or "beta". No tier indicates "stable".

```bash
. $KEYMAN_REPOSITORY_ROOT/resources/shellHelperFunctions.sh

set_npm_version MAJOR.MINOR.PATCH[-alpha|-beta]
```
