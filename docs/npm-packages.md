# npm packages

This is a guide for how the various Keyman `npm` packages are developed
and published.

`npm` packages are published through CI (continuous integration). This is done
through the `.github/workflows/npm-publish.yml` GitHub Action.

Packages are **never** published from a developer's machine.

## List of current published npm packages

* See `resources/build/ci/npm-packages.inc.sh` for an authoritative list.

* `@keymanapp/common-types` -- located at `common/web/types`
* `@keymanapp/keyman-version` -- located at `common/web/keyman-version`
* `@keymanapp/kmc` -- located at `developer/src/kmc`
* `@keymanapp/kmc-analyze` -- located at `developer/src/kmc-analyze`
* `@keymanapp/kmc-copy` -- located at `developer/src/kmc-copy`
* `@keymanapp/kmc-generate` -- located at `developer/src/kmc-generate`
* `@keymanapp/kmc-keyboard-info` -- located at `developer/src/kmc-keyboard-unfo`
* `@keymanapp/kmc-kmn` -- located at `developer/src/kmc-kmn`
* `@keymanapp/kmc-ldml` -- located at `developer/src/kmc-ldml`
* `@keymanapp/kmc-model` -- located at `developer/src/kmc-model`
* `@keymanapp/kmc-model-info` -- located at `developer/src/kmc-model-info`
* `@keymanapp/kmc-package` -- located at `developer/src/kmc-package`
* `@keymanapp/langtags` -- located at `common/web/langtags`
* `@keymanapp/ldml-keyboard-constants` -- located at `core/include/dml`

### Deprecated npm packages

* `@keymanapp/developer-lexical-model-compiler` -- replaced by `@keymanapp/kmc`
* `@keymanapp/lexical-model-compiler` -- replaced by `@keymanapp/kmc`
* `@keymanapp/lexical-model-types` -- replaced by `@keymanapp/common-types`
* `@keymanapp/models-types` -- replaced by `@keymanapp/common-types`
* `@keymanapp/lmlayer-cli` -- replaced by `@keymanapp/kmc`
* `@keymanapp/models-wordbreakers` -- published accidentally

## For CI developers

### In general

Before publishing, a package must be **built** and **tested**. This is typically
done with:

```bash
./build.sh configure build test
```

Once the build succeeds and the tests pass, you can publish! Add the package to
`resources/build/ci/npm-packages.inc.sh` and it will be published in the next
alpha release build.

It is then uploaded to the npm package directory. **Ensure that the compiled
sources are included in the tarball**. For example:

```bash
$ ./build.sh publish
npm notice
npm notice 📦  @keymanapp/kmc@16.0.61-alpha-local
npm notice === Tarball Contents ===
npm notice 841B   Makefile
npm notice 2.8kB  README.md
npm notice 2.5kB  build.sh
npm notice 116B   build/src/kmc.d.ts
npm notice 114B   build/src/kmc.d.ts.map
npm notice 2.8kB  build/src/kmc.js
npm notice 2.6kB  build/src/kmc.js.map
npm notice 120B   build/src/kmlmc.d.ts
npm notice 118B   build/src/kmlmc.d.ts.map
npm notice 1.4kB  build/src/kmlmc.js
npm notice 1.4kB  build/src/kmlmc.js.map
npm notice 131B   build/src/kmlmi.d.ts
npm notice 118B   build/src/kmlmi.d.ts.map
npm notice 3.1kB  build/src/kmlmi.js
npm notice 2.7kB  build/src/kmlmi.js.map
npm notice 128B   build/src/kmlmp.d.ts
npm notice 118B   build/src/kmlmp.d.ts.map
npm notice 1.6kB  build/src/kmlmp.js
npm notice 1.5kB  build/src/kmlmp.js.map
npm notice 285B   build/src/util/sysexits.d.ts
npm notice 186B   build/src/util/sysexits.d.ts.map
npm notice 51B    build/src/util/sysexits.js
npm notice 128B   build/src/util/sysexits.js.map
npm notice 29.8kB build/tsconfig.tsbuildinfo
npm notice 5.4kB  bundle.inc.sh
npm notice 1.7kB  package.json
npm notice 2.8kB  src/kmc.ts
npm notice 1.4kB  src/kmlmc.ts
npm notice 3.2kB  src/kmlmi.ts
npm notice 1.6kB  src/kmlmp.ts
npm notice 237B   src/util/sysexits.ts
npm notice 740B   tsconfig.json
npm notice 449B   tsconfig.kmc-base.json
```

For every release build, CI publishes a release of all packages.

