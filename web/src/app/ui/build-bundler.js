/*
 * Note:  while this file is not meant to exist long-term, it provides a nice
 * low-level proof-of-concept for esbuild bundling of the various Web submodules.
 *
 * Add some extra code at the end of src/index.ts and run it to verify successful bundling!
 */

import esbuild from 'esbuild';

import fs from 'fs';

/*
 * Refer to https://github.com/microsoft/TypeScript/issues/13721#issuecomment-307259227 -
 * the `@class` emit comment-annotation is designed to facilitate tree-shaking for ES5-targeted
 * down-level emits.  `esbuild` doesn't look for it by default... but we can override that with
 * this plugin.
 */
let es5ClassAnnotationAsPurePlugin = {
  name: '@class -> __PURE__',
  setup(build) {
    build.onLoad({filter: /\.js$/ }, async (args) => {
      let source = await fs.promises.readFile(args.path, 'utf8');
      return {
        // Marks any classes compiled by TS (as per the /** @class */ annotation)
        // as __PURE__ in order to facilitate tree-shaking.
        contents: source.replace('/** @class */', '/* @__PURE__ */ /** @class */'),
        loader: 'js'
      }
    });
  }
}

const modules = ['kmwuibutton', 'kmwuifloat', 'kmwuitoggle', 'kmwuitoolbar'];

for(let module of modules) {
  await esbuild.build({
    bundle: true,
    sourcemap: true,
    format: "iife",
    nodePaths: ['../../../../node_modules'],
    entryPoints: {
      'index': `../../../build/app/ui/obj/${module}.js`,
    },
    outfile: `../../../build/app/ui/debug/${module}.js`,
    plugins: [ es5ClassAnnotationAsPurePlugin ],
    target: "es5",
    treeShaking: true,
    tsconfig: './tsconfig.json'
  });

  await esbuild.build({
    bundle: true,
    sourcemap: true,
    minifyWhitespace: true,
    minifySyntax: true,
    minifyIdentifiers: false,
    format: "iife",
    nodePaths: ['../../../../node_modules'],
    entryPoints: {
      'index': `../../../build/app/ui/obj/${module}.js`,
    },
    outfile: `../../../build/app/ui/release/${module}.js`,
    plugins: [ es5ClassAnnotationAsPurePlugin ],
    target: "es5",
    treeShaking: true,
    tsconfig: './tsconfig.json'
  });
}