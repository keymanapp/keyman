import type * as esbuild from 'esbuild';
import * as fs from 'fs';

/**
 * Refer to https://github.com/microsoft/TypeScript/issues/13721#issuecomment-307259227 -
 * the `@class` emit comment-annotation is designed to facilitate tree-shaking for ES5-targeted
 * down-level emits.  `esbuild` doesn't look for it by default... but we can override that with
 * this plugin.
 */
export const pluginForDowncompiledClassTreeshaking: esbuild.Plugin = {
  name: '@class -> __PURE__',
  setup(build) {
    build.onLoad({filter: /\.js$/ }, async (args) => {
      let source = await fs.promises.readFile(args.path, 'utf8');
      return {
        // Marks any classes compiled by TS (as per the /** @class */ annotation)
        // as __PURE__ in order to facilitate tree-shaking.
        contents: source.replaceAll('/** @class */', '/** @__PURE__ */'),
        loader: 'js'
      }
    });
  }
}