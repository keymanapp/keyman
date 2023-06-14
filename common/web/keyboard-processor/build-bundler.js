import esbuild from 'esbuild';

/** @type {esbuild.BuildOptions} */
const commonConfig = {
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  sourcemap: true,
  format: "esm",
  target: "es5"
};

// Bundled ES module version
esbuild.buildSync({
  entryPoints: ['build/obj/index.js'],
  outfile: "build/lib/index.mjs",
  ...commonConfig
});

esbuild.buildSync({
  entryPoints: ['build/obj/keyboards/loaders/dom-keyboard-loader.js'],
  outfile: 'build/lib/dom-keyboard-loader.mjs',
  ...commonConfig
});

// The node-based keyboard loader needs an extra parameter due to Node-built-in imports:
esbuild.buildSync({
  entryPoints: ['build/obj/keyboards/loaders/node-keyboard-loader.js'],
  outfile: 'build/lib/node-keyboard-loader.mjs',
  platform: "node",
  ...commonConfig
});