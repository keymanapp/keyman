import esbuild from 'esbuild';

// Bundled ES module version
esbuild.buildSync({
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  sourcemap: true,
  format: "esm",
  target: "es5",
  entryPoints: ['build/obj/index.js'],
  outfile: "build/lib/index.mjs"
});