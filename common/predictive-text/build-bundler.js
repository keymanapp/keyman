import esbuild from 'esbuild';

await esbuild.build({
  alias: {
    'tslib': '@keymanapp/tslib'
  },
  bundle: true,
  format: "esm",
  sourcemap: true,
  target: "es5",
  external: ['fs', 'vm'],
  // nodePaths: ['../../node_modules'],
  entryPoints: {
    'index': 'build/obj/web/index.js',
  },
  outdir: 'build/lib/web',
  outExtension: { '.js': '.mjs' }
});