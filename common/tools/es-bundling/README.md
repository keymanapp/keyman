This subproject provides definitions for the common configuration settings used for bundling
web/ and common/web projects via `esbuild`.  This includes:

- Standard settings for bundling to IIFE and ESM module formats
- An `esbuild` plugin that facilitate tree-shaking of classes downcompiled to ES5 by TypeScript
- Two `esbuild` plugins that facilitate tree-shaking of unused `tslib` helper functions