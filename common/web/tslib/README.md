The default import setup for the `tslib` package is unfortunately incompatible with `esbuild` when in ES5 mode.  But...
with a little elbow grease, we can fix that with _this_ package by importing its ES5-compatible file and exporting it
as _this_ package's default export for use in anything looking to `"importHelpers"`.

To utilize this with `esbuild` while enabling the `"importHelpers"` compilation option in your tsconfig.json, you'll want
to set the following in your `esbuild` config:

```javascript
  alias: {
    'tslib': '@keymanapp/tslib'
  },
```

Note that esbuild 0.15.16 is the minimum required version to utilize the 'alias' feature necessary to replace `tslib` for
`tsc`-generated `import { /* */ } from 'tslib'` statements that result from enabling `"importHelpers"` in a tsconfig.