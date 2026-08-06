# javascript_error keyboard

## Description

Keyboard with a script error in `gs()`, taken originally from release/b/banne

The .js and .kmp files in modified_build/ have been manually tweaked to throw the error:

```js
  this.gs=function(t,e) {
    throw new Error("javascript_error.js - runtime error");
    // return this.g_main_0(t,e);
  };
```