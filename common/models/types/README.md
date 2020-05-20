`@keymanapp/models-types`
=========================

Declares TypeScript types and interfaces required to define **Lexical
Models**, **Word breaking functions**, and all things in between.

Install
-------

> (see below if you are working in the keymanapp/keyman repo!)

    npm install --save-dev @keymanapp/models-types

Usage
-----

Say you are creating a **custom lexical model**. Then import this module
in your lexical model file!

```Typescript
// my-nifty-model.ts

import "@keymanapp/models-types";

export class MyNiftyClass implements LexicalModel {
  configure(capabilities: Capabilities): Configuration {
    // TODO: implement me!
    throw new Error("Method not implemented.");
  }
  predict(transform: Transform, context: Context): ProbabilityMass<Suggestion>[] {
    // TODO: implement me!
    throw new Error("Method not implemented.");
  }
  wordbreak(context: Context): string {
    // TODO: implement me!
    throw new Error("Method not implemented.");
  }
  punctuation?: LexicalModelPunctuation;
}
```

If your are creating a **custom word breaker**, you would do the same:

```typescript
import "@keymanapp/models-types";

export let myShinyWordBreaker: WordBreakingFunction;
myShinyWordBreaker = function (phrase: string): Span[] {
  // TODO: implement me!
  throw new Error("Function not implemented")
}
```

Look at [index.d.ts](./index.d.ts) for documentation on each and every
type!


Usage within keymanapp/keyman repo
----------------------------------

To use it in other subprojects within keymanapp/keyman, ensure that this
package is linked using **Lerna**. From the root of the repository, run:

    npx lerna bootstrap

Add this dependency to your subproject like so:

    npx lerna add @keymanapp/models-types path/to/your/subproject
