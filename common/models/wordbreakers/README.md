# `@keymanapp/lexical-model-word-breakers`

The word breakers bundled in Keyman's lexical model layer (LMLayer).

Includes:

 - `default` — a word breaker based on the Unicode default word boundary specification
 - _deprecated_ `ascii` — an example word breaker
 - _deprecated_ `placeholder` — an example word breaker

## Usage

```
import {default as breakWords} from '@keymanapp/lexical-model-word-breakers';

console.log(breakWords('Hello, World!').map(span => span.text));
// prints: [ 'Hello', ',', 'World', '!' ]
```
