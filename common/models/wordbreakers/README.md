# `@keymanapp/models-wordbreakers`

The word breakers bundled in Keyman's modeling layer (common/models).

Includes:

 - `default` — a word breaker based on the Unicode default word boundary specification
 - _deprecated_ `ascii` — an example word breaker
 - _deprecated_ `placeholder` — an example word breaker

## Usage

```
import {wordBreakers} from '@keymanapp/models-wordbreakers';
const breakWords = wordBreakers['default'];

console.log(breakWords('Hello, World!').map(span => span.text));
// prints: [ 'Hello', ',', 'World', '!' ]
```

## TODO: dict-breakers
