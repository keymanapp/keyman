---
author: "Steven R. Loomis <srl295@gmail.com>"
date: "2025-05-19"
issue: "https://github.com/keymanapp/keyman/issues/14020"
---

# Keyman Developer Messaging

## Summary

This document discusses the mechanism for kmc Compiler \+ Toolchain Messages in Keyman Developer.

## Objectives

1. Provide developers (end users) with feedback on the failure (or success) of their use of Keyman Developer tools. (Compilers and others: All compilers are tools, but not all tools are compilers.)
2. Enable that feedback to be actionable:
   1. Clear: so the developer knows **what** happened.
   2. Concise: so the developer can identify the relevant parts of the message without extraneous information. Enable deduplication: allow the consumer of the messages to deduplicate messages, but don't deduplicate them prematurely.
   3. Context: so the developer knows **where** the issue happened. Exactly what part of the process, which file, which line/column number.
      Enable presentation of failing LOC by message consumer
3. Empower the \#developer-team of Keyman to effectively author these messages, that is, encourage deployment of messages meeting above goals
   1. Contain ceremony (repetitive content) around message call sites
      1. declutter the calling code
      2. encourage developers to make use of distinct messaging vs. 'catch-alls': "Something Bad Happened \[Here\]"
   2. Contain burden on tool/platform implementation code
   3. Single mechanism for reporting (and it's not `console.log`).
      1. we don't expect exceptions to bubble up to users, that's an exceptional \[sic\] case.
   4. [publish messages to the website](https://help.keyman.com/developer/current-version/reference/messages/)
   5. validation/ linting
   6. formal representation of parameterized messages rather than string concatenation or in-compiler message massaging code
      1. in short, string content should live in the messages, not the compiler
4. Responsibility of consumer (i.e. CLI tool, IDE) to:
   1. format
   2. display
   3. future: localize ( i18n )?

## Details of the Current Mechanism

### Types

#### `CompilerEvent`

* `filename` — string
* `offset` — number, in some cases, resolves to line:column
* `line` — number, may be set directoly
* `column` — number, may be set directly
* `code` — hex, [see below](#code)
* `message` — resolved formatted text
* `detail` — Markdown with more detail and suggestions
* `exceptionVar` — used for internal tracking of exceptions

### Code

1. [https://help.keyman.com/developer/current-version/reference/messages/](https://help.keyman.com/developer/current-version/reference/messages/)
2. Example: `KM03009: ERROR_UnknownWordBreaker`

* `00503009` - hex value
  * `RRSNNBBB` \- (see below)
  * `RR______` \- Reserved
  * `__S_____` \- Severity, example, `5` = error
  * `___NN___` \- Namespace, example  `03` = model compiler
  * `_____BBB` \- Base error, ex `009`=  "unknown word breaker"

### Callback

The `callbacks` object includes the function:

* `reportMessage(e: CompilerEvent)` provided by the consumer

  We have chained/wrapper callbacks in some cases.

### Metadata

* The fast-xml-parser package has an option to generate metadata (via a special symbol property) on each object in the parse tree.
* This metadata includes line numbering.

```js
interface XMLMetaData { startIndex?: number };
```

* We extend this metadata concept to include symbols for import status and filename, in the case of imported files.

## Flow

### Call Site

Example: keys.ts  \- compiler \- call site for error

```js
  this.callbacks.reportMessage(
    LdmlCompilerMessages.Error_InvalidScanCode({
      invalidCodeList: codes.join(' '), id:x?.id }, form)
  );
```

  * `{ id, codes }` \- message parameters (optional)
  * `form` \- object that caused the error (contains line number metadata)

### Message Expansion

 Message expansion `m()` or `mx()` (in LDML) function is used.

```js
static ERROR_InvalidScanCode = SevError | 0x0009;
static Error_InvalidScanCode = (o:{id: string, invalidCodeList: string}, x: ObjectWithCompileContext) => mx(
  this.ERROR_InvalidScanCode, context,
Form '${def(o.id)}' has invalid/unknown scancodes '${def(o.codes)}',
  `…additional markdown detail…`
);
```

Parameters:

* 'o' (Object) used for parameters- here, the object id and the codes.
* 'x' (conteXt) used for extracting the offset, which will be resolved to line numbers.

_Q: But wait, doesn't `x.id` already have the 'id'? DRY!?_

_A: Yes, however, we want a clean separation between parameters and context. This way, even though repetitive, makes it clear exactly what's being passed and consumed as a parameter.  `x` has other irrelevent stuff on it._

That is, no additional code in the \*_*() function (the caller of m(), mx() \- put helper functions outside ,move into the call site)

### Context extraction

The `mx()` function does the following (note that `m()` is called first).

```js
let evt = m(code, message, detail);        // raw message
evt = LdmlCompilerMessages.offset(evt, x); // with offset
```

Note: We could have:    `ml(e:CompilerEvent, lineNumber?: number)` so that  compilers with a line number could pass that in directly.

### Callbacks


`reportMessage()`

* pre-process if needed \- expand `offset`  (ex: byte offset 276\) to `line:column`  (line 13 column 4\)
* "end user" of compiler receives result with `messages[]` array
