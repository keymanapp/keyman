Language Modelling Layer (LMLayer)
==================================

Provide predictions and corrections while you type!

See [Worker Communication Protocol](./docs/worker-communication-protocol.md) for a
semi-formal specification on how the Worker and the main thread communicate.

System dependencies
-------------------

You will need Bash and Node.js >= 10.0.

Build
-----

Run `build.sh`. This will also automatically install dependencies with `npm`.

```sh
./build.sh configure build
```

### Two-stage compilation process

TODO: this is well and truly out of date...

Since the primary LMLayer code runs within a [Web Worker][], `build.sh` compiles the
LMLayer in two stages:

 1. Compile the inner worker code
    1. Compile the TypeScript sources for _only_ the Worker code.
    2. Wrap the Worker code as `embedded_worker.js`

 2. Compile the top-level code
    1. Include `embedded_worker.js` verbatim using a TypeScript directive.
    2. Compile the top-level TypeScript code.
    3. Unwrap the Worker code at runtime.

[Web Worker]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers

Test
----

This will run both headless unit tests, and in-browser unit tests and integration
tests:

```sh
./build.sh test
```

### TODO: epic/user-dict