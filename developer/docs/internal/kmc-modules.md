---
title: kmc module interfaces
---

kmc, aka 'Keyman Compiler' is designed as a set of activity modules (e.g.
`@keymanapp/kmc-kmn`) which are all access with a common interface
`KeymanCompiler`, together with a module `@keymanapp/kmc` that provides
command line access to these modules.

The modules can also be accessed programmatically through these interfaces
and classes, defined in compiler-interfaces.ts:

* `KeymanCompiler`
* `KeymanCompilerArtifact`
* `KeymanCompilerArtifactOptional`
* `KeymanCompilerArtifacts`
* `KeymanCompilerResult`

The following interfaces and classes are also relevant:

* `CompilerCallbacks`
* `CompilerEvent`
* `CompilerOptions`
* `CompilerError`

Each module defines one implementation of `KeymanCompiler`, and extensions
of `KeymanCompilerArtifact`, `KeymanCompilerResult`, and `CompilerOptions`.

## A sample run

```ts
  const callbacks = new MyCompilerCallbacks();
  const options: KmnCompilerOptions = { ... };
  const compiler = new KmnCompiler();
  if(!await compiler.init(callbacks, options)) { ... }
  const result = await compiler.run(infile, outfile);
  if(result) {
    if(!await compiler.write(result.artifacts)) { ... }
  }
```

## Design requirements for activity modules

Some design requirements for the activity modules:
* Isolated from I/O
* Cross-platform
* Error and message management
* Activity artifacts
* Stateless
* Unit test endpoint pattern

### Isolated from I/O

Modules must be isolated from all I/O (module loading is assumed to be
available); `CompilerCallbacks` provides access to external files if they are
required. No console messages may be emitted in production builds.

### Cross-platform
Modules must run on both Node and Web platforms, thus must not import `node:`
libraries or libraries that are incompatible with either platform.

### Error and message management

Compiler messages -- errors relating to user data -- must be emitted using the
`CompilerCallbacks.reportMessage` function and defined as a `CompilerEvent` using
the common messages.ts file pattern.

Internal errors should be thrown as exceptions, but these should be handled
within the module, rather than pushed back up to the caller. The top-most
exception handler in the module should use the
`Fatal_UnexpectedException({e:e})` pattern to report the error to the user.

* See [Keyman Developer Messaging](developer-messaging) for further detail on
  messages.

### Activity artifacts

The activity will produce a set of in-memory artifacts using an extension of the
`KeymanCompilerArtifact` interface. These will be written to disk by the
compiler using a callback.

### Stateless

Each module should be not include internal state between runs; compiler options
will persist. This allows the same module instance to be re-used for batch
builds.

### Unit test endpoint pattern

Any class implementing the `KeymanCompiler` interface should include a property
`unitTestEndpoints` to make internal functions accessible for test, rather than
making the functions public. The only public functions should be implementations
of the `KeymanCompiler` interface.

