Keyman LMLayer prototype
========================

Prototype of NRC language model layer for Keyman.

I'm developing and experimenting with the API and how different API
designs will practically work.

Current status (Tue Oct  2 09:45:10 MDT 2018):

 - [ ] Developing communication protocol between keyboard and Web Worker.


Communication protocol between keyboard and asynchronous worker
---------------------------------------------------------------

![Sequence diagram of obtaining a prediction](./docs/predictive-text-sequence.png)

We have decided that everything to the right of the `KeymanWeb` will be in a
Web Worker. However, communication can happen only through
[`postMessage(payload)`][postMessage] commands,
where `payload` is a serializable object (via the [structured clone][]
algorithm).

What serializable object can we send that will adhere to the [open-closed principle]?

The idea is to use a [discriminated union][]. The protocol involves
plain JavaScript objects with one property called `method` that can take
a finite set of `string` values.

These string values indicate what method should be called. The rest of
the properties in the object are the parameters to method.

```javascript
{
    method: 'predict',
    // method-specific properties here
}
```

See also: [XML-RPC][]

[discriminated union]: http://www.typescriptlang.org/docs/handbook/advanced-types.html#discriminated-unions
[open-closed principle]: https://en.wikipedia.org/wiki/Open%E2%80%93closed_principle
[XML-RPC]: https://en.wikipedia.org/wiki/XML-RPC
[structured clone]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Structured_clone_algorithm
[postMessage]: https://developer.mozilla.org/en-US/docs/Web/API/Worker/postMessage


### Tokens

Tokens uniquely identify an input event, such as a keypress. Since
Keyman should ask for an asynchronous prediction on most keypresses, the
token is intended to associate a prediction and its response with
a particular input; Keyman is free to ignore prediction responses if
they are for outdated input events.

The `Token` type is opaque to LMLayer. That is, LMLayer does not inspect
its contents; it simply uses it to identify a request and pass it back
to Keyman. There are a few requirements on the concrete type of the
`Token`:

 1. It MUST be serializable via the [structured clone][] algorithm;
 2. It MUST be usable as a key in a [`Map`][Map object] object.

It is up to Keyman to create unambiguous tokens that can be uniquely
identified through the round-trip process.

In the following examples, I'll use the subset of `number` values that
are interpretable as [31-bit signed integers][Smi].

[Map object]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map#Key_equality
[Smi]: https://github.com/thlorenz/v8-perf/blob/master/data-types.md#efficiently-representing-values-and-tagging


### Example

An asynchronous message to predict after typing 'D':

```javascript
{
    method: 'predict',

    token: 1,
    transform: {
        insert: 'D',
        deleteLeft: 0,
        deleteRight: 0
    },
    contexts: [
        // TO BE DETERMINED
    ]
}
```
