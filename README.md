Keyman LMLayer prototype
========================

[![Build Status](https://travis-ci.org/eddieantonio/keyman-lmlayer-prototype.svg?branch=master)](https://travis-ci.org/eddieantonio/keyman-lmlayer-prototype)

Prototype of NRC language model layer for Keyman.

I'm developing and experimenting with the API and how different API
designs will practically work.

>  The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
>  NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and
>  "OPTIONAL" in this document are to be interpreted as described in
>  [RFC 2119].

[RFC 2119]: https://www.ietf.org/rfc/rfc2119.txt


Communication protocol between keyboard and asynchronous worker
---------------------------------------------------------------

![Sequence diagram of obtaining a prediction](./docs/predictive-text-sequence.png)

We have decided that everything to the right of the `KeymanWeb` will be
in a Web Worker. However, communication can happen only through
[`postMessage(data)`][postMessage] commands,
where `data` is a serializable object (via the [structured clone][]
algorithm).

What serializable object can we send that will adhere to the [open-closed principle]?

### Messages

The idea is to use a [discriminated union][]. The protocol involves
plain JavaScript objects with one property called `message` that takes
a finite set of `string` values.

These string values indicate what message should be sent. The rest of
the properties in the object are the parameters send with the message.

```javascript
{
    message: 'predict',
    // message-specific properties here
}
```

See also: [XML-RPC][]

Messages are **not** methods. That is, there is no assumption that
a client will receive a reply when a message is sent. However, some
pairs of messages, such as `predict` and `suggestions`, lightly assume
request/response semantics.

[discriminated union]: http://www.typescriptlang.org/docs/handbook/advanced-types.html#discriminated-unions
[open-closed principle]: https://en.wikipedia.org/wiki/Open%E2%80%93closed_principle
[XML-RPC]: https://en.wikipedia.org/wiki/XML-RPC
[structured clone]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Structured_clone_algorithm
[postMessage]: https://developer.mozilla.org/en-US/docs/Web/API/Worker/postMessage


### Tokens

Tokens uniquely identify an input event, such as a key press. Since
Keyman should ask for an asynchronous prediction on most key presses, the
token is intended to associate a prediction and its response with
a particular input; Keyman is free to ignore prediction responses if
they are for outdated input events.

The `Token` type is opaque to LMLayer. That is, LMLayer does not inspect
its contents; it simply uses it to identify a request and pass it back
to Keyman. There are a few requirements on the concrete type of the
`Token`:

 1. Tokens **MUST** be serializable via the [structured clone][] algorithm;
 2. Tokens **MUST** be usable as a key in a [`Map`][Map object] object.
 3. Tokens **MUST** be unique across messages. That is, tokens **MUST
    NOT** be duplicated between different messages.

It is up to the keyboard to create unambiguous tokens that can be uniquely
identified through the round-trip process.

In the following examples, I'll use the subset of `number` values that
are interpretable as [31-bit signed integers][Smi].

[Map object]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map#Key_equality
[Smi]: https://github.com/thlorenz/v8-perf/blob/master/data-types.md#efficiently-representing-values-and-tagging
[Tokens]: #tokens

### Example

An asynchronous message to predict after typing 'D':

```javascript
{
    message: 'predict',

    token: 1,
    transform: {
        insert: 'D',
        deleteLeft: 0,
        deleteRight: 0
    },
    context: {
      left: '',
      right: '',
      startOfBuffer: true,
      endOfBuffer: true
    },
}
```

Message types
-------------

Currently there are four message types:

Message       | Direction          | Parameters          | Expected reply      | Uses token
--------------|--------------------|---------------------|---------------------|---------------
`initialize`  | keyboard → LMLayer | initialization      | Yes — `ready`       | No
`ready`       | LMLayer → keyboard | configuration       | No                  | No
`predict`     | keyboard → LMLayer | transform, context  | Yes — `suggestions` | Yes
`suggestions` | LMLayer → keyboard | suggestions         | No                  | Yes


### Message: `initialize`

Must be sent from the keyboard to the LMLayer so that the LMLayer
initializes a model. It will send `initialization` which is a plain
JavaScript object specify the path to the model, as well configurations
and platform restrictions.

The keyboard **MUST NOT** send any messages to the LMLayer prior to
sending `initialize`. The keyboard **SHOULD NOT** send another message
to the keyboard until it receives `ready` message from the LMLayer
before sending another message.

These are the configurations, and platform restrictions sent to
initialize the LMLayer and its model.

```javascript
let initialization = {
  /**
   * [REQUIRED]
   * Path to the model. There are no concrete restrictions on the path
   * to the model, so long as the LMLayer can succesfully use it to
   * initialize the model.
   *
   * type: string
   */
   model: './models/en_CA-x-testing',

  /**
   * Whether the platform supports right contexts.
   * The absense of this rule implies false.
   *
   * type: bool
   */
  supportsRightContexts: false,

  /**
   * Whether the platform supports deleting to the right.
   * The absence of this rule implies false.
   *
   * type: bool
   */
  supportsDeleteRight: false,

  /**
   * [REQUIRED]
   * The maximum amount of UTF-16 code units that the keyboard will
   * provide to the left of the cursor.
   *
   * type: number
   */
  maxLeftContextCodeUnits: 32,

  /**
   * The maximum amount of code units that the keyboard will provide to
   * the right of the cursor. The absence of this rule implies 0.
   * See also, supportsRightContexts.
   *
   * type: number
   */
  maxRightContextCodeUnits: 32,
};
```


### Message: `ready`

Must be sent from the LMLayer to the keyboard when the LMLayer's model
as a response to `initialize`. It will send `configuration`, which is
a plain JavaScript object requesting configuration from the keyboard.

There are only two options defined so far:

```javascript
let configuration = {
    /**
     * How many UTF-16 code units maximum to send as the context to the
     * left of the cursor ("left" in the Unicode character stream).
     *
     * Affects the `context` property sent in `predict` messages.
     *
     * TODO: Will this ever bisect graphical cluster boundaries?
     */
    leftContextCodeUnits: 32,

    /**
     * How many UTF-16 code units maximum to send as the context to the
     * right of the cursor ("right" in the Unicode character stream).
     *
     * Affects the `context` property sent in `predict` messages.
     *
     * TODO: Will this ever bisect graphical cluster boundaries?
     */
    rightContextCodeUnits: 32,
};
```


### Message: `predict`

Sent from the keyboard to the LMLayer whenever a new prediction should
be generated. This is typically initiated by a key press event. The
keyboard **SHOULD** track each `predict` message using a [token][Tokens]. The
token **MUST** be unique across all prediction events. The LMLayer
**SHOULD** respond to each `predict` message with a `suggestions`
message. The `suggestions` message **MUST** contain the corresponding
token as sent in the initial `predict` message.

The keyboard **MUST** send the `context` parameter. The keyboard
**SHOULD** send the `transform` parameter. The keyboard **MUST** send
a unique token.

The semantics of the `predict` message **MUST** be from the
perspective of this sequence of events:

 1. After the input event is received by the keyboard.
 2. Before the keyboard applies the associated `transform` to the buffer.

**NOTE**: The keyboard **MAY** apply the `transform` associated with the
input event before receiving the corresponding `suggestions` message
from the LMLayer. The intention is that once the suggestions are displayed,
the typist may select one of the suggestions in the place of the effects
of their original input.

**NOTE**: The keyboard **MAY** send the `predict` message after the
`transform` associated with the input event is applied to the buffer,
however the semantics **MUST** remain the same—the prediction happens
from the perspective before the `transform` has been applied.

The context is the text surrounding the insertion point, _before_ the
transform is applied to the buffer.

```javascript
let context = {
  /**
   * Up to maxLeftContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the left of the insertion point in the
   * buffer. If there is nothing to the left of the buffer, this returns
   * an empty string.
   *
   * type: USVString
   */
  left: "I'm a little ",

  /**
   * [OPTIONAL]
   * Up to maxRightContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the right of the insertion point in the
   * buffer. If there is nothing to the right of the buffer, this returns
   * an empty string.
   *
   * type: USVString
   */
  right: '',

  /**
   * Whether the insertion point is at the start of the buffer.
   *
   * type: boolean
   */
  startOfBuffer: false,

  /**
   * Whether the insertion point is at the end of the buffer.
   *
   * type: boolean
   */
  endOfBuffer: true
};
```

The transform parameter describes how the input event will change the
buffer.

```javascript
let transform = {
  /**
   * The Unicode scalar values (i.e., characters) to be inserted at the
   * cursor position.
   *
   * Corresponds to `s` in com.keyman.KeyboardInterface.output.
   *
   * type: USVString <https://heycam.github.io/webidl/#idl-USVString>
   */
  insert: 't',

  /**
   * The number of code units to delete to the left of the cursor.
   *
   * Cooresponds to `dn` in com.keyman.KeyboardInterface.output.
   *
   * type: number (integer values only)
   */
  delete: 0,

  /**
   * [OPTIONAL]
   * The number of code units to delete to the right of the cursor.
   *
   * type: number (integer values only)
   */
  deleteRight: 0,
};
```


### Message: `suggestions`

The `suggestions` message is sent from the LMLayer to the keyboard. This
message sends a ranked array of suggestions, in descending order of
probability (i.e., entry `0` is most likely, followed by entry `1`,
etc.). This message **MUST** be in response to a `predict` message, and
it **MUST** respond with the corresponding [token].

```javascript
/**
 * `suggestions` is an ordered array of suggestion objects.
 * Each suggestion is a transform bundled with a `displayAs` property.
 */
let suggestions = [
  {
    /**
     * Same object as an input event transform.
     * Note that the transform is applied AFTER the input event
     * transform.
     */
    transform: {
      insert: 'teapot',
      deleteLeft: 1,
      deleteRight: 0
    },

    /**
     * A string to display the suggestion to the typist.
     * This should aid the typist understand what the transform will do
     * to their text.
     *
     * type: string
     */
    displayAs: '🍵'
  }
];
```

#### Timing

Each suggestion provides a `transform`. This transform is applied
_after_ the transform associated with the input event that initiated
this prediction. That is, the suggested transform applies to the buffer
after the transform associated with the input event.

Rephrased in somewhat mathematical terms:
Let 𝑥 ∈ 𝐵 be the input text buffer. Let 𝑇<sub>𝑖</sub> be the transform
associated with an input event that maps a text buffer 𝐵 → 𝐵. Let
𝑇<sub>𝑠</sub> be a transform suggested through the LMLayer. 𝑦 ∈ 𝐵 is the
text buffer after the suggestion transform has been applied.  The
correct sequence of applications should be as follows:

> 𝑦 = 𝑇<sub>𝑠</sub>(𝑇<sub>𝑖</sub>(𝑥))


TODO
====

 - [ ] Do word segmentation
 - [ ] Determine the exact arguments given to the model's `predict()`
       method.
 - [ ] Document `suggestions`
 - [~] TypeScript!
 - [ ] make simple `index.html` that demos a dummy model
 - [ ] make an `error` initialization message.
 - [ ] LOGLIKEIHOOD IN THE TRANSFORM!
 - [ ] `possibleTransforms` where `transform` is an alias for `possibleTransforms[0]`
 - [ ] Use [puppeteer](https://github.com/GoogleChrome/puppeteer)?
