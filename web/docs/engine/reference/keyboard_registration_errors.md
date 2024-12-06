---
title: Keyboard Registration Errors
---

If KeymanWeb has an error while adding/registering a keyboard, the `ErrorStub` object containing the following members is returned:

`keyboard`

: `object` <span class="optional">optional</span>

  Information on the keyboard

`language`

: `object` <span class="optional">optional</span>

  Information on the language associated with the keyboard

`error`

: `Error`

  Javascript error that occurred during keyboard registration

---

The `ErrorStub.keyboard` object contains the following members:

`id`

: `string`

ID (internal name) of keyboard

`name`

: `string`

Name of the keyboard

---

The `ErrorStub.language` object contains the following members:

`id`

: `string`

BCP 47 code for supported keyboard language. If a keyboard supports multiple languages, an array of ErrorStubs is returned.

`name`

: `string`

Name of the language
