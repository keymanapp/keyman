Lexical Model Types
===================

Declares types required to define **Lexical Models**,
**Word breaking functions**, and all things in between.

Look at [index.d.ts](./index.d.ts) for documentation on each type.


Usage within keymanapp/keyman repo
----------------------------------

**Important**! `npm link` this module before building any other package with
*keymanapp/keyman that
depends on it:

    npm link .

Now you can build:

    common/predictive-text/
    developer/js/
