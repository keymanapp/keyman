Lexical Model Types
===================

Declares types required to define **Lexical Models**,
**Word breaking functions**, and all things in between.

Look at [index.d.ts](./index.d.ts) for documentation on each type.


Usage within keymanapp/keyman repo
----------------------------------

To use it in other subprojects within keymanapp/keyman, ensure that this
package is linked using:

    npm link .

The following subprojects already perform `npm link .` as part of their `build.sh`:

    common/predictive-text/
    developer/js/
