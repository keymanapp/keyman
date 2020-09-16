# Scopes and Commit Types

This folder contains definitions for valid scopes and commit types for Keyman.

[Scope Documentation](https://github.com/keymanapp/keyman/wiki/Keyman-Project-Scopes)

## scopes.json

* A tree structured JSON object, with root node `"scopes"`.
* Keys must have lower case alpha names; leaf nodes must always be `null`.
* If you change scopes here, please update /.github/labeler.yml accordingly.

## commit-types.json

* A JSON object with a single property: `"commitTypes"` which must be an array of strings.
* Values in the array must contain lower case alphabetical letters only.

# Schemas

* scopes.schema.json
* commit-types.schema.json

Any changes MUST be validated against these schemas before commit. One online tool that does this: https://www.jsonschemavalidator.net/
