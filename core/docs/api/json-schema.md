---
title: JSON Introspection Schema
---

The [`km_core_state_to_json()`](state#km_core_state_to_json) call
generates a JSON document describing the internal state of the keyboard
processor, this is the schema describing that document.

**WARNING**: The structure and format of the JSON document is independently
versioned is not considered part of C API. It is intended solely for use in
diagnostics or by development and debugging tools which may need to be aware of
keyboard processor engine implementation details.

```json
{"$schema": "http://json-schema.org/draft-06/schema#",
  "$id": "@replace_me_with_introspection_schema_uri@",
  "$comment": "© 2018 SIL International. MIT Licensed",
  "title": "Keyboard Processor State object introspection",
  "description": "Internal data from a Keyboard Processor State object, sufficient for debugging and diagnostics",
  "type": "object",

  "definitions": {
    "options": {
      "type": "object",
      "properties": {
        "scope": { "enum": ["unknown", "environment", "keyboard"] },
        "options": {
          "type": "object",
          "default": {}
        }
      },
      "required": ["scope", "options"]
    },
    "rule": {
      "type": "object"
    },
    "keyboard" : {
      "type": "object",
      "properties": {
        "id": { "type": "string" },
        "version": { "type": "string" },
        "folder": { "type": "string"},
        "options": { "$ref": "#definitions/options" },
        "rules": {
          "type":"array",
          "items": { "$ref": "#/definitions/rule" }
        }
      },
      "required": ["id", "version", "folder", "options", "rules"]
    },
    "context": {
      "type":"array",
      "items": {"$ref": "#/definitions/context_item"},
      "default": []
    },
    "context_item": {
      "oneOf": [
        {"type": "string", "minLength": 1, "maxLength": 4},
        {"type": "number"}
      ]
    }
  },
  "properties": {
    "$schema": { "const": "keyman/core/docs/introspection.schema" },
    "keyboard": { "$ref": "#/definitions/keyboard" },
    "options": {
      "type": "object",
      "properties": {
        "enviroment": { "$ref": "#/definitions/options" },
        "dynamic": { "$ref": "#/definitions/options" }
      },
      "required": [ "enviroment","dynamic"]
    },
    "context": { "$ref": "#/definitions/context" }
  }
}
```