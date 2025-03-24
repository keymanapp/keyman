---
title: New Lexical Model Project Parameters Dialog
---

![New Lexical Model Project Parameters dialog](../images/ui/frmNewLMProjectParameters.png)

Allows you to quickly fill in common parameters for a new lexical model
project, adding author name, model name, version, and language
information following the file layout used in the [Keyman lexical-models repository](https://github.com/keymanapp/lexical-models).

### Parameters

Author Name
:   The name of the developer of the lexical model.

Model Name
:   The descriptive name of the lexical model. This should be a unique
    name to distinguish this lexical model from others in the same
    language.

Copyright
:   A copyright string for the model. This will be set in the the
    package metadata, and where appropriate in documentation and
    metadata.

Version
:   The initial version number of the model should usually be left at
    1.0. This will be set also in the package metadata, and where
    appropriate in documentation and metadata. The version string is
    made of `major revision`.`minor revision`. When releasing
    significant updates to the model, increment the major revision (e.g.
    2.0). Increment the minor revision (e.g. 1.1) for small changes to
    the model.

Languages
:   Specifies the default BCP 47 language tags which will be added to
    the [package metadata](../reference/file-types/metadata) and project
    metadata. In order to install and use a lexical model with a Keyman
    keyboard, the BCP 47 language tags for both must be identical.