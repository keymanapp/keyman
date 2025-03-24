---
title: Clone Keyman Cloud Project Parameters Dialog
---

![Clone Keyman Cloud Project Parameters dialog](../images/ui/frmCloneKeymanCloudProjectParameters.png)

Allows you to clone a project from Keyman Cloud, giving the project a
new id, and organizing project files following the
[file layout](/developer/keyboards/) used in the
[Keyman keyboards repository](https://github.com/keymanapp/keyboards).

Projects can also be cloned from the command line with [`kmc copy`](kmc).

Only keyboard projects can be cloned from Keyman Cloud at this time via this
dialog. Lexical model projects can be cloned from Keyman Cloud via the command
line.

Search for a keyboard using the standard keyman.com keyboard search in the upper
pane. Once you select a keyboard, and fill in the New Project Details, the OK
button will be enabled to allow you to finish the cloning process.

### Parameters

Destination path
:   Specifies the base path where the project folder will be created.
    The project folder name will be the keyboard ID. If the folder
    already exists, then you will be prompted before Keyman Developer
    overwrites files inside it.

New project ID
:   The base filename of the keyboard, project and package. This must
    conform to the Keyman keyboard identifier rules.

Relocate external files into new project folder
:   When selected, if the project references files that are not inside the same
    folder as the .kpj file, move these files into an `external` subfolder

