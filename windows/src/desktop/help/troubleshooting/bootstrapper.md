---
title: How To - Use the Keyman Setup Bootstrapper
---

## About the Bootstrapper

TODO: rewrite for 14.0 content

The Keyman installation bootstrapper is a self-extracting executable
file that contains a Windows Installer technology MSI file, and
optionally keyboards.

The bootstrapper:

1.  Checks the system before starting the installation to ensure that
    all prerequisites are met, and optionally downloads and installs the
    prerequisites.

2.  Optionally checks online for an updated version of Keyman before
    installing.

3.  Starts the Keyman Windows Installer MSI package.

4.  Installs any keyboards included.

5.  Starts Keyman after the install completes.

## Command Line Options

The following command line parameters are supported in the bootstrapper:

-   -c — Continue setup after Windows restarts (this parameter is not
    normally required)

-   -s — Run a silent installation

-   -o — Do not check online for updates or prerequisites (useful with
    -s)

-   -x &lt;extractpath&gt; — Extract all the files from the
    self-extracting executable to the path extractpath and exit the
    bootstrapper.

## Related Topics

-   [How To - Download and Install Keyman](../start/download-install_keyman)
