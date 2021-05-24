# Keyman for Windows

Formerly called "Keyman Desktop", now called "Keyman" to harmonize with other platforms.

# help

Contains all the help documents in Markdown format for Keyman for Windows. It can build both compiled help chm or HTML format.

# inst - keymandesktop.msi

Building and signing of the installation archive keymandesktop.msi

# insthelp - insthelp.exe

Builds a helper executable to do various installation tasks - insthelp.exe.

# kmbrowserhost - kmbrowserhost.exe

kmbrowserhost.exe is the chromium embedded framework (CEF) host process executable. CEF uses a multi-process model and subprocesses are hosted in kmbrowserhost.exe.

# kmconfig

Used for configuration of advanced Keyman settings. Configuration can be imported from a json file or exported to a json file. Individual settings can be set.

# kmshell - Kmshell.exe

Keyman configuration tool that can be run from the command line with arguments to automate a number of tasks.
For example installing and uninstalling keyboards.

# locale
User interface strings in various languages

# setup

Installation program for installing and initial setup of Keyman. A wrapper for keymandesktop.msi that also automates keyboard downloads and update downloads.
