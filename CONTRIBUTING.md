# Contributing to Keyman

⭐ Thank you for your contribution! ⭐

The following is a set of guidelines for contributing to Keyman, Keyman
keyboards, Keyman lexical models and Keyman websites, which are hosted
in the [Keymanapp Organization](https://github.com/keymanapp) on GitHub.
These are mostly guidelines, not rules. Use your best judgment, and feel
free to propose changes to this document in a pull request.

(These guidelines have been adapted from [Atom Guidelines](https://github.com/atom/atom/blob/master/CONTRIBUTING.md).)

## Table Of Contents

[Code of Conduct](#code-of-conduct)

[I don't want to read this whole thing, I just have a question!!!](#i-dont-want-to-read-this-whole-thing-i-just-have-a-question)

[What should I know before I get started?](#what-should-i-know-before-i-get-started)
  * [Keyman, Keyboard Layouts and Lexical Models](#keyman-keyboard-layouts-and-lexical-models)

[How Can I Contribute?](#how-can-i-contribute)
  * [Reporting Bugs](#reporting-bugs)
  * [Suggesting Enhancements](#suggesting-enhancements)
  * [Your First Code Contribution](#your-first-code-contribution)
  * [Building Keyman](#building-keyman)
  * [Localize Keyman](#localize-keyman)
  * [Other Ways of Getting Involved](#other-ways-of-getting-involved)

[Style Guides](#style-guides)
  * [Principles of Keyman Code Changes](#principles-of-keyman-code-changes)
  * [Pull Request and Commit Workflow](#pull-request-and-commit-workflow)
  * [Code Style Guide](#code-style-guide)
  * [User Testing](#user-testing)
  * [Documentation Style Guide](#documentation-style-guide)

[Additional Notes](#additional-notes)
  * [Issue and Pull Request Labels](#issue-and-pull-request-labels)

## Code of Conduct

This project and everyone participating in it is governed by the [Keyman
Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected
to uphold this code. Please report unacceptable behavior to
[community@keyman.com](mailto:community@keyman.com).

## I don't want to read this whole thing I just have a question!!!

**Note:** If you have a question about using Keyman, you'll get faster
results by asking in the Keyman Community, where many other users may
also be able to assist you. If you have a question about the Keyman
source code, feel free to open an issue!

* [Keyman Community](https://community.software.sil.org/c/keyman)

## What should I know before I get started?

### Keyman, Keyboard Layouts and Lexical Models

Keyman is a complex project that spans six different platforms, with
many a variety of components on each platform. We have chosen to host
the Keyman project in the
[keymanapp/keyman](https://github.com/keymanapp/keyman) monorepo, but
[keyboard layouts](https://github.com/keymanapp/keyboards) and [lexical
models](https://github.com/keymanapp/lexical-models) are stored in their
own repositories.

* Issues relating to the Keyman apps and their interactions with
  operating systems or other apps should be opened in the Keyman repo
* For keyboard-specific or model-specific issues, open issues in the
  corresponding repository.
* The Keyman websites are also hosted in their own repositories.
  Contributions, including issues and pull requests can be opened
  against these repositories also.
* Product documentation for the Keyman app is hosted in the Keyman app
  repository, and replicated to the help.keyman.com repository.

The primary repositories you are most likely to interact with are:

* [Keyman app](https://github.com/keymanapp/keyman)
* [Keyboards](https://github.com/keymanapp/keyboards)
* [Lexical models](https://github.com/keymanapp/lexical-models)
* [keyman.com site](https://github.com/keymanapp/keyman.com)
* [help.keyman.com site](https://github.com/keymanapp/help.keyman.com)
* [keymanweb.com site](https://github.com/keymanapp/keymanweb.com)
* [api.keyman.com site](https://github.com/keymanapp/api.keyman.com)

## How Can I Contribute?

### Reporting Bugs

This section guides you through submitting a bug report for Keyman.
Following these guidelines helps maintainers and the community
understand your report :pencil:, reproduce the behavior :computer:
:computer:, and find related reports :mag_right:.

Before creating bug reports, please check [this
list](#before-submitting-a-bug-report) as you might find out that you
don't need to create one. When you are creating a bug report, please
[include as many details as
possible](#how-do-i-submit-a-good-bug-report). Fill out [the required
template](https://github.com/keymanapp/keyman/issues/new?assignees=&labels=bug&template=bug_report.md&title=bug%3A+);
the information it asks for helps us resolve issues faster.

> **Note:** If you find a **Closed** issue that seems like it is the
> same thing that you're experiencing, open a new issue and include a
> link to the original issue in the body of your new one.

#### Before Submitting A Bug Report

* **Check that you are running the [latest
  version](https://keyman.com/downloads).**
* **Check the [community](https://community.software.sil.org/c/keyman)**
  for common questions and problems; search there to see if other people
  have reported the issue and possible solutions.
* **Determine [which repository the problem should be reported
  in](#keyman-keyboard-layouts-and-lexical-models)**.
* **Perform a [cursory
  search](https://github.com/search?q=+is%3Aissue+user%3Akeymanapp+label%3Abug)**
  to see if the problem has already been reported. If it has **and the
  issue is still open**, add a comment to the existing issue instead of
  opening a new one.

#### How Do I Submit A (Good) Bug Report?

Bugs are tracked as [GitHub
issues](https://guides.github.com/features/issues/). After you've
determined [which
repository](#keyman-keyboard-layouts-and-lexical-models) your bug is
related to, create an issue on that repository and provide the following
information by filling in the template (not all repositories currently
have a bug reporting template; in those cases, you can start with the
[Keyman bug report
template](https://github.com/keymanapp/keyman/blob/master/.github/ISSUE_TEMPLATE/bug_report.md?plain=1)
if you wish).

Explain the problem and include additional details to help maintainers
reproduce the problem:

* **Use a clear and descriptive title** for the issue to identify the
  problem.
* **Describe the exact steps which reproduce the problem** in as many
  details as possible. When listing steps, **don't just say what you
  did, but explain how you did it**. For example, if you typed on
  Android using a bluetooth keyboard, instead of with the touch
  keyboard, note that.
* **Provide specific examples to demonstrate the steps** If you are
  documenting a problem with unexpected output from a keyboard, make
  sure you include the exact keystroke sequence you typed, the output
  you received, and the expected output.

* **Include screenshots and animated GIFs** which show you following the
  described steps and clearly demonstrate the problem. You can use [this
  tool](https://www.cockos.com/licecap/) to record GIFs on macOS and
  Windows, and [this tool](https://github.com/colinkeenan/silentcast) or
  [this tool](https://github.com/GNOME/byzanz) on Linux.

* **If you're reporting that Keyman crashed**, include the reference
  number from the crash report, if available.

Provide more context by answering these questions:

* **Did the problem start happening recently** (e.g. after updating to a
  new version of Keyman, keyboard, app or operating system) or was this
  always a problem?
* If the problem started happening recently, **can you reproduce the
  problem in an older version of Keyman?** What's the most recent
  version in which the problem doesn't happen? You can download older
  versions of Keyman from [the Downloads
  page](https://keyman.com/downloads).
* **Can you reliably reproduce the issue?** If not, provide details
  about how often the problem happens and under which conditions it
  normally happens.

Include details about your configuration and environment:

* **Which version of Keyman are you using?** The version information is
  typically available in the About or Info screen, depending on your
  device type.
* **What's the name and version of the OS you're using**? Note exactly
  which type of device you are using, the version of the operating
  system.
* **Are you running Keyman in a virtual machine?** If so, which VM
  software are you using and which operating systems and versions are
  used for the host and the guest?
* **Which keyboards and models do you have installed?** You can get that
  list by running `apm list --installed`.
* **What app are you interacting with?** If the problem is in
  interaction with a specific app, note the app you are trying to work
  with, and its version.
* **Which keyboard layout experiences the problem?** Capture the
  keyboard ID and the version of the keyboard layout. On Windows, tell
  us also the base keyboard layout, found in Keyman Configuration,
  Options tab.
* **Diagnostic reports** On Windows, you can capture a diagnostic report
  by clicking Diagnostics in the Support tab of Keyman Configuration,
  save it and attach it to the issue.

### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion for
Keyman, including completely new features and minor improvements to
existing functionality. Following these guidelines helps maintainers and
the community understand your suggestion :pencil: and find related
suggestions :mag_right:.

Before creating enhancement suggestions, please check [this
list](#before-submitting-an-enhancement-suggestion) as you might find
out that you don't need to create one. When you are creating an
enhancement suggestion, please [include as many details as
possible](#how-do-i-submit-a-good-enhancement-suggestion). Fill in [the
template](https://github.com/keymanapp/keyman/issues/new?assignees=&labels=feat&template=feature_request.md&title=feat%3A+),
including the steps that you imagine you would take if the feature
you're requesting existed.

#### Before Submitting An Enhancement Suggestion

* **Determine [which repository the enhancement should be suggested
  in](#keyman-keyboard-layouts-and-lexical-models).**
* **Perform a [cursory
  search](https://github.com/search?q=+is%3Aissue+user%3Akeymanapp+label%3Afeat)**
  to see if the enhancement has already been suggested. If it has, add a
  comment to the existing issue instead of opening a new one.

#### How Do I Submit A (Good) Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub
issues](https://guides.github.com/features/issues/). After you've
determined [which
repository](#keyman-keyboard-layouts-and-lexical-models) your
enhancement suggestion is related to, create an issue on that repository
and provide the following information:

* **Use a clear and descriptive title** for the issue to identify the
  suggestion.
* **Provide a step-by-step description of the suggested enhancement** in
  as many details as possible.
* **Provide specific examples to demonstrate the steps**. Include
  copy/pasteable snippets which you use in those examples, as [Markdown
  code
  blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the current behavior** and **explain which behavior you
  expected to see instead** and why.
* **Include screenshots and animated GIFs** which help you demonstrate
  the steps or point out the part of Keyman which the suggestion is
  related to. You can use [this tool](https://www.cockos.com/licecap/)
  to record GIFs on macOS and Windows, and [this
  tool](https://github.com/colinkeenan/silentcast) or [this
  tool](https://github.com/GNOME/byzanz) on Linux.
* **Explain why this enhancement would be useful** to most Keyman users.
* **List some other keyboard tools or operating systems where this
  enhancement exists.**
* **Specify which version of Keyman you're using.** The version
  information is typically available in the About or Info screen,
  depending on your device type.
* **Specify the name and version of the OS you're using.**

### Your First Code Contribution

Unsure where to begin contributing to Keyman? You can start by looking
through these `good-first-issue` and `help-wanted` issues:

* [Good first
  issues](https://github.com/keymanapp/keyman/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22)
  - issues which should only require a few lines of code, and a test or
    two.
* [Help wanted
  issues](https://github.com/keymanapp/keyman/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22)
  - issues which should be a bit more involved than `Good First Issues`
    issues.

### Building Keyman

Keyman can be built locally. For instructions on how to do this, see the
[build documentation](./docs/build/index.md).

### Localize Keyman

We use CrowdIn to develop translations for the Keyman user interface.

* [Localize Keyman](https://translate.keyman.com/project/keyman)
  [![Crowdin](https://badges.crowdin.net/keyman/localized.svg)](https://translate.keyman.com/project/keyman)

### Other Ways of Getting Involved

Even if you are not a coder, there are many other ways you can help with
the Keyman project:

* [Other Ways of Getting
  Involved](https://keyman.com/about/get-involved)

## Style Guides

### Principles of Keyman Code Changes

These are some general principles that we need to be following when we
create features or bug fixes in Keyman. These principles help to uphold
a good user experience for Keyman end users and keyboard/model
developers.

* [Principles of Keyman Code Changes
  Wiki](https://github.com/keymanapp/keyman/wiki/Principles-of-Keyman-Code-Changes)

###  Pull Request and Commit Workflow

We have a number of git commit and pull request formatting preferences.
We can review your pull request faster if you follow these guidelines:

* [Pull Request and Commit workflow
  wiki](https://github.com/keymanapp/keyman/wiki/Pull-Request-and-Commit-workflow-notes)

### Code Style Guide

The Keyman style guide will help you to prepare code in a way that
matches the Keyman source. Note that older source may not yet meet this
guide, so if you are fixing up an old piece of code, you may need to use
your judgment to determine the code styling to use.

* [Code Style Guide
  wiki](https://github.com/keymanapp/keyman/wiki/Keyman-Code-Style-Guide)

### User Testing

Any pull request that impacts end user experience should include a user
test. This is a set of instructions that an experienced user of Keyman
could follow to validate that your pull request does what it should do.
A good tester may depart from the test steps you provide to check the
things you forgot -- but you should try and provide comprehensive test
steps.

The Keyman app repository has a bot which coordinates user testing for
each pull request:

* [User Testing
  wiki](https://github.com/keymanapp/keyman/wiki/User-Testing-Workflows)

### Documentation Style Guide

This applies to user documentation found both on
[help.keyman.com](https://github.com/keymanapp/help.keyman.com) and the
application user documentation found in the [Keyman
repository](https://github.com/keymanapp/keyman). Remember that
application documentation is replicated to help.keyman.com but should be
edited in the Keyman repository.

* All new documentation pages should be written in
  [Markdown](https://daringfireball.net/projects/markdown).
* Older documentation may be written in HTML or a strange hybrid of PHP
  scripts and HTML. If you make more than a minor change to an older
  doc, rewrite it to Markdown:
    1. View Source on the page from your browser
    2. Find the documentation content (often within the `<article>` tag)
    3. Use a tool such as [Browserling HTML to
       Markdown](https://www.browserling.com/tools/html-to-markdown) to
       convert the document to Markdown
    4. Paste this into a new file with the same name but the .md
       extension, and remove the old .html or .php file.

The style guide for code documentation and comments can be found in the
[Code Style Guide
wiki](https://github.com/keymanapp/keyman/wiki/Keyman-Code-Style-Guide).

## Additional Notes

### Issue and Pull Request Labels

This section lists the labels we use to help us track and manage issues
and pull requests. These labels are used on the [Keyman app repo and the
website repos](#keyman-keyboard-layouts-and-lexical-models).

[GitHub search](https://help.github.com/articles/searching-issues/)
makes it easy to use labels for finding groups of issues or pull
requests you're interested in. The labels are loosely grouped by their
purpose, but it's not required that every issue has a label from every
group or that an issue can't have more than one label from the same
group.

#### Type of Issue

| Label name | `keyman` :mag_right: | Description |
| --- | --- | --- |
| `auto` | [search][search-auto] | For PRs only: automatically-opened PRs, e.g. opened by CI. |
| `bug` | [search][search-bug] | For issues only: confirmed bugs or reports that are very likely to be bugs. PRs use `fix` to mark a bug fix. |
| `change` | [search][search-change] | Minor change in functionality, but not new. |
| `chore` | [search][search-chore] | Cleanup work, maintenance, without change in functionality. |
| `docs` | [search][search-docs] | Relating to any type of documentation. |
| `feat` | [search][search-feat] | Feature requests. |
| `fix` | [search][search-fix] | For PRs only: a bug fix, corresponds to issue label `bug`. |
| `question` | [search][search-question] | Questions more than bug reports or feature requests (e.g. how do I do X). [**Usually better on the Keyman Community**](https://community.software.sil.org/c/keyman) |
| `refactor` | [search][search-refactor] | Code reorganization and refactoring, without change in functionality. |
| `spec` | [search][search-spec] | Issues which are specifications for a large scale feature. |
| `style` | [search][search-style] | Code formatting only. |
| `test` | [search][search-test] | Relating to automated tests. |

#### Issue State

| Label name | `keyman` :mag_right: | Description |
| --- | --- | --- |
| `requires-design-work` | [search][search-requires-design-work] | Issues which need design before they can proceed. |
| `help wanted` | [search][search-help-wanted] | The Keyman core team would appreciate help from the community in resolving these issues. |
| `good first issue` | [search][search-good-first-issue] | Less complex issues which would be good first issues to work on for users who want to contribute to Keyman. |
| `duplicate` | [search][search-duplicate] | Issues which are duplicates of other issues, i.e. they have been reported before. |
| `low-priority` | [search][search-low-priority] | The Keyman core team has decided not to fix these issues for now, but may in the future as time permits. |
| `wontfix` | [search][search-wontfix] | The Keyman core team has decided not to fix these issues for now, either because they're working as intended or for some other reason. |
| `invalid` | [search][search-invalid] | Issues which aren't valid (e.g. user errors). |
| `compatibility` | [search][search-compatibility] | Related to interactions with other applications. |
| `dependencies` | [search][search-dependencies] | Related to changes in dependencies, often automatically generated. |
| `external` | [search][search-external] | Related to issues that require changes to third party applications in order to be resolved. |

#### Platform and Feature Categories

These labels also include sublabels, such as `windows/config/`.

| Label name | `keyman` :mag_right: | Description |
| --- | --- | --- |
| `android/` | [search][search-android] | Related to Keyman running on Android. |
| `common/` | [search][search-common] | Related to shared code. |
| `core/` | [search][search-core] | Related to Keyman Core. |
| `developer/` | [search][search-developer] | Related to Keyman Developer. |
| `ios/` | [search][search-ios] | Related to Keyman running on iOS. |
| `linux/` | [search][search-linux] | Related to Keyman running on Linux. |
| `mac/` | [search][search-mac] | Related to Keyman running on macOS. |
| `oem/` | [search][search-oem] | Related to third party projects that are built together with Keyman. |
| `web/` | [search][search-web] | Related to KeymanWeb. |
| `windows/` | [search][search-windows] | Related to Keyman running on Windows. |

#### Pull Request Labels

| Label name | `keyman` :mag_right: | Description |
| --- | --- | --- |
| `cherry-pick` | [search][search-cherry-pick] | Pull requests which are essentially identical to another pull request, but for a different release version of Keyman. |
| `refactor` | [search][search-refactor] | Pull requests which include no functional changes but reorganise code. |
| `user-test-missing` | [search][search-user-test-missing] | Pull requests where no user tests have been defined. |
| `user-test-required` | [search][search-user-test-required] | Pull requests where user testing is incomplete. |


[search-auto]: https://github.com/keymanapp/keyman/labels/auto
[search-bug]: https://github.com/keymanapp/keyman/labels/bug
[search-change]: https://github.com/keymanapp/keyman/labels/change
[search-chore]: https://github.com/keymanapp/keyman/labels/chore
[search-docs]: https://github.com/keymanapp/keyman/labels/docs
[search-feat]: https://github.com/keymanapp/keyman/labels/feat
[search-fix]: https://github.com/keymanapp/keyman/labels/fix
[search-question]: https://github.com/keymanapp/keyman/labels/question
[search-spec]: https://github.com/keymanapp/keyman/labels/spec
[search-style]: https://github.com/keymanapp/keyman/labels/style
[search-test]: https://github.com/keymanapp/keyman/labels/test

[search-requires-design-work]: https://github.com/keymanapp/keyman/labels/requires-design-work
[search-help-wanted]: https://github.com/keymanapp/keyman/labels/help%20wanted
[search-good-first-issue]: https://github.com/keymanapp/keyman/labels/good%20first%20issue
[search-duplicate]: https://github.com/keymanapp/keyman/labels/duplicate
[search-low-priority]: https://github.com/keymanapp/keyman/labels/low-priority
[search-wontfix]: https://github.com/keymanapp/keyman/labels/wontfix
[search-invalid]: https://github.com/keymanapp/keyman/labels/invalid

[search-compatibility]: https://github.com/keymanapp/keyman/labels/compatibility
[search-dependencies]: https://github.com/keymanapp/keyman/labels/dependencies
[search-external]: https://github.com/keymanapp/keyman/labels/external

[search-android]: https://github.com/keymanapp/keyman/labels/android%2F
[search-common]: https://github.com/keymanapp/keyman/labels/common%2F
[search-core]: https://github.com/keymanapp/keyman/labels/core%2F
[search-developer]: https://github.com/keymanapp/keyman/labels/developer%2F
[search-ios]: https://github.com/keymanapp/keyman/labels/ios%2F
[search-linux]: https://github.com/keymanapp/keyman/labels/linux%2F
[search-mac]: https://github.com/keymanapp/keyman/labels/mac%2F
[search-oem]: https://github.com/keymanapp/keyman/labels/oem%2F
[search-web]: https://github.com/keymanapp/keyman/labels/web%2F
[search-windows]: https://github.com/keymanapp/keyman/labels/windows%2F

[search-cherry-pick]: https://github.com/keymanapp/keyman/labels/cherry-pick
[search-refactor]: https://github.com/keymanapp/keyman/labels/refactor
[search-user-test-missing]: https://github.com/keymanapp/keyman/labels/user-test-missing
[search-user-test-required]: https://github.com/keymanapp/keyman/labels/user-test-required
