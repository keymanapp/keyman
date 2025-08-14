# Controlling builds in CI with the Keyman build bot

The Keyman CI test infrastructure can be controlled by use of a `Build-bot:`
commit trailer, or a PR body `Build-bot:` trailer. This allows you to specify
what is built for any given platform. The primary purpose of the build bot is to
reduce the build agent load, but it can also be used to ensure that specific
artifacts are available for the purposes of testing.

The Keyman build bot is only available for test builds on pull requests. It is
not used for any other builds, either test builds on target branches (master,
beta, stable-x.y), or for release builds.

The default set of platform builds is determined by the files touched in the
pull request; see /resources/build/trigger-definitions.inc.sh. The default build
level for this set is 'build' (see [Build Level] section). This is known as the
'build set'.

You may choose to increase or decrease what is built -- for example, for a
documentation-only change, you may decide that nothing needs to be built at all,
with:

```
Build-bot: skip
```

Or you may want to ensure that an artifact is built for Windows:

```
Build-bot: release:windows
```

## The Build-bot command

The build bot is controlled through the `Build-bot` command, which can be put
into commit trailers or the PR body (not PR comments). The format of the command
is:

```
Build-bot: <BuildLevel>:[Platforms] ...
```

* `BuildLevel` can be `skip`, `build`, or `release`. See [Build Level] for more
  details.
* `Platforms` can be omitted, in which case the command applies to all
  previously-specified platforms (which is not equivalent to specifying `all` --
  if no platform is specified, it will only update platforms that were already
  in the build set). If specified, it must be a comma-separated list, without
  spaces, of one or more of the following platform identifiers:

  * `all`: apply to all platforms listed below
  * `android`
  * `developer`
  * `ios`
  * `linux`
  * `mac`
  * `web`
  * `windows`
  * `common`: build common components, on all three build platforms (note:
    `release` is equivalent to `build` buildLevel for `common`)
  * `core`: build Keyman Core, on all three build platforms, and also WASM
    (note: `release` is equivalent to `build` buildLevel for `core`)

The list of platforms can be found in the `available_platforms` variable in
trigger-definitions.inc.sh.

## Build Level

The build level specifies what we want to be run for a test build on a PR.

There is a bit of nomenclature overlap with a 'release build'. A 'release'
buildLevel for a 'test build' is roughly equivalent to what is performed in a
'release build', however, a test build is only ever uploaded to test endpoints
(i.e. TestFlight, Play Store 'test' streams), and never to *.keyman.com, or to
other release distribution endpoints.

The build level is controlled by the Build-bot commit trailer and PR body
Build-bot/Test-bot trailers. The default build level will be 'build'.

For target branch (master, beta, stable-x.y) builds, the build level will always
be 'build', and Build-bot: commit trailers are ignored.

### 'skip' build level

Don't do a build at all. This would be appropriate for documentation PRs, for
example, or changes only to comments in source files.

### 'build' build level

Build the code and run unit tests, but don't create artifacts. What this looks
like will vary from platform to platform, but there are some common things we
won't do:
* we won't upload artifacts to TeamCity or *.keyman.com
* we won't upload artifacts to any endpoint such as Play Store
* we won't upload symbols to Sentry
* we won't codesign

However, we _will_ still build an installer (skipping codesigning), as this is
part of the 'build' buildLevel rather than the 'release' buildLevel. The
installer will be thrown away for 'build' build level -- it will not be
available for download as an artifact.

For a platform-specific example, on macOS we will also skip notarizing, as this
is costly and depends on external network resources, making it fragile.

### 'release' build level

A full test build will be run, roughly equivalent to a release build. We will do
the following steps:
* codesign (many platforms) and notarization (macOS)
* upload artifacts to TeamCity (all platforms)
* upload builds to TestFlight / Play Store 'test' endpoints (iOS/Android)
* upload symbols to Sentry

For a 'release' build level:
* we won't upload artifacts to *.keyman.com
* we won't upload artifacts to any release endpoint such as Debian,
  packages.sil.org, etc, or to the release areas for Play Store or App Store

(uploading to *.keyman.com and to release endpoints happens in the release TC
build config/GHA)

## Controlling the build bot with trailers

The build bot respects commit trailers and trailers in the PR body. The commands
are cumulative and applied in order; the Build-bot trailer in the PR body is
applied after any trailers in commit messages.

If no platform is specified, then the command applies to all platforms in the
current build set, overriding any previous Build-bot commands. Any platforms not
in the current build set will remain 'skipped'.

If a platform is specified, then the command will apply only to that platform,
and the platform will be added to the build set if not already present (and
thus, subsequent Build-bot commands will be applied to this new platform as
well).

It is important to note that the Build-bot trailers are read when builds are
triggered, which happens within 2 minutes of a PR being opened or commits being
pushed. Editing the PR body after the build trigger has run will have no effect
on existing builds, until another commit is pushed, or the test build trigger is
run manually from TeamCity. Thus, do not rely on editing the PR body after PR
creation to control the build bot; either include your build bot trailers in
commit messages, or include them in the PR body submitted when creating the PR.

## Interactions with Test Bot

The Build-bot has limited interactions with the Keyman test bot (aka
keymanapp-test-bot): if a 'User Testing' section is found in the PR body, the
default build level will be upgraded from 'build' to 'release'.

Build bot trailers found in either the commits or in the PR body are applied
after the test bot command.

WARNING: The build bot does not check PR comments for Test-bot commands or a
'User Testing' section.

## Example Build-bot interactions

Say we have a PR that touches `/android/build.sh`. The default build set will be
`(android:build)`.

The PR body has a User Testing section: `# User Testing`. The build set is
upgraded to: `(android:release)`.

The first commit includes a Build-bot command: `Build-bot: build:ios`. The build
set is now `(android:release ios:build)`.

In a subsequent commit, the PR author decides that nothing needs to be built,
after all: `Build-bot: skip`. The build set is now `(android:skip ios:skip)`.
Note that other platforms are still 'skip' but not included in the build set.

Finally, the PR author pushes another commit, with `Build-bot: release:windows`.
The build set is now: `(android:skip ios:skip windows:release)`.

# FAQ

* How do I specify commands for multiple platforms, e.g. building on Windows and Linux?

```
Build-bot: build:windows,linux
```

* If I modify a file that would cause a build on all platforms, does `Build-bot:
  build android` then cause a build only on Android and skip all others? Or is
  it only additive?

The commands are additive. To skip all others, you would do:

```
Build-bot: skip
Build-bot: build:android
```

* What happens in the following scenario: I have a PR that I only want to build
  on Android and I add `Build-bot: build android`. Later on I merge in the
  changes from `master` and add another commit. If I still only want to build on
  Android, do I have to add `Build-bot: build android` on my new commit again?
  Or what happens with merges that touch other files which would cause builds
  for other platforms?

First, you would need to specify `skip` for all other platforms in order to only
build on Android:

```
Build-bot: skip
Build-bot: build:android
```

Then, the build bot scans all the commits in the PR, and additively builds the
final build set from the `Build-bot` commands from all the commits (up to 2000
in one PR). Thus, earlier bot commands commits will continue to be honoured.
This makes it easier to apply merge commits, for example, or small fixups,
without needing to think about what needs to be built again.

* How can I re-trigger builds after changing my build bot commands?

In [TeamCity - Trigger Test Builds](https://build.palaso.org/buildConfiguration/Keyman_Test),
select a custom Run (small button attached to right of Run button), and in the
"Changes" tab, "Build branch" field, enter the PR number to re-run the builds.
Any test builds currently running against that PR will be cancelled and
restarted.

Note: if you reduce the build set (with `skip`), and re-run builds in this way
without pushing a new commit, you will end up with stale status checks on the
latest commit in your PR for the builds that are now skipped on the new run, so
in this scenario it is safest to push a new commit with the updated build bot
commands.

[Build Level]: #Build_Level