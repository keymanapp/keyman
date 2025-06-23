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
Build-bot: release windows
```

## Build Level

The build level specifies what we want to be run for a test build on a PR.

There is a bit of nomenclature overlap with a 'release build'. A 'release'
buildLevel for a 'test build' is roughly equivalent to what is performed in a
'release build', however, a test build is only ever uploaded to test endpoints
(i.e. TestFlight, Play Store 'test' streams), and never to *.keyman.com, or to
other release distribution endpoints.

The build level is controlled by the Build-bot commit trailer and PR body
Build-bot/Test-bot trailers. The default build level will be 'build'.

For target branch builds, the build level will always be 'build', and Build-bot:
commit trailers are ignored.

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

(uploading to *.keyman.com and to release endpoints happens in the release TC build config/GHA)

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

Build bot trailers found in the commits or in the PR body are applied after the
test bot command.

WARNING: The build bot does not check PR comments for Test-bot commands.

## Example Build-bot interactions

Say we have a PR that touches `/android/build.sh`. The default build set will be
`(android:build)`.

The PR body has a User Testing section: `# User Testing`. The build set is
upgraded to: `(android:release)`.

The first commit includes a Build-bot command: `Build-bot: build ios`. The build
set is now `(android:release ios:build)`.

In a subsequent commit, the PR author decides that nothing needs to be built,
after all: `Build-bot: skip`. The build set is now `(android:skip ios:skip)`.
Note that other platforms are still 'skip' but not included in the build set.

Finally, the PR author pushes another commit, with `Build-bot: release windows`.
The build set is now: `(android:skip ios:skip windows:release)`.



[Build Level]: #Build_Level