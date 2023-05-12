# Unit tests for kmcmplib

**Note:** the keyboards repository tests only run if --full-test is passed to
build.sh.

kmcmplib tests include building all keyboards in the keyboards repository and
comparing against a 'known good' legacy compiler set. This is trickier than it
sounds because the legacy compiler only runs under Windows/WINE and we don't
want to introduce a dependency on that for running the tests.

Instead we've gone the route of adding a pre-compiled set of .kmx fixtures,
alongside a commit ref in keyboards_commit_ref.txt. The test script will
checkout that specific ref in its own private clone of the keyboards repository
(including a shallow clone of the keyboards repo if it isn't already present),
and compiling the sources from that specific commit.

For performance reasons, it is useful to setup KEYBOARDS_ROOT env var to point
to your local clone of the keyboards repository. The build will make a private
clone of this repo rather than the remote clone if it is available, which is
typically much faster.

# Preparing fixtures

prep.sh allows us to regenerate the fixtures with the legacy compiler for
testing. It depends on the regression test scripts in keyboards.sh, and specific
changes introduced into those scripts in
https://github.com/keymanapp/keyboards/pull/2169.

It is essential that the keyboards repository is clean and checked out to the
commit that you want to be using (usually this will be HEAD of master). All of
Keyman Developer must be built locally and binaries should be in developer/bin/.

prep.sh has only been tested on Windows, because it relies on kmcomp.exe.
