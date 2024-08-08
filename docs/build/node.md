# Node use in the Keyman project

For a given release cycle, we select and pin to a specific version of node.js,
and try to use that throughout. The version in use can be found in
`package.json/engines.node`.

If we encounter a blocking issue, we will upgrade to a known-good release, but
this is exceptional.

Generally, as a developer, if you use the same major version as is found in
package.json, you probably won't have significant issues. However, if you are
switching between stable branches and alpha branches, you may find the required
node version changes.

## Automatic node version selection

The Keyman build system can be configured to manage the node.js version for you.
This does have several caveats, so it is not enabled by default.

When this is enabled, the build system will download the required node version
and make it available on the PATH, and it will also stop the build if the node
version mismatches. This happens in `build.sh configure` steps for any
Typescript/Javascript project.

To enable automatic node version selection, add the variable `KEYMAN_USE_NVM=1`
to your environment.

### Caveats

nvm (for macOS/Linux) and nvm-windows use somewhat different paradigms, so the
caveats vary per platform.

### Caveats on macOS/Linux

On macOS/Linux, nvm is provided as a shell function that modifies the current
environment PATH to make a specific node version available -- and so this does
not affect unrelated processes.

1. You must use [nvm](https://github.com/nvm-sh/nvm) (macOS/Linux) to install
   and manage Node versions.
2. The Keyman build environment will create a symlink at `~/.keyman/node`
   pointing to the version of node selected by nvm.
3. You should add `$HOME/.keyman/node` to the front of your `PATH` variable,
   e.g. in `~/.bash_profile`. This means that the node version _will_ be set for
   the entire system, unlike with standard nvm usage. This allows build scripts
   to run without calling `nvm` for each invocation.

### Caveats on Windows

On Windows, nvm-windows creates a symlink to the current node version, and
this symlink is what is on the system PATH, so it affects all processes on the
system.

1. You must use [nvm-windows](https://github.com/coreybutler/nvm-windows) to
   install and manage Node versions.
2. By default on Windows, creating symlinks requires elevation. While
   nvm-windows does this for you, it can be irritating to have a build script
   pause for elevation. In Local Group Policy Editor, `Computer Configuration`,
   `Windows Settings`, `Security Settings`, `Local Policies`,
   `User Rights Assignment`, you can add your username to the
   `Create symbolic links` policy (and reboot), to avoid elevation.

## Build Agents

The Keyman build agents use nvm as described above, including the caveats.

## Implementation

See `_select_node_version_with_nvm()` in
`/resources/build/shellHelperFunctions.sh`, and
`/resources/build/_builder_nvm.sh`.