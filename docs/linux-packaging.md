# Linux packaging

## Package distribution

We use different channels to build and distribute the Linux packages:

- (older version) included in official repos since Ubuntu 19.04 and Debian Buster
- Launchpad repo for [stable](https://launchpad.net/~keymanapp/+archive/ubuntu/keyman),
  [beta](https://launchpad.net/~keymanapp/+archive/ubuntu/keyman-beta) and
  [alpha](https://launchpad.net/~keymanapp/+archive/ubuntu/keyman-alpha) versions
- [pso](http://packages.sil.org/) and [llso](http://linux.lsdev.sil.org/ubuntu/)
  for stable, beta, and alpha versions
- artifacts on [Jenkins](https://jenkins.lsdev.sil.org/view/Keyman/view/Pipeline/job/pipeline-keyman-packaging/view/change-requests/)
  for pull requests

Packages on [llso](http://linux.lsdev.sil.org/ubuntu/) are uploaded automatically and are
intended as a staging environment for testing. Packages on [pso](http://packages.sil.org/)
are uploaded manually after the packages received some testing. End users usually have only
pso enabled.

## Package builds

Package builds happen on Launchpad and Jenkins. Package builds for the official Ubuntu/Debian
repos happen outside of our control.

## Package builds on Jenkins

### Build jobs

The definition of the packaging jobs, the triggering of the jobs and the necessary build scripts
are scattered over several source repos:

- [ci-builder-scripts](https://github.com/sillsdev/ci-builder-scripts) contains the definition of
  a meta job (multi-branch pipeline job) that gets triggered when a change gets pushed to the
  [Keyman GitHub repo](https://github.com/keymanapp/keyman). The meta job creates a new build
  configuration/job for each branch/pull request on GitHub. The new job gets triggered to initialize
  itself, but then exits immediately. We use the Jenkins
  [Job DSL plugin](https://github.com/jenkinsci/job-dsl-plugin/wiki) to define the meta job.

  ci-builder-scripts also contains several generic scripts to set up a package build environment
  (using `sbuilder`) and for building source and binary packages. These scripts are shared with
  other projects.

  The Keyman GitHub repo defines a webhook that triggers the meta job on Jenkins.

  Changes to ci-builder-scripts go through [Gerrit](https://gerrit.lsdev.sil.org). See
  [CONTRIBUTING.md](https://github.com/sillsdev/ci-builder-scripts/blob/master/CONTRIBUTING.md)
  for details.

  File structure:

  - [groovy/KeymanPackagingJobs.groovy](https://github.com/sillsdev/ci-builder-scripts/blob/master/groovy/KeymanPackagingJobs.groovy)
    contains the meta job definition
  - The [bash/](https://github.com/sillsdev/ci-builder-scripts/tree/master/bash) subdirectory
    contains `bash` scripts:

    - [setup.sh](https://github.com/sillsdev/ci-builder-scripts/blob/master/bash/setup.sh) -
      setup sbuild chroot environment
    - [update](https://github.com/sillsdev/ci-builder-scripts/blob/master/bash/update) -
      update the sbuild chroot environment
    - [build-package](https://github.com/sillsdev/ci-builder-scripts/blob/master/bash/build-package) -
      create a binary package

- [lsdev-pipeline-library](https://github.com/sillsdev/lsdev-pipeline-library) contains a reusable
  Jenkins pipeline library. The
  [vars/keymanPackaging.groovy](https://github.com/sillsdev/lsdev-pipeline-library/blob/master/vars/keymanPackaging.groovy)
  file contains the bulk of the logic of the Keyman packaging job.

- The [Keyman GitHub repo](https://github.com/keymanapp/keyman) contains various scripts that are
  used to trigger a build and as part of the package build, and of course the source code for the
  packages:

  - [resources/build/run-required-test-builds.sh](https://github.com/keymanapp/keyman/blob/master/resources/build/run-required-test-builds.sh)
    runs on [TeamCity](https://build.palaso.org/buildConfiguration/Keyman_Test?) to trigger the
    builds for the various platforms, among them the Jenkins package build.
  - [resources/build/increment-version.sh](https://github.com/keymanapp/keyman/blob/master/resources/build/increment-version.sh)
    runs on [TeamCity](https://build.palaso.org/buildConfiguration/Keyman_TriggerReleaseBuildsMaster?)
    and increments the version number before triggering the builds for the various platforms.
  - [linux/Jenkinsfile](https://github.com/keymanapp/keyman/blob/master/linux/Jenkinsfile) is a flag
    for the meta job. If the meta job finds this file, it will create a new build configuration. This
    file simply calls the packaging functionality defined in `lsdev-pipeline-library` and passes the
    distributions and architectures to build as parameters.
  - [linux/build/agent/install-deps](https://github.com/keymanapp/keyman/blob/master/linux/build/agent/install-deps)
    installs dependencies on the current build agent.
  - The [linux/scripts](https://github.com/keymanapp/keyman/tree/master/linux/scripts) subdirectory
    contains `bash` scripts that are used during the package build. Some are only needed for
    Launchpad builds.

    - [jenkins.sh](https://github.com/keymanapp/keyman/blob/master/linux/scripts/jenkins.sh)
      gets called from `lsdev-pipeline-library` to create a source package.

  - [linux/debian](https://github.com/keymanapp/keyman/tree/master/linux/debian) - this is the `debian`
    subdirectory for Keyman for Linux with the meta data for the Linux package. The legacy
    kmfl-related packages have separate debian subdirectories under `linux/*/debian`.
    See [Debian New Maintainers' Guide](https://www.debian.org/doc/manuals/maint-guide/) for
    details to the various files.

### Flow of a Linux package build

- TeamCity jobs [Keyman_Test](https://build.palaso.org/buildConfiguration/Keyman_Test) or
  [Keyman_TriggerReleaseBuilds*](https://build.palaso.org/buildConfiguration/Keyman_TriggerReleaseBuildsBeta)
  trigger a build on [Jenkins](https://jenkins.lsdev.sil.org/view/Keyman/view/Pipeline/job/pipeline-keyman-packaging/)
- Jenkins verifies the build parameters and starts the matching build configuration for the PR or
  branch
- The [build job](https://github.com/sillsdev/lsdev-pipeline-library/blob/master/vars/keymanPackaging.groovy) runs several checks:

  - it exits immediately if the build is not manually triggered and no parameters are passed in
    (i.e. it got triggered by the GitHub webhook)
  - it doesn't build if this is a PR, didn't get triggered manually and the PR is not from a trusted
    user
  - it doesn't build if no Linux-relevant files changed unless the parameter `force` was passed
  - manually triggered builds will always build

- build job installs
  [dependencies](https://github.com/keymanapp/keyman/blob/master/linux/build/agent/install-deps)
  on the current build agent
- build job creates a source package for the linux packages (keyman, kmflcomp,
  libkmfl, and ibus-kmfl). This is done by calling
  [scripts/jenkins.sh](https://github.com/keymanapp/keyman/blob/master/linux/scripts/jenkins.sh).
- build job creates the binary package for each linux package on each distribution (currently
  bionic, focal, and groovy) and each architecture (amd64, i386 only for bionic)
- at the end of the build if it is not a build of a PR, the `.deb` file gets uploaded to llso
  (alpha packages to e.g. `bionic-experimental`, beta packages to `bionic-proposed` and
  packages build from the stable branch to the main section `bionic`)
- if the build is successful the job archives the artifacts

The Jenkins build progress is visible in two ways:

- [traditional view](https://jenkins.lsdev.sil.org/view/Keyman/view/Pipeline/job/pipeline-keyman-packaging/)
- [blue ocean view](https://jenkins.lsdev.sil.org/blue/organizations/jenkins/pipeline-keyman-packaging/activity)

**Note:** TC release builds pass the git tag to build to the Jenkins job. The same tag
gets passed twice as parameters `tag` and `tag2`. The first parameter gets persisted between
builds, allowing to retrigger a tag-build. The second parameter is necessary to distinguish
if this is a retriggered build of a tag-build.

### Local package builds

It is possible to use the usual Debian/Ubuntu tools to create the package locally. For someone who
only occasionally deals with packaging it might be easier to use the scripts that Jenkins runs:

#### Prerequisites for local package builds

Install `sbuild` (and probably some other packages that I forgot).

You’ll need a chroot image before you can use sbuild. The scripts in
[ci-builder-scripts](https://github.com/sillsdev/ci-builder-scripts) will help
with that. [`setup.sh`](https://github.com/sillsdev/ci-builder-scripts/blob/master/bash/setup.sh)
can setup such chroots:

```bash
bash/setup.sh --dists "focal bionic" --arches "amd64 i386"
```

[`update`](https://github.com/sillsdev/ci-builder-scripts/blob/master/bash/update) is used to
later update those chroots:

```bash
bash/update --dists "focal bionic" --arches "amd64 i386"
```

Set the `DEBSIGNKEY` environment variable to your public GPG key that will be used to sign
the packages.

#### Building packages

Building packages happen in the [Keyman source tree](https://github.com/keymanapp/keyman).

The Keyman
[`linux/scripts/jenkins.sh`](https://github.com/keymanapp/keyman/blob/master/linux/scripts/jenkins.sh)
script can be used to create a source package (replace `packageName` with the name of the package,
i.e. one of keyman, kmflcomp, libkmfl, and ibus-kmfl).

```bash
cd linux
./scripts/jenkins.sh ${packageName} ${DEBSIGNKEY}
```

This creates a source package (`<packageName>_<version>-1.dsc`) and some `*.tar.?z` files in the
`linux/legacy/<packageName>` subdirectory, respective in the source root directory for `keyman`.

ci-builder-script's [`build-package`](https://github.com/sillsdev/ci-builder-scripts/blob/master/bash/build-package)
script creates the binary packages:

```bash
cd linux/legacy/${packageName}
~/ci-builder-scripts/bash/build-package \
    --dists "focal bionic" --arches "amd64 i386" \
    --debkeyid ${DEBSIGNKEY} --build-in-place --no-upload
```

This will create the binary package `<packageName>_<version>-1+<dist>1_<arch>.deb`.

To speed up package building you might want to limit the build to a single dist
(e.g. `--dists "bionic"`) and arch (e.g. `--arches "amd64"`).

After building packages it might be a good idea to clean up the source tree before doing further
work:

```bash
git clean -dxf
```

## Package builds on Launchpad

Package builds on Launchpad are triggered manually by running the Keyman script
[linux/scripts/launchpad.sh](https://github.com/keymanapp/keyman/blob/master/linux/scripts/launchpad.sh).

### Prerequisites for Launchpad

1. If you don't have one, create an account at [launchpad.net](https://launchpad.net)
2. Request to join the ["Keyman for Linux"](https://launchpad.net/~keymanapp) team.
3. Create a [GPG](https://help.ubuntu.com/community/GnuPrivacyGuardHowto) key and associate it
   to your launchpad account
4. Set the following environment variables in your `~/.profile` or `~/.bashrc` (so you don't have
   to set them every time)

    ```bash
    export GPGKEY=[key_id] # using the `key_id` of your GPG key
    export DEBEMAIL="your.email.address@example.org"
    export DEBFULLNAME="Firstname Lastname"
    ```

### Building packages on Launchpad

The `launchpad.sh` script downloads the current source code (beta or stable) from
[downloads.keyman.com](https://downloads.keyman.com/linux/stable/), creates a Debian source package
and uploads this to launchpad. Launchpad then rebuilds for the different distros and architectures.

To upload the packages to launchpad, run the following script from the `linux/` directory:

```bash
./scripts/launchpad.sh [UPLOAD="yes"] [TIER="<tier>"] [PROJECT="<project>"] [DIST="<dist>"] [PACKAGEVERSION="<version>"]
```

#### Parameters

- `UPLOAD="yes"` - do the dput for real
- `TIER="<tier>"` - alpha, beta, or stable, default from `../TIER.md`
- `PROJECT="<project>"` - only upload this package
- `DIST="<dist>"` - only upload for this distribution
- `PACKAGEVERSION="<version>"` - normally use the default so don't specify it. But if you
  change packaging and run another upload you need to increment the number at the end of
  `PACKAGEVERSION`. e.g. next one is `1~sil2` then `1~sil3`…

### Releasing a new version

As part of releasing a new version it might be good to do some local testing first before uploading
to Launchpad:

- Run `launchpad.sh` with `UPLOAD="no"` to build the packages
- Then install them on a clean VM and make sure no glaring bugs
- Once you are happy with the packages you can run `launchpad.sh` with `UPLOAD="yes"`

### Troubleshooting

Refer to the [launchpad uploading help](https://help.launchpad.net/Packaging/PPA/Uploading)
for troubleshooting and setting up for `dput` upload.

## Reference

See the [Linux readme](https://github.com/keymanapp/keyman/blob/master/linux/README.md)
for how to build Keyman on Linux etc.
