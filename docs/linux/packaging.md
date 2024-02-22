# Linux packaging

## Package distribution

We use different channels to build and distribute the Linux packages:

- (older version) included in official repos since Ubuntu 19.04 and Debian Buster
- Launchpad repo for [stable](https://launchpad.net/~keymanapp/+archive/ubuntu/keyman),
  [beta](https://launchpad.net/~keymanapp/+archive/ubuntu/keyman-beta) and
  [alpha](https://launchpad.net/~keymanapp/+archive/ubuntu/keyman-alpha) versions
- [pso](http://packages.sil.org/) and [llso](http://linux.lsdev.sil.org/ubuntu/)
  for stable, beta, and alpha versions
- artifacts on [GitHub](https://github.com/keymanapp/keyman/actions/workflows/deb-packaging.yml)
  for pull requests

Packages on [llso](http://linux.lsdev.sil.org/ubuntu/) are uploaded automatically and are
intended as a staging environment for testing. Packages on [pso](http://packages.sil.org/)
are uploaded manually after the packages received some testing. End users usually have only
pso enabled.

## Package builds

Package builds happen on [Launchpad](#package-builds-on-launchpad) and
[GitHub](#github-actions-package-builds). Package builds for the official Ubuntu/Debian
repos happen outside of our control. However, we
[upload source packages](#uploading-debian-source-packages) to the Debian community.

## GitHub Actions package builds

### Build jobs

The [Keyman GitHub repo](https://github.com/keymanapp/keyman) contains various
scripts that are used to trigger a build and as part of the package build,
and of course the source code for the packages:

- [.github/workflows/deb-packaging.yml](https://github.com/keymanapp/keyman/blob/master/.github/workflows/deb-packaging.yml)
  contains the definition of the packaging GHA
- [resources/build/run-required-test-builds.sh](https://github.com/keymanapp/keyman/blob/master/resources/build/run-required-test-builds.sh)
  runs on [TeamCity](https://build.palaso.org/buildConfiguration/Keyman_Test?)
  to trigger the builds for the various platforms, among them the GHA package build.
- [resources/build/increment-version.sh](https://github.com/keymanapp/keyman/blob/master/resources/build/increment-version.sh)
  runs on [TeamCity](https://build.palaso.org/buildConfiguration/Keyman_TriggerReleaseBuildsMaster?)
  and increments the version number before triggering the builds for the
  various platforms.
- The [linux/scripts](https://github.com/keymanapp/keyman/tree/master/linux/scripts)
  subdirectory contains `bash` scripts that are used during the package build.
  Some are only needed for Launchpad builds.

  - [deb-packaging.sh](https://github.com/keymanapp/keyman/blob/master/linux/scripts/deb-packaging.sh)
    gets called by the packaging GHA to install dependencies, create the source
    package and to verify the API.

- [linux/debian](https://github.com/keymanapp/keyman/tree/master/linux/debian) -
  this is the `debian` subdirectory for Keyman for Linux with the meta data
  for the Linux package.
  See [Debian New Maintainers' Guide](https://www.debian.org/doc/manuals/maint-guide/)
  for details to the various files in the `debian` directory.

### Flow of a Linux package build

- TeamCity jobs [Keyman_Test](https://build.palaso.org/buildConfiguration/Keyman_Test)
  or [Keyman_TriggerReleaseBuilds*](https://build.palaso.org/buildConfiguration/Keyman_TriggerReleaseBuildsBeta)
  trigger a packaging GHA build
- packaging GHA calls [deb-packaging.sh](https://github.com/keymanapp/keyman/blob/master/linux/scripts/deb-packaging.sh)
  which installs dependencies and creates the source package
- packaging GHA creates the binary package for each linux package on each
  distribution
- packaging GHA verifies that the API didn't change with the help of
  [deb-packaging.sh](https://github.com/keymanapp/keyman/blob/master/linux/scripts/deb-packaging.sh)
- at the end of the build if it is not a build of a PR, the `.deb` files get
  uploaded to llso (alpha packages to e.g. `jammy-experimental`, beta
  packages to `jammy-proposed` and packages build from the stable branch
  to the main section `jammy`)
- if the build is successful the job archives the artifacts

### Local package builds with Docker

It is possible to use the usual Debian/Ubuntu tools to create the package locally.
For someone who only occasionally deals with packaging it might be easier to use
Docker and the scripts that run on GitHub actions:

#### Prerequisites for local package builds with Docker

You'll have to create the docker image.

- clone [gha-ubuntu-packaging](https://github.com/sillsdev/gha-ubuntu-packaging)
  repo
- create the image:

  ```bash
  cd /path/to/gha-ubuntu-packaging
  docker build --build-arg DIST=jammy --build-arg PLATFORM=amd64 -t sillsdev/jammy .
  ```

#### Building packages with Docker

- create the source package

  ```bash
  cd $KEYMAN_ROOT
  TIER=$(cat TIER.md)
  export TIER
  cd linux
  ./scripts/deb-packaging.sh source
  ```

  This will create the source package in $KEYMAN_ROOT directory.

- Create the binary packages with Docker:

  ```bash
  cd $KEYMAN_ROOT
  DIST=jammy
  docker run -v $(pwd):/source -i -t -w /source --platform=amd64 \
    --env INPUT_DIST=$DIST --env INPUT_SOURCE_DIR=. \
    --env INPUT_SOURCEPACKAGE=$(ls keyman_*.dsc | sort | tail -1) \
    sillsdev/$DIST
  ```

  This will create the binary packages in `$KEYMAN_ROOT/artifacts`.

## Package builds on Launchpad

Package builds on Launchpad are triggered manually by running the Keyman script
[linux/scripts/launchpad.sh](https://github.com/keymanapp/keyman/blob/master/linux/scripts/launchpad.sh).

### Prerequisites for Launchpad

1. If you don't have one, create an account at [launchpad.net](https://launchpad.net)
2. Request to join the ["Keyman for Linux"](https://launchpad.net/~keymanapp) team.
3. Create a [GPG](https://help.ubuntu.com/community/GnuPrivacyGuardHowto) key
   and associate it to your launchpad account
4. Set the following environment variables in your `~/.profile` or `~/.bashrc`
   (so you don't have to set them every time)

    ```bash
    export GPGKEY=[key_id] # using the `key_id` of your GPG key
    export DEBEMAIL="your.email.address@example.org"
    export DEBFULLNAME="Firstname Lastname"
    ```

### Building packages on Launchpad

The `launchpad.sh` script downloads the current source code (beta or stable) from
[downloads.keyman.com](https://downloads.keyman.com/linux/stable/), creates a
Debian source package and uploads this to launchpad. Launchpad then rebuilds
for the different distros and architectures.

To upload the packages to launchpad, run the following script from the `linux/` directory:

```bash
./scripts/launchpad.sh [UPLOAD="yes"] [TIER="<tier>"] [PROJECT="<project>"] [DIST="<dist>"] [PACKAGEVERSION="<version>"]
```

#### Parameters

- `UPLOAD="yes"` - do the dput for real
- `TIER="<tier>"` - alpha, beta, or stable, default from `../TIER.md`
- `PROJECT="<project>"` - only upload this package
- `DIST="<dist>"` - only upload for this distribution
- `PACKAGEVERSION="<version>"` - normally use the default so don't specify
  it. But if you change packaging and run another upload you need to increment
  the number at the end of `PACKAGEVERSION`. e.g. next one is `1~sil2` then
  `1~sil3`…

### Releasing a new version

As part of releasing a new version it might be good to do some local testing
first before uploading to Launchpad:

- Run `launchpad.sh` with `UPLOAD="no"` to build the packages
- Then install them on a clean VM and make sure no glaring bugs
- Once you are happy with the packages you can run `launchpad.sh` with `UPLOAD="yes"`

### Troubleshooting

Refer to the [launchpad uploading help](https://help.launchpad.net/Packaging/PPA/Uploading)
for troubleshooting and setting up for `dput` upload.

## Uploading Debian source packages

Unless you're a Debian maintainer you can't directly upload to the Debian repos.
Instead you upload to <mentors.debian.net> and then look for a sponsor who will
review the packages and upload them for you. Be prepared that this might take some
persistence, and if somebody looks at it, it might take some iterations to get it
accepted.

The Keyman packages are maintained on the Debian side by the
[Debian Input Method Team](https://wiki.debian.org/Teams/IMEPackagingTeam).

**NOTE:** All `changelog` files should contain the exact same entry that was
previously accepted into the Debian repo (plus the new entry for the new
update). This means that when your upload got accepted into Debian (not
<mentors.debian.net>) you'll have to update the `changelog` files to match
what got accepted (sometimes the Debian maintainers will create additional
package versions).

### Prerequisites

- an account on [mentors.debian.net](https://mentors.debian.net/accounts/register/)
- an entry for `mentors` in your `.dput.cf` file:

  ```bash
  [mentors]
  fqdn = mentors.debian.net
  incoming = /upload
  method = https
  allow_unsigned_uploads = 0
  progress_indicator = 2
  # Allow uploads for UNRELEASED packages
  allowed_distributions = .*
  ```

- subscribe to the [debian-input-method](https://lists.debian.org/debian-input-method/)
  mailing list

### Updating and uploading a stable release to Debian

To do this you can run the `linux/scripts/upload-to-debian.sh` script:

```bash
linux/scripts/upload-to-debian.sh -k ${DEBSIGNKEY} --push
```

This does several steps:

1. Download the source code from <download.keyman.com> and create the source
   package by running `scripts/debian.sh`
2. sign the source package
3. upload to mentors (unless `-n` is passed)
4. Create a branch with the updated `linux/debian/changelog` file based on the
   stable branch
5. Cherry-pick the change on a new branch based on `master`
6. If `--push` is passed, the two branches will be pushed to GitHub

There are a few additional manual required steps:

1. Create a draft-PR for the change against stable branch
2. Create a draft PR for the cherry-picked change against `master`
3. file a RFS bug (Request For Sponsorship) against the `sponsorship-requests`
   pseudo-package, cc'ing `debian-input-method`, or just send an email to the
   `debian-input-method` list.

After the package got published in Debian you can mark the PRs as ready
for review. This should only be done after the package got published in
Debian because the changelog file needs to contain the exact same
information that the changelog in Debian has.

## Reference

See the [Linux readme](https://github.com/keymanapp/keyman/blob/master/docs/linux/README.md)
for how to build Keyman on Linux etc.

### References for Debian packaging

- [mentors intro](https://mentors.debian.net/intro-maintainers/), especially section
  3 (Publish your package)
- explanation of the [sponsoring process](https://mentors.debian.net/sponsors/)
- [personal package upload page](https://mentors.debian.net/packages/my/)
