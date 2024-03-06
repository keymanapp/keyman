Keyman Developer - Command Line Compiler
================

This package provides a command-line interface to the Keyman Developer compiler
toolchain, `kmc`.

## Install kmc

`kmc` is available as:
* a part of Keyman Developer (Windows only)
* an npm package, and
* a zip download

Hint: Unlike previous versions of Keyman Developer, version 17 of kmc does not
require WINE to run the command line tools on Linux or macOS.

### Keyman Developer integration (Windows only)

kmc is included with a default installation of Keyman Developer, including a
runtime of node.js, and will be on the system path by default. No additional
configuration or installation is required.

### npm (Windows, macOS, and Linux)

kmc is also available as an npm package,
[@keymanapp/kmc](https://npmjs.com/package/@keymanapp/kmc).

You'll need [node.js](https://nodejs.org/), version 18.0 or later.

```shell
npm install -g @keymanapp/kmc
```

### Zip download (Windows, macOS, and Linux)

kmc is also available as a zip download from
[keyman.com/developer/download](https://keyman.com/developer/download),
or can be installed from the command line (`curl` and `unzip` required):

```shell
# To build keyboards and packages:
mkdir kmc
cd kmc
# hint: the download is currently called 'kmcomp', although the
#       compiler is now called 'kmc'.
curl -L https://keyman.com/go/download/kmcomp -o kmc.zip
unzip kmc.zip
# Optionally, add kmc to your PATH
```

## The five minute quick start

### 1. Download a sample keyboard project

<!-- TODO: THIS SECTION NEEDS REWRITE ONCE `kmc generate` lands in 18.0. -->

We'll download a sample project from GitHub for Khmer. If you do not have the
command-line git tools installed, you can visit the [repository
website](https://github.com/keyman-keyboards/khmer_angkor) and download it as a
zip file instead.

```shell
git clone https://github.com/keyman-keyboards/khmer_angkor
```

This will have created a new folder called `khmer_angkor/`.

### 2. Build the project

Now, we'll build our keyboard project with kmc.

```shell
cd khmer_angkor
kmc build .
```

And... that's it! We'll now have a compiled keyboard and package in the `build/`
subfolder. The file `build/khmer_angkor.kmp` can be installed into any of the
Keyman apps, and `build/khmer_angkor.js` can be added to KeymanWeb.

### 3. Install the keyboard

**Windows**: we can install this keyboard using [`kmshell`][windows-install-keyboard]:

```cmd
"%ProgramFiles(x86)%\Keyman\Keyman Desktop\kmshell" -i build\khmer_angkor.kmp -s
```

Alternatively you can double-click the .kmp package file in Windows Explorer to
install it.

**Linux**: we'd use the following
[`km-package-install`][linux-install-keyboard]
command:

```shell
km-package-install -f build/khmer_angkor.kmp
```

**macOS**: open Keyman Configuration and drop the package khmer_angkor.kmp file
onto the Keyman Configuration window.

**Android**: send khmer_angkor.kmp to your Android device, and [install it](https://help.keyman.com/products/android/current-version/basic/installing-custom-packages) from the
hamburger menu in the Keyman app.

**iOS**: send khmer_angkor.kmp to your iOS device, and install it from the
hamburger menu in the Keyman app.

**Web**: copy khmer_angkor.js to your website, then [load it with KeymanWeb][load-keymanweb-keyboard]:

```js
keyman.addKeyboards({
  id:'khmer_angkor',        // The keyboard's unique identification code.
  name:'Khmer Angkor',      // The keyboard's user-readable name.
  language:{
    id:'km',                // A BCP 47 code uniquely identifying the language.
    name:'Khmer'            // The language's name.
  },
  filename:'./khmer-angkor.js',
});
```

## File layout

See [file layout][file-layout] for details on the standard source file layout
that Keyman Developer works best with.

## Reference and Examples

### kmc - command line compiler

[kmc][kmc] is the command line compiler. You can use it to compile
all Keyman files.

The most common command will be `kmc build`:

`kmc build project.kpj`
: Compile all components of a keyboard or model project named `project.kpj`
KMComp will respect the path settings within the project file. This is the
recommended way to build, as it will build keyboards, models and packages all in
one step. You can also call `kmc build <folder>` to build the project in the
referenced folder, e.g. `kmc build .`.

* [kmc reference][kmc]

-----

# Working with the source

## Building kmc

Run `build.sh`:

```shell
./build.sh configure build
```

Once you have run `configure` once, you should not normally need to do it again
unless dependencies change or you clean the build folder. `./build.sh` without
parameters will do the default action, which is `build`.

## Testing kmc

```shell
./build.sh test
```

## Bundling for installation

```shell
./build.sh bundle --build-path <temp_path>
```

The temp_path must be a path outside the repository to avoid npm getting
confused by the root package.json. This is called by inst/download.in.mak
normally when building the Keyman Developer installer.

## Publishing to NPM

```shell
./build.sh publish [--dry-run]
```

Publishes the current release to NPM. This should only be run from CI.


[kmc]: https://help.keyman.com/developer/current-version/reference/kmc/cli
[file-layout]: https://help.keyman.com/developer/current-version/reference/file-layout
[load-keymanweb-keyboard]: https://help.keyman.com/developer/engine/web/current-version/guide/adding-keyboards
[linux-install-keyboard]: https://help.keyman.com/products/linux/current-version/reference/km-package-install
[windows-install-keyboard]: https://help.keyman.com/knowledge-base/98
