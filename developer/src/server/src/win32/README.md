# External modules

These folders are clones of win32 libraries for node addons, where we have done
binary builds for specific node versions to bundle.

build_addins.inc.sh will build these node addons. For now we are not running
these out of separate repositories, to simplify the build process and management.

The build will attempt to build both x86 and x64 versions of the addins, using
first the globally installed x64 version of node, and then the x86 version of
node in /developer/src/server/inst/dist/. This x86 version is the one that is
bundled with Keyman Developer.
