# External modules

These folders are clones of win32 libraries for node addons, where we have done
binary builds for specific node versions to bundle and avoid conflicts with
differing versions of node on build machines.

The target versions of node are:

* ia32:  node 12 (matches version in developer/inst/dist)
* x64:   node 17 (matches current team environment)

If we update the node.exe version in developer/inst/dist, we will need to
rebuild the modules accordingly.

To build Win32 version using Keyman Developer version of node, for deployment:

```
  # git clone <project>
  # cd <proect>
  set PATH=%KEYMAN_ROOT%\developer\inst\dist;%path%
  npx node-gyp clean configure build --verbose --arch=ia32
  # copy build/Release/<file.node> to destination/file.node
```

To build x64 version using current environment version of node:

```
  # git clone <project>
  # cd <proect>
    npx node-gyp clean configure build --verbose --arch=x64
  # copy build/Release/<file.node> to destination/file.x64.node
```

While this could probably be managed through build scripts, it is a little
fragile, so for now, keeping it manual.
