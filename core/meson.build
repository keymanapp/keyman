# Copyright:    © 2018-2022 SIL International.
# Description:  Cross platform build script to compile libkeymancore, documentation
#               and tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

project('keyman_core', 'cpp', 'c',
        version: run_command(find_program('getversion.bat', 'getversion.sh'), check:true).stdout().strip(),
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++17',
                           'b_vscrt=static_from_buildtype',
                           'warning_level=2',
                           'debug=true'],
        meson_version: '>=0.57.0')

# Import our standard compiler defines; this is copied from
# /resources/build/standard.meson.build by build.sh, because
# meson doesn't allow us to reference a file outside its root
subdir('resources')

fs = import('fs')
lib_version = fs.read('CORE_API_VERSION.md').strip()

py = import('python')
python = py.find_installation()

# TODO: Shared includes may use namespaces, etc which need future tidyup.
# For now, we use KM_CORE_LIBRARY to inject the km::core::kmx namespace
defns += ['-DKM_CORE_LIBRARY']

# #define DEBUG when we are on a debug build
if get_option('buildtype') == 'debug'
  add_global_arguments('-DDEBUG', language : 'cpp')
endif

# shared with a number of subdirs
if cpp_compiler.get_id() == 'emscripten'
  wasm_exported_runtime_methods = '-sEXPORTED_RUNTIME_METHODS=[\'UTF8ToString\',\'stringToNewUTF8\']'
endif

subdir('doc')
subdir('include')
subdir('src')
subdir('tests')
