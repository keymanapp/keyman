#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to compile kmcmplib and tests
#

project('kmcmplib', 'cpp', 'c',
        version: run_command(find_program('getversion.bat', 'getversion.sh'), check: true).stdout().strip(),
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++14',
                           'b_vscrt=static_from_buildtype',
                           'default_library=static',
                           'debug=true'])

# Import our standard compiler defines; this is copied from
# /resources/build/standard.meson.build by build.sh, because
# meson doesn't allow us to reference a file outside its root
subdir('resources')

# ------------------------------------------------------------

subdir('include')
subdir('src')
subdir('tests')
