# Copyright:    © 2018-2022 SIL International.
# Description:  Cross platform build script to compile libkmnkbp, documentation
#               and tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

project('keyboardprocessor', 'cpp', 'c',
        version: run_command(find_program('getversion.bat', 'getversion.sh'), check:true).stdout().strip(),
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++14',
                           'b_vscrt=static_from_buildtype',
                           'warning_level=2'],
        meson_version: '>=0.53.0')

# Import our standard compiler defines; this is copied from
# /resources/build/standard.meson.build by build.sh, because
# meson doesn't allow us to reference a file outside its root
subdir('resources')

lib_version = '0.0.0'

py = import('python')
python = py.find_installation()

# TODO: Shared includes may use namespaces, etc which need future tidyup.
# For now, we use KMN_CORE to inject the km::kbp::kmx namespace
defns += ['-DKMN_CORE']

# #define DEBUG when we are on a debug build
if get_option('buildtype') == 'debug'
  add_global_arguments('-DDEBUG', language : 'cpp')
endif

subdir('doc')
subdir('include')
subdir('src')
subdir('tests')