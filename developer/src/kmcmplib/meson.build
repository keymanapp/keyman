#
# Keyman is copyright (C) SIL International. MIT License.
#
# Cross platform build script to compile kmcompx and tests
#

project('kmcompx', 'cpp', 'c',
        version: run_command(find_program('getversion.bat', 'getversion.sh')).stdout().strip(),
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++14',
                           'b_vscrt=static_from_buildtype'])

message('meson.project_version(): ' + meson.project_version() + '\n')
compiler = meson.get_compiler('cpp')

# lib_version = '0.0.0'

if compiler.get_id() == 'msvc'
  add_global_arguments('/source-charset:utf-8', language: ['c', 'cpp'])
endif

message('host_machine.system(): ' + host_machine.system())
message('compiler.get_id(): ' + compiler.get_id())

cc = meson.get_compiler('c')

#defns=["-DUSE_CHAR16_T"]

# subdir('doc')
subdir('include')
subdir('src')
subdir('tests')
