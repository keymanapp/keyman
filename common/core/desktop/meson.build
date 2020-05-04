# Copyright:    © 2018-2020 SIL International.
# Description:  Cross platform build script to compile libkmnkbp, documentation
#               and tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

project('keyboardprocessor', 'cpp', 'c',
        # Get version number from file (VERSION.md is in root of repo)
        # Fallback to "more" for Windows compatbility
        version: run_command(find_program('cat', 'more'),
                 files('../../../VERSION.md')).stdout().strip(),
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++14'])

message('meson.project_version(): ' + meson.project_version() + '\n')
compiler = meson.get_compiler('cpp')

lib_version = '0.0.0'

if compiler.get_id() == 'msvc'
  add_global_arguments('/source-charset:utf-8', language: ['c', 'cpp'])
endif

py = import('python3')
python = py.find_python()

subdir('doc')
subdir('include')
subdir('src')
subdir('tests')
