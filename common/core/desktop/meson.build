# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp, documentation
#               and tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

project('keyboardprocessor', 'cpp', 'c',
        version: '14.0.17',
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++14'])

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
