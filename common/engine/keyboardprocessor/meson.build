# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp, documentation
#               and tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

project('keyboardprocessor', 'cpp', 'c',
        version: '0.0.0',
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++14'])

compiler = meson.get_compiler('cpp')

if compiler.get_id() == 'msvc'
  add_global_arguments('/source-charset:utf-8', language: ['c', 'cpp'])
endif

py = import('python3')
python = py.find_python()

inc = include_directories('include', is_system: true)
subdir('doc')
subdir('include')
subdir('src')
subdir('tests')
