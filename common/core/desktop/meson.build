# Copyright:    © 2018-2020 SIL International.
# Description:  Cross platform build script to compile libkmnkbp, documentation
#               and tests.
# Create Date:  2 Oct 2018
# Authors:      Tim Eves (TSE)
#

project('keyboardprocessor', 'cpp', 'c',
        version: run_command(find_program('getversion.bat', 'getversion.sh')).stdout().strip(),
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

# Rust library path search

if host_machine.system() == 'windows'
  if host_machine.cpu_family() == 'x86'
    rust_platform = 'x86' #i686-pc-windows-msvc in rust
  else
    rust_platform = 'x64' #x86_64-pc-windows-msvc in rust
  endif
else
  if host_machine.system() == 'darwin'
    # todo we need to support M1 targets as well, eventually
    rust_platform = 'arch'
  else #linux
    # we only build for our current arch
    rust_platform = 'arch'
  endif
endif

rust_path = join_paths(meson.current_source_dir(), 'build', 'rust', rust_platform, get_option('buildtype'))
# message('rust library path is: '+rust_path)
cc = meson.get_compiler('c')
rust_mock_processor = cc.find_library('rust_mock_processor', dirs: rust_path)

subdir('doc')
subdir('include')
subdir('src')
subdir('tests')
