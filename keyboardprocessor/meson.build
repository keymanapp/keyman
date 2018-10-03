project('keyboardprocessor', 'cpp', 'c',
        version: '0.0.0',
        license: 'MIT',
        default_options : ['buildtype=release',
                           'cpp_std=c++14'])

compiler = meson.get_compiler('cpp')

py = import('python3')
python = py.find_python()

inc = include_directories('include', is_system: true)
subdir('include')
subdir('src')
subdir('tests')
