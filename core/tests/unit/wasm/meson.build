if cpp_compiler.get_id() == 'emscripten'
  npm = find_program('npm', required: true)

  test('wasm-tests', npm, args: ['test'], workdir: meson.current_source_dir())
endif
