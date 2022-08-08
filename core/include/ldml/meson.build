bash = find_program('bash')

kbp_ldml_h = custom_target('keyboardprocessor_ldml.h',
    output : 'keyboardprocessor_ldml.h',
    input :  ['keyboardprocessor_ldml.ts', 'ldml-const-builder.ts'],
    command : [ bash, join_paths(meson.current_source_dir(), '../../tools/ldml-const-builder/build.sh'), 'clean', 'build', 'run' ],
    depend_files : files('keyboardprocessor_ldml.ts', 'ldml-const-builder.ts')
    )
