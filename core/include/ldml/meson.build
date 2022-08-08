# TODO: stuff here, call core/include/ldml/build.sh
inc = include_directories('.', is_system: true)

infile = ['keyboardprocessor_ldml.ts', 'ldml-const-builder.ts']
outfile = 'keyboardprocessor_ldml.h'

mytarget = custom_target('keyboardprocessor_ldml.h',
    output : outfile,
    input : infile,
    command : [ 'bash', '../../tools/ldml-const-builder/build.sh', 'clean', 'build', 'run' ])
