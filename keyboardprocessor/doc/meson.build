# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to extract and generate
#               documentation for the API. This is speculative ATM.
# Create Date:  5 Oct 2018
# Authors:      Tim Eves (TSE)
#

hotdoc = find_program('hotdoc', required: false)
installdir = join_paths(get_option('datadir'), 'doc', meson.project_name())

if hotdoc.found()
  cfg = configuration_data()
  cfg.set('project_name', meson.project_name())
  cfg.set('project_version', meson.project_version())
  configure_file(input: 'hotdoc.json',
                 output: 'hotdoc.json',
                 configuration: cfg)
  deps = files(
    '../include/keyboardprocessor.h.in',
    '../src/json.hpp',
    '../src/utfcodec.hpp'
  )

  docs = custom_target('docs',
    output: ['html'],
    input: ['sitemap.txt', 'markdown_files/index.md'],
    command: [hotdoc, '--verbose', '--conf-file=doc/hotdoc.json', '--output=doc', 'run'],
    depend_files: deps,
    install: true,
    install_dir: installdir)
endif
