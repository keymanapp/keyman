
node = find_program('node', required: false)

hextobin_root = join_paths(meson.source_root(),'..','common','tools','hextobin','build','hextobin.js')
hextobin_cmd = [node, hextobin_root]

subdir('json')
subdir('utftest')
subdir('kmnkbd')
subdir('kmx')
subdir('ldml')
