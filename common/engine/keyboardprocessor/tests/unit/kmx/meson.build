# Copyright:    © 2018 SIL International.
# Description:  Cross platform build script to compile libkmnkbp API unit tests.
# Create Date:  19 Oct 2018
# Authors:      Marc Durdin, Tim Eves (TSE)
# History:      19  Oct 2018 - TSE - Added test for context API functions.
#

kmx = executable('kmx', 'kmx.cpp',
    include_directories: [inc, libsrc],
    link_args: links,
    objects: lib.extract_all_objects())

configure_file (input : '000 - null keyboard.kmn', output: '000 - null keyboard.kmn', copy : true, install : true)
configure_file (input : '000 - null keyboard.kmx', output: '000 - null keyboard.kmx', copy : true, install : true)
configure_file (input : '001 - basic input UnicodeI.kmn', output: '001 - basic input UnicodeI.kmn', copy : true, install : true)
configure_file (input : '001 - basic input UnicodeI.kmx', output: '001 - basic input UnicodeI.kmx', copy : true, install : true)
configure_file (input : '002 - basic input Unicode.kmn', output: '002 - basic input Unicode.kmn', copy : true, install : true)
configure_file (input : '002 - basic input Unicode.kmx', output: '002 - basic input Unicode.kmx', copy : true, install : true)
configure_file (input : '004 - basic input (shift 2).kmn', output: '004 - basic input (shift 2).kmn', copy : true, install : true)
configure_file (input : '004 - basic input (shift 2).kmx', output: '004 - basic input (shift 2).kmx', copy : true, install : true)
configure_file (input : '006 - vkey input (shift ctrl).kmn', output: '006 - vkey input (shift ctrl).kmn', copy : true, install : true)
configure_file (input : '006 - vkey input (shift ctrl).kmx', output: '006 - vkey input (shift ctrl).kmx', copy : true, install : true)
configure_file (input : '007 - vkey input (ctrl alt).kmn', output: '007 - vkey input (ctrl alt).kmn', copy : true, install : true)
configure_file (input : '007 - vkey input (ctrl alt).kmx', output: '007 - vkey input (ctrl alt).kmx', copy : true, install : true)
configure_file (input : '008 - vkey input (ctrl alt 2).kmn', output: '008 - vkey input (ctrl alt 2).kmn', copy : true, install : true)
configure_file (input : '008 - vkey input (ctrl alt 2).kmx', output: '008 - vkey input (ctrl alt 2).kmx', copy : true, install : true)
configure_file (input : '012 - ralt.kmn', output: '012 - ralt.kmn', copy : true, install : true)
configure_file (input : '012 - ralt.kmx', output: '012 - ralt.kmx', copy : true, install : true)
configure_file (input : '013 - deadkeys.kmn', output: '013 - deadkeys.kmn', copy : true, install : true)
configure_file (input : '013 - deadkeys.kmx', output: '013 - deadkeys.kmx', copy : true, install : true)
configure_file (input : '014 - groups and virtual keys.kmn', output: '014 - groups and virtual keys.kmn', copy : true, install : true)
configure_file (input : '014 - groups and virtual keys.kmx', output: '014 - groups and virtual keys.kmx', copy : true, install : true)
configure_file (input : '015 - ralt 2.kmn', output: '015 - ralt 2.kmn', copy : true, install : true)
configure_file (input : '015 - ralt 2.kmx', output: '015 - ralt 2.kmx', copy : true, install : true)
configure_file (input : '017 - space mnemonic kbd.kmn', output: '017 - space mnemonic kbd.kmn', copy : true, install : true)
configure_file (input : '017 - space mnemonic kbd.kmx', output: '017 - space mnemonic kbd.kmx', copy : true, install : true)
configure_file (input : '018 - nul testing.kmn', output: '018 - nul testing.kmn', copy : true, install : true)
configure_file (input : '018 - nul testing.kmx', output: '018 - nul testing.kmx', copy : true, install : true)
configure_file (input : '019 - multiple deadkeys.kmn', output: '019 - multiple deadkeys.kmn', copy : true, install : true)
configure_file (input : '019 - multiple deadkeys.kmx', output: '019 - multiple deadkeys.kmx', copy : true, install : true)
configure_file (input : '020 - deadkeys and backspace.kmn', output: '020 - deadkeys and backspace.kmn', copy : true, install : true)
configure_file (input : '020 - deadkeys and backspace.kmx', output: '020 - deadkeys and backspace.kmx', copy : true, install : true)


test('kmx', kmx)
