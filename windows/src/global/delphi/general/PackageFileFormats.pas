unit PackageFileFormats;

interface

implementation


{###############################################################################
 # KEYMAN 5.0                                                                  #
 ##############################################################################}


{-------------------------------------------------------------------------------
  .kps file format                                                             -
  ================                                                             -
                                                                               -
  [Package]                                                                    -
  FileType=ftPackageSource|ftPackageGroupSource                                -
  ExecuteProgram=<cmdline>                                                     -
  AddUninstallEntry=1|0                                                        -
  CreateStartMenu=1|0                                                          -
  StartMenuPath=<name of start menu folder>                                    -
  OutPath=<fully qualified output file name>                                   -
  AddReadmeStartMenuEntry=1|0                                                  -
  ReadMeFile=<filename>                                                        -
                                                                               -
  [Info]                                                                       -
  <name>=""<description>","<url>"" |                                           -
  <name>=""<description>""                                                     -
                                                                               -
  [Files]                                                                      -
  <n>=       [n=0..count of files-1]                                           -
                                                                               -
  [File-<n>]                                                                   -
  FileName=<relative file name>                                                -
  FileNameAbsolute=<absolute file name>                                        -
  FileType=ftPackageFile|...                                                   -
  Description=<descriptive text>                                               -
  AddStartMenuIcon=0|1                                                         -
  AddUninstallEntry=0|1                                                        -
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  kmp.inf file format                                                          -
  ===================                                                          -
                                                                               -
  [Install]                                                                    -
  KMXFile=<keyboardfile>                                                       -
  ExecuteProgram=<cmdline>                                                     -
  StartMenuPath=<start menu path>                                              -
                                                                               -
  [InstallFiles]                                                               -
  kmp.inf=Package information                                                  -
  <filename>=<description>                                                     -
                                                                               -
  [Fonts]                                                                      -
  <fontfilename>=<fontdescription>                                             -
                                                                               -
  [PackageInfo]                                                                -
  <name>=""<description>","<url>"" |                                           -
  <name>=""<description>""                                                     -
                                                                               -
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  kmredist.inf file format                                                     -
  ========================                                                     -
                                                                               -
  [Install]                                                                    -
  StartMenuPath=<name>                                                         -
                                                                               -
  [InstallFiles]                                                               -
  kmredist.inf=Installation information                                        -
  keyman.exe=Keyman executable                                                 -
  keyman32.dll=Keyman executable                                               -
  kmshell.exe=Keyman executable                                                -
  kmuninst.bin=Keyman uninstaller                                              -
  klicence.htm=Keyman licence                                                  -
  keyman.chm=Keyman help                                                       -
                                                                               -
  [Packages]                                                                   -
  <packagename_n>=<description_n>                                              -
                                                                               -
  [PackageInfo]                                                                -
  <name>=""<description>","<url>"" |                                           -
  <name>=""<description>""                                                     -
                                                                               -
-------------------------------------------------------------------------------}


{###############################################################################
 # KEYMAN 6.0                                                                  #
 ##############################################################################}


{-------------------------------------------------------------------------------
  .kps file format                                                             -
  ================                                                             -
                                                                               -
  [Package]                                                                    -
  Version=6.0                                                                  -
  RedistributableInstaller=1|0                                                 -
  ExecuteProgram=<cmdline>                                                     -
  OutPath=<relative output file name>                                          -
  ReadMeFile=<filename, relative from [Files]>                                 -
  FileCount=<n>                                                                -
                                                                               -
  [StartMenu]                                                                  -
  Path=<name of start menu folder>                                             -
  Create=1|0                                                                   -
  AddUninstallEntry=1|0                                                        -
                                                                               -
  [StartMenuEntries]                                                           -
  <name>="<program>",<parameters>                                              -
                                                                               -
  [Info]                                                                       -
  <name>=""<description>","<url>"" |                                           -
  <name>=""<description>""                                                     -
                                                                               -
  [Files]                                                                      -
  <n>="<Description>","<FileName>"[,0|1]                                       -
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  kmp.inf file format                                                          -
  ===================                                                          -
                                                                               -
  [Install]                                                                    -
  Version=6.0                                                                  -
  ExecuteProgram=<cmdline>                                                     -
  StartMenuPath=<start menu path>                                              -
                                                                               -
  [InstallFiles]                                                               -
  kmp.inf=Package information                                                  -
  <filename>=<description>                                                     -
                                                                               -
  [PackageInfo]                                                                -
  <name>=""<description>","<url>"" |                                           -
  <name>=""<description>""                                                     -
                                                                               -
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  kmredist.inf file format                                                     -
  ========================                                                     -
                                                                               -
  [Install]                                                                    -
  Version=6.0                                                                  -
                                                                               -
  [InstallFiles]                                                               -
  kmredist.inf=Installation information                                        -
  keyman.exe=Keyman executable                                                 -
  keyman32.dll=Keyman executable                                               -
  kmshell.exe=Keyman executable                                                -
  kmuninst.bin=Keyman uninstaller                                              -
  klicence.htm=Keyman licence                                                  -
  keyman.chm=Keyman help                                                       -
  <otherfile>=<description>         ; these files added when .kps is built to  -
                                    ; .exe and CopyToKeyman=1                  -
  [Packages]                                                                   -
  <packagename_n>=<description_n>                                              -
                                                                               -
  [PackageInfo]                                                                -
  <name>=""<description>","<url>"" |                                           -
  <name>=""<description>""                                                     -
                                                                               -
  [Graphic]                                                                    -
  Background=w,h,<bitmapfile>|<resourceid>                                     -
  Icon=<bmpfile>|<icofile>                                                     -
                                                                               -
  [Buttons]                                                                    -
  ; bmp, hover, down not req -- std btn used if not specified                  -
  ; if [Graphic]/Background spec, then Inst, Exit, About buttons required.     -
  InstallButton=x,y,w,h,<bmp>|<res>,<bmp-hover>|<res-hover>,                   -
     <bmp-down>|<res-down>                                                     -
  ExitButton=x,y,w,h,<bmp>|<res>,<bmp-hover>|<res-hover>,                      -
     <bmp-down>|<res-down>                                                     -
  AboutButton=x,y,w,h,<bmp>|<res>,<bmp-hover>|<res-hover>,                     -
     <bmp-down>|<res-down>                                                     -
  Button#=x,y,w,h,<bmp>|<res>,<bmp-hover>|<res-hover>,                         -
     <bmp-down>|<res-down>,<cmdline>                                           -
                                                                               -
-------------------------------------------------------------------------------}


{###############################################################################
 # GENERAL NOTES                                                               #
 ##############################################################################}


{-------------------------------------------------------------------------------
                                                                               -
Requirements                                                                   -
============                                                                   -
                                                                               -
GENERAL                                                                        -
                                                                               -
 1. The file names must follow the existing patterns (kmp.inf, *.kps,          -
    kmredist.inf)                                                              -
 2. When building a redist installer, all component files will be placed into  -
    a single .kmp file before being included in the installer.  This ensures   -
    that the files will be installed into the correct locations.  No           -
    additional files can be included in the Keyman directory by default.       -
 3. We will have an option for files to be copied into the Keyman directory.   -
    This will only be available if a registry setting is set to turn this on   -
    (usually only for OEMs) and will be ignored for .kmp and only used for     -
    redist installers.                                                         -
 4. All kmps are delinked from each other.  A kmp does not need to have .kmx   -
    files in it, or even other .kmp files.  It can be just a collection of     -
    fonts or associated files.                                                 -
 5. Very important to ensure that users cannot uninstall individual keyboards  -
    from a package.                                                            -
                                                                               -
KEYMAN                                                                         -
                                                                               -
 1. Keyman 6.0 can install .kmp packages from Keyman 5.0.                      -
 2. Keyman 6.0 reads the file version of .kmp packages and does not attempt to -
    load newer ones.                                                           -
                                                                               -
KEYMAN DEVELOPER                                                               -
                                                                               -
 1. Keyman Developer 6.0 can load version 5.0 .kps files (both kinds)          -
 2. Keyman Developer 6.0 can export to version 5.0 .kps files (both kinds)     -
      -- loss of information okay                                              -
 3. Keyman Developer 6.0 can save version 6.0 .kps files                       -
 4. Keyman Developer 6.0 reads the file version of .kps files and does         -
    not attempt to load newer ones.                                            -
                                                                               -
 * Keyman Developer 6.0 will not produce .kmp packages for Keyman 5.x; if the  -
   user wishes to do this, they will need to export the package to version 5.0 -
   .kps and create the .kmp with a Keyman Developer 5.0 install.               -
                                                                               -
-------------------------------------------------------------------------------}

end.
