JwaWindows was separated from the other header files because 
in this way a compiled JwaWindows.pas can be used with the single units
in the same Delphi. However you should not use JwaWindows in combination with
the single units in the same project. It is possible but this way is not supported by JEDI team.

To compile JwaWindows.pas you need to add the parent folder of jwaWindows (usually Win32API)
to your source path.

The packages were adapted.