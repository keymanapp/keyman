{Jedi Windows Security Code Library Header File
Do not add DocOMatic comments into this file!}

//Freepascal compability mode (not fully supported)
{.$DEFINE FPC}

//use JEDI Component Library
{.$DEFINE JCL}

{Activate VISTA functions calls.
If this definition is set and the application is compiled with
static linked jwaVista.pas the binary application will fail
on pre Vista Windows versions!!

Warning: Unsetting this value will not remove constructor, method and property
definitions created for Windows Vista. It only removes function calls
so that static linked apps will not fail immediately on pre Vista systems.
Of course they will fail if Vista stuff is called on them.

Warning #2: If the JEDI API headers are used with a directive WIN_XP or lower
  this switch must be unset.
  These compiler directives reside in includes\jediapilib.inc
  Use at least WINVISTA or WIN2008 
}
{$DEFINE VISTA}   

//adds compiler version directives
//jwscl.inc is only added by JWSCL units in source folder
//so we're actually in the source folder and must jump up

//This is for compability reasons only!
//On of them should work. However the second one is standard and supported.
{.$I .\jedi.inc}
{$I ..\Includes\jedi.inc}


{$IFDEF FPC}
{$mode delphi}{.$H+}
{$ENDIF}

{$IFDEF DEBUG}
{.$DEFINE JWSCL_DEBUG_INFO}
{$ENDIF DEBUG}

{$DEFINE JWSCL_USE_CACHES}

{$IFDEF JWSCL_USE_CACHES}
  {$DEFINE JWSCL_SIDCACHE}
{$ENDIF JWSCL_USE_CACHES}

{$IFDEF DEBUG}
  {.$WARNINGS OFF}
{$IFDEF DELPHI7_UP}
  {$WARN GARBAGE OFF} //removes warning about code behind end.
{$ENDIF DELPHI7_UP}
{$ENDIF DEBUG}


//the following line must be at the end of this file!!
{Warning: This line is printed because an identifier has no pasdoc comment. This error must be fixed.}
