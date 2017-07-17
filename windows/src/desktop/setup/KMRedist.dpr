{$A-,B-,C-,D+,E-,F-,G+,H+,I-,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
(******************************************************************)
(* ZipSFX                                                         *)
(* Copyright 1997, Carl Bunton                                    *)
(* Home of ZipTV compression components for Delphi                *)
(* Email: Twojags@cris.com                                        *)
(* Web-page: http://www.concentric.net/~twojags                   *)
(*                                                                *)
(* This program was written in Delphi 2 because version 2         *) //## this release in delphi3
(* compiles a much smaller executable using the windows api.  It  *)
(* should be fully compatible with Delphi 3, but will produce a   *)
(* noticable increase of size in the final compiled program.      *)
(*                                                                *)
(*MODIFIED by M. Stephany mirbir.st@t-online.de  12/28/97-08/25/98*)
(* for some special purposes; modified lines are marked (##)      *)
(******************************************************************)

{ notes:

the initial release of zipsfx comes from Carl Bunton (see above).
he has created a powerful shareware vcl for delphi that can handle
almost all of the popular archive-formats like zip,lzh,arj...., called ziptv.

the first modifications came from Eric W. Engler, the author of the great freeware
delphi-vcl delzip that can handle zip-archives and -sfx's. (englere@swcp.com)

now i am trying to make the code a bit more sfx-creator- and sfx-user- friendly,
and i must say, eric is a very hard beta-tester :).

original zip-code comes from the infozip-group, they developped a free implementation
of the zip/unzip-code for unix and later for other platforms.
  Info-Zip home page:
  http://www.cdrom.com/pub/infozip/

regards, Markus Stephany, mirbir.st@t-online.de
Merkes' Pages at http://home.t-online.de/home/mirbir.st/

losheim am see, saarland, germany, jan 03 1998 ; mar 15, 1998 ; july 05 , 1998 ; august 13 , 1998

NOTE : !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                                                                       !
Keyman Developer, CompilePackage.pas, sets the size of the redist installer in the StartOfFileRecord   !
details in Dialogsel.pas.  You do not need to set this yourself...                                     !
                                                                                                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

}

program
  KMRedist;

uses
  Windows,
  Commctrl,
  Dialog in 'dialog.pas',
  SFXgbls in 'sfxgbls.pas',
  SFXmisc in 'sfxmisc.pas',
  dialogsel in 'dialogsel.pas',
  SFXStrings in 'sfxstrings.pas',
  keymanstrings in 'rel\keymanstrings.pas';

{$R VERSION.RES}
{$R SFX1.res}

var
  ShouldRunSilent: Boolean = False;

BEGIN
  (* Open the archive *)
  InFile := CreateFile( PChar( ParamStr( 0 ) ),
                       GENERIC_READ,
                       FILE_SHARE_READ,
                       NIL,
                       OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL,
                       0 );

  (* If error, notify and abort *)
  IF InFile = INVALID_HANDLE_VALUE THEN
  BEGIN
     MessageBox( 0, PChar( ParamStr( 0 ) ), STR_EARCHIVE, MB_ICONERROR );
     EXIT;
  END;

  try
    CreateTempDir;
    try
      New( CRC32Table );
      Make_CRC32Table;

      InitCommonControl(ICC_PROGRESS_CLASS);

      TRY
        FileLength := GetFileSize(InFile, nil);

        { Display the dialog }

        TotalBytes := 0;
        //Cancel_From_InfoWin := False;
        if not getdefaultparams then Exit;

        if not ProcessArchive(0, True) then
          Exit;

        {TESTING CODE: ExtPath := 'c:\keyman\5.1\devel\kmredist';
        SFXDetails.Flags := [sdfRegistered, sdfGraphicDialog];}
         

        if not (sdfRegistered in SFXFlags) then
        begin
          if Windows.MessageBox(0, PChar(SKUnregisteredPackage), PChar(SKApplicationTitle),
            MB_YESNOCANCEL or MB_ICONQUESTION) <> IDYES then Exit;
        end;

        GetExtraParams(ShouldRunSilent);

        if ShouldRunSilent then
          if not RunSilent then ExitCode := 1 else ExitCode := 0

        else if sdfGraphicDialog in SFXFlags then
        begin
          if DialogBox( hInstance, 'GRDIALOG', 0, @GraphicDialogProc ) = -1 then
            MessageBox(0, PChar(SKCannotOpenDialog), PChar(SKApplicationTitle), MB_OK);
        end
        else
         if DialogBox( hInstance, 'MAINDIALOG', 0, @MainDialogProc ) = -1 then
            MessageBox(0, PChar(SKCannotOpenDialog), PChar(SKApplicationTitle), MB_OK);
      FINALLY
        { Close the archive }
        Dispose(CRC32Table);
      END;
    finally
      RemoveTempDir;
    end;
  finally
    if not CloseHandle(InFile) then
      if ShouldRunSilent then ExitCode := 1 else MessageBox( 0, STR_CANNOTCLOSE, STR_E , MB_ICONERROR );
  end;
END.

