(*
  Name:             si_processes_x64
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Dec 2011

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Dec 2011 - mcdurdin - I3180 - x64 process details for tsysinfo
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
*)
unit si_processes_x64; // I3180

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, si_base;

type
  TSI_Processes_x64 = class(TSI_Base)
  private
    procedure DoSpin(Sender: TObject);
    procedure NewParent(Sender: TSIList; const Caption: string;
      var Parent: TWinControl);
  protected
    function DoCollect: Boolean; override;
    class function GetCaption: String; override;
  end;

implementation

uses
  sysinfo_util, TempFileManager, utildir, utilsystem;

{$R tsysinfo_x64.res}

{ TSI_Processes_x64 }

class function TSI_Processes_x64.GetCaption: String;
begin
  Result := 'Processes (x64)';
end;

procedure TSI_Processes_x64.DoSpin(Sender: TObject);
begin
  Sleep(0);
end;

function TSI_Processes_x64.DoCollect: Boolean;
var
  FDiagX64App: WideString;
  FTempFileName: TTempFile;   // I4181
  FErrorString: WideString;
  v: Cardinal;
  res: TResourceStream;
  FDiagX64AppTemp: TTempFile;   // I4181
begin
  Result := False;
  if not Wow64Process then
    Exit;

  FDiagX64AppTemp := nil;   // I4181
  FDiagX64App := ExtractFilePath(ParamStr(0))+'tsysinfox64.exe';
  if not FileExists(FDiagX64App) then
  begin
    FDiagX64AppTemp := TTempFileManager.Get('.exe'); //'tsysinfox64.exe';   // I4181
    FDiagX64App := FDiagX64AppTemp.Name;

    res := TResourceStream.Create(HInstance, 'TSYSINFOX64_EXE', RT_RCDATA);
    try
      with TFileStream.Create(FDiagX64App, fmCreate) do
      try
        CopyFrom(res, 0);
      finally
        Free;
      end;
    finally
      res.Free;
    end;
  end;

  try
    FTempFileName := TTempFileManager.Get();   // I4181
    try
      v := WaitForProgram(Application.Handle, FDiagX64App, ExtractFilePath(FDiagX64App), FTempFileName.Name, FErrorString, DoSpin, False);   // I4181
      if v <> 0 then
        raise Exception.Create('Failed to run tsysinfox64.exe: '+FErrorString+' ('+IntToStr(v)+')');

      with TSIList.Create(nil) do
      try
        OnNewParent := NewParent;
        Load(FTempFileName.Name);   // I4181
        if Count = 0 then
          raise Exception.Create('No data in temp file '+FTempFileName.Name);   // I4181
        Self.doc.loadXML(Items[0].XMLData);
      finally
        Free;
      end;
    finally
      FreeAndNil(FTempFileName);   // I4181
    end;
  finally
    FreeAndNil(FDiagX64AppTemp);
  end;

  Result := True;
end;

procedure TSI_Processes_x64.NewParent(Sender: TSIList; const Caption: string; var Parent: TWinControl);
begin
  Parent := nil;
end;

initialization
  TSI_Processes_x64.Register;
end.
