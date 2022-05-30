{******************************************************************************}
{ JEDI example DiscBurner - IMAPI-Implementation													   }
{ http://jedi-apilib.sourceforge.net										   }
{ 																			   }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 																			   }
{ Author(s): Benjamin Schwarze												   }
{ Creation date: 18 july 2009 					   				   }
{ Last modification date:	 												   }
{ 																			   }
{ Description: Shows how to burn a cd with IMAP and unit uDiscBurner	   }
{ 																			   }
{ 																			   }
{ Preparations: JWA, CD-Recorder						   }
{ 																			   }
{ Article link:  http://blog.delphi-jedi.net/2009/07/18/im-burning-baby        }
{ 																			   }
{ Version history: 1.0 First Version										   }
{ 																			   }
{ Written in Delphi: 2009													   }
{******************************************************************************}

unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uDiscBurner, ComCtrls;

type
  Tform_Main = class(TForm)
    com_Drive: TComboBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    mem_Props: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure com_DriveChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FDiscMaster : TDiscMaster;
    FRecorderList : TDiscRecorderList;
    function GetActiveRecorder: TDiscRecorder;

    procedure OnProgress(ACompleted, ATotal : Integer);
  public
    property ActiveRecorder : TDiscRecorder read GetActiveRecorder;
  end;

var
  form_Main: Tform_Main;

implementation

{$R *.dfm}

procedure Tform_Main.Button1Click(Sender: TObject);
var
  idx : Integer;
begin
  if OpenDialog1.Execute then
  begin
    for idx := 0 to OpenDialog1.Files.Count - 1 do
      FDiscMaster.JolietAddFile(OpenDialog1.Files[idx], '\', true);
  end;
end;

procedure Tform_Main.Button2Click(Sender: TObject);
begin
  FDiscMaster.RecordDisc(false, false);
end;

procedure Tform_Main.Button3Click(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if Assigned(ActiveRecorder) then
      ActiveRecorder.Erase(false);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure Tform_Main.com_DriveChange(Sender: TObject);
var
  idx : Integer;
begin
  mem_Props.Clear;

  if Assigned(ActiveRecorder) then
  begin
    ActiveRecorder.ReadPropertyNames(mem_Props.Lines);

    FDiscMaster.ActiveRecorder := ActiveRecorder.DiscRecorder;

    for idx := 0 to mem_Props.Lines.Count - 1 do
    begin
      mem_Props.Lines[idx] := mem_Props.Lines[idx] + ' = ' + VarToStr(ActiveRecorder.Prop[mem_Props.Lines[idx]]);
    end;
  end;
end;

procedure Tform_Main.FormCreate(Sender: TObject);
begin
  FDiscMaster := TDiscMaster.Create(Self);
  FRecorderList := TDiscRecorderList.Create;
  FDiscMaster.GetRecorderList(FRecorderList);
  FRecorderList.ToStrings(com_Drive.Items);

  FDiscMaster.OnAddProgress := OnProgress;
  FDiscMaster.OnBlockProgress := OnProgress;

  if com_Drive.Items.Count > 0 then
  begin
    com_Drive.ItemIndex := 0;
    com_DriveChange(com_Drive);
  end;
end;

procedure Tform_Main.FormDestroy(Sender: TObject);
begin
  FRecorderList.Free;
end;

function Tform_Main.GetActiveRecorder: TDiscRecorder;
begin
  if com_Drive.ItemIndex > -1 then
    Result := TDiscRecorder(com_Drive.Items.Objects[com_Drive.ItemIndex])
  else
    Result := nil;
end;

procedure Tform_Main.OnProgress(ACompleted, ATotal : Integer);
begin
  if ProgressBar1.Max <> ATotal then
    ProgressBar1.Max := ATotal;

  ProgressBar1.Position := ACompleted;
end;

end.
