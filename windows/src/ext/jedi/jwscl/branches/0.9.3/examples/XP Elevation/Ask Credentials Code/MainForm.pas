unit MainForm;

interface

uses
  jwaWindows, ShellApi,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExControls, JvButton, JvTransparentButton, StdCtrls,
  SessionPipe,
  Menus, JvComponent;

type
  TFormMain = class(TForm)
    Image1: TImage;
    JvTransparentButton1: TJvTransparentButton;
    JvTransparentButton2: TJvTransparentButton;
    PopupMenuMain: TPopupMenu;
    Screenzoom1: TMenuItem;
    Screenkeyboard1: TMenuItem;
    N1: TMenuItem;
    Center1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure JvTransparentButton1Click(Sender: TObject);
    procedure Center1Click(Sender: TObject);
    procedure Screenzoom1Click(Sender: TObject);
    procedure Screenkeyboard1Click(Sender: TObject);
  private
    { Private-Deklarationen }
    fButtons : array of TJvTransparentButton;
    fJobs : array of TProcessInformation;

  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;



implementation

uses Types, CredentialsForm;

{$R *.dfm}


procedure TFormMain.FormCreate(Sender: TObject);
  procedure Center(F : TControl; I : Integer);
  var SX, SY : Integer;
  begin
    SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
    F.Left := Screen.Monitors[I].Left + ((SX div 2) - (F.Width div 2));

    SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
    F.Top := Screen.Monitors[I].Top + ((SY div 2) - (F.Height div 2));
  end;

  procedure LeftBottom(F : TControl; I : Integer);
  var SX, SY : Integer;
  begin
    SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
    F.Left := Screen.Monitors[I].Left;// + (F.Width);

    SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
    F.Top := Screen.Monitors[I].Top + SY - (F.Height);
  end;

var i : Integer;
begin
  //

  SetLength(fButtons, Screen.MonitorCount);
  for i := low(fButtons) to high(fButtons) do
  begin
    fButtons[i] := TJvTransparentButton.Create(Self);
    fButtons[i].Width := 128;
    fButtons[i].Height := 128;
    Center(fButtons[i], i);
    fButtons[i].Glyph.Assign(JvTransparentButton1.Glyph);
    fButtons[i].Parent := self;
    fButtons[i].HotTrack := true;
    fButtons[i].AutoGray := false;
    fButtons[i].FrameStyle := fsNone;;

    fButtons[i].Tag := i;
    fButtons[i].OnClick := JvTransparentButton1Click;
    fButtons[i].Visible := true;


    fButtons[i] := TJvTransparentButton.Create(Self);
    fButtons[i].Width := 128;
    fButtons[i].Height := 128;
    LeftBottom(fButtons[i], i);
    fButtons[i].Glyph.Assign(JvTransparentButton2.Glyph);
    fButtons[i].Parent := self;
    fButtons[i].HotTrack := true;
    fButtons[i].AutoGray := false;
    fButtons[i].FrameStyle := fsNone;;

    fButtons[i].PopupMenu := PopupMenuMain;
    fButtons[i].DropDownMenu := PopupMenuMain;
    fButtons[i].Tag := i;
   
    fButtons[i].Visible := true;
  end;

  PopupMenu := PopupMenuMain;

{$IFDEF TEST}
  Visible := false;
{$ENDIF}
end;



procedure TFormMain.JvTransparentButton1Click(Sender: TObject);
begin
  try
    FormCredentials.CenterInMonitor((Sender as TControl).Tag);
  except
  end;
end;

procedure TFormMain.Center1Click(Sender: TObject);
var ProcInfo: PROCESS_INFORMATION;
    StartInfo : TStartupInfo;
begin
//StartInfo.cb := sizeof(StartInfo);

//  if not CreateProcess(nil,'control /name Microsoft.EaseOfAccessCenter',nil,nil,true,True, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then

  ShellExecute(0,'open','control.exe','/name Microsoft.EaseOfAccessCenter',nil,SW_SHOWNORMAL);
end;

procedure TFormMain.Screenzoom1Click(Sender: TObject);
begin
  //
  ShellExecute(0,'open','magnify.exe','',nil,SW_SHOWNORMAL);
end;

procedure TFormMain.Screenkeyboard1Click(Sender: TObject);
begin
  //
  ShellExecute(0,'open','osk.exe','',nil,SW_SHOWNORMAL);
end;

end.
