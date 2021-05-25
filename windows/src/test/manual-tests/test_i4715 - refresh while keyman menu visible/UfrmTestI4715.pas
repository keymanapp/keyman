(*
  Name:             UfrmTestI4715
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      2 Jun 2015

  Modified Date:    2 Jun 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          02 Jun 2015 - mcdurdin - I4715 - Language profile change notification while Keyman menu is visible sometimes causes a crash [CrashID:keyman.exe_9.0.492.0_00000000_EAccessViolation]
*)
unit UfrmTestI4715;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  LangManager;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    ListBox1: TListBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    n: Integer;
    FManager: TLangSwitchManager;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  n := 5;
  Button1.Caption := IntToStr(n);
  timer1.Enabled := true;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  i: Integer;
  j: Integer;
begin
  FManager := TLangSwitchManager.Create;
  FManager.Refresh;

  for i := 0 to FManager.LanguageCount - 1 do
  begin
    for j := 0 to FManager.Languages[i].KeyboardCount - 1 do
    begin
      ListBox1.Items.AddObject(FManager.Languages[i].Keyboards[j].Caption, FManager.Languages[i].Keyboards[j]);
    end;
  end;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
var
  wm_keyman_control_internal: Cardinal;
  h: THandle;
  wm_keyman_control: Cardinal;
begin
  Dec(n);
  Button1.Caption := IntToStr(n);
  if n = 0 then
  begin
    timer1.Enabled := False;
//  listbox1.SetFocus;
//    TLangSwitchKeyboard(ListBox1.Items.Objects[ListBox1.ItemIndex]).Activate;
  wm_keyman_control_internal := RegisterWindowMessage('WM_KEYMAN_CONTROL_INTERNAL');
  wm_keyman_control := RegisterWindowMessage('WM_KEYMAN_CONTROL');
  h := FindWindow('TfrmKeyman7Main', nil);
 PostMessage(h, wm_keyman_control, 18{KMC_PROFILECHANGED}, 0);
// PostMessage(h, wm_keyman_control_internal, 5{KMCI_GETACTIVEKEYBOARD}, 0);
    SendMessage(Handle, WM_INPUTLANGCHANGE, 0, 0);
    Button1.Caption := 'Activate selected language after 5 seconds';
  end;
end;

end.
