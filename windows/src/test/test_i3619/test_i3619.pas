unit test_i3619;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, msctf, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    Memo1: TMemo;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    pInputProcessorProfiles: ITfInputProcessorProfiles;
    pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
    clsid, guid: TGUID;
    langid: Word;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses activex, comobj;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  pInputProcessorProfileSubstituteLayout: ITfInputProcessorProfileSubstituteLayout;
  h: HKL;
begin
  OleCheck(pInputProcessorProfiles.QueryInterface(IID_ITfInputProcessorProfileSubstituteLayout,
    pInputProcessorProfileSubstituteLayout));
  OleCheck(pInputProcessorProfileSubstituteLayout.GetSubstituteKeyboardLayout(clsid, langid, guid, h));
  showmessage(inttohex(h,8));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  h: HKL;
begin
  //h := LoadKeyboardLayout('00010409', 0);
  OleCheck(pInputProcessorProfileMgr.RegisterProfile(clsid, langid, guid, 'My Test Desc', Length('My Test Desc'),
    'C:\ProgramData\Tavultesoft\Keyman Engine 9.0\Keyboard\_Package\gff-amh-powerpack-7\gff-amh-7.kmx.ico',
    Length('C:\ProgramData\Tavultesoft\Keyman Engine 9.0\Keyboard\_Package\gff-amh-powerpack-7\gff-amh-7.kmx.ico'),
    0, $0409, 0, 1, 0));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  OleCheck(pInputProcessorProfileMgr.UnregisterProfile(clsid, langid, guid, 0));
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  profile: TF_INPUTPROCESSORPROFILE;
begin
  OleCheck(pInputProcessorProfileMgr.GetProfile(TF_PROFILETYPE_INPUTPROCESSOR, langid, clsid, guid, 0, profile));
  ShowMessage(IntToHex(profile.hklSubstitute, 8));
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  memo1.SetFocus;
  OleCheck(pInputProcessorProfiles.ChangeCurrentLanguage(langid));
  OleCheck(pInputProcessorProfileMgr.ActivateProfile(TF_PROFILETYPE_INPUTPROCESSOR, langid, clsid, guid, 0,
TF_IPPMF_FORPROCESS));
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  h: HKL;
begin
  h := LoadKeyboardLayout('00010409', 0);
  UnloadKeyboardLayout(h);
  UnloadKeyboardLayout($40c040c);
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  h: HKL;
begin
  h := LoadKeyboardLayout('00010409', 0);
  ActivateKeyboardLayout(h, 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));
  OleCheck(pInputProcessorProfiles.QueryInterface(IID_ITfInputProcessorProfileMgr,
    pInputProcessorProfileMgr));

  clsid := StringToGuid('{B4794F27-E3F8-40DE-A9B4-C305EB3CAB68}');
  guid := StringToGuid('{CA612A09-FB06-4432-9649-EFA83DC8FF21}');
  langid := $40c;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  pInputProcessorProfiles := nil;
  pInputProcessorProfileMgr := nil;
  CoUninitialize;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := 'Active HKL: '+IntToHex(GetKeyboardLayout(0), 8);
end;

end.
