unit test_i3721;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  msctf;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    pInputProcessorProfiles: ITfInputProcessorProfiles;
    pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Winapi.ActiveX,
  System.Win.ComObj,
  utiltsf,
  keymanapi_TLB;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  kmcom: ITavultesoftKeyman;
  keyboard: IKeymanKeyboardInstalled;
  k3: IKeymanKeyboardInstalled3;
  n: Integer;
  guid: TGUID;
  langid: Word;
  clsid: TGUID;
  i: Integer;
begin
  kmcom := CoTavultesoftKeyman.Create;

  if not kmcom.SystemInfo.IsAdministrator then
  begin
    ShowMessage('This test must be run with elevated privileges');
    Exit;
  end;

  n := kmcom.Keyboards.IndexOf('i3721');
  if n = 0 then
  begin
    ShowMessage('Please install keyboard i3721.kmx before continuing.');
    Exit;
  end;

  keyboard := kmcom.Keyboards[n];
  k3 := (keyboard as IKeymanKeyboardInstalled3);
  if k3.Languages.Count = 0 then
  begin
    ShowMessage('Please install a language profile for i3721.kmx before continuing');
    Exit;
  end;

  clsid := c_clsidKMTipTextService;
  guid := k3.Languages[1].ProfileGUID;
  langid := k3.Languages[1].LangID;

  OleCheck(pInputProcessorProfileMgr.UnregisterProfile(clsid, langid, guid, 0));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  kmcom: ITavultesoftKeyman;
  keyboard: IKeymanKeyboardInstalled;
  k3: IKeymanKeyboardInstalled3;
  n: Integer;
  guid: TGUID;
  langid: Word;
  clsid: TGUID;
  i: Integer;
begin
  kmcom := CoTavultesoftKeyman.Create;

  if not kmcom.SystemInfo.IsAdministrator then
  begin
    ShowMessage('This test must be run with elevated privileges');
    Exit;
  end;

  n := kmcom.Keyboards.IndexOf('i3721');
  if n = 0 then
  begin
    ShowMessage('Please install keyboard i3721.kmx before continuing.');
    Exit;
  end;

  keyboard := kmcom.Keyboards[n];
  k3 := (keyboard as IKeymanKeyboardInstalled3);
  if k3.Languages.Count = 0 then
  begin
    ShowMessage('Please install a language profile for i3721.kmx before continuing');
    Exit;
  end;

  clsid := c_clsidKMTipTextService;
  guid := k3.Languages[1].ProfileGUID;
  langid := k3.Languages[1].LangID;

  k3.Languages[1].Uninstall;

  for i := 1 to kmcom.Errors.Count do
    ShowMessage(kmcom.Errors[i].Description);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));
  OleCheck(pInputProcessorProfiles.QueryInterface(IID_ITfInputProcessorProfileMgr,
    pInputProcessorProfileMgr));
end;

end.
