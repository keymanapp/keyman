unit subst_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure GetCurrentClick(Sender: TObject);
    procedure DoSubstClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearSubstClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Winapi.ActiveX,
  ComObj,
  msctf;

{$R *.dfm}

const
  CLSID_TF_InputProcessorProfiles: TGUID = '{33C53A50-F456-4884-B049-85FD643ECFED}';
  EmptyGuid: TGUID =                       '{00000000-0000-0000-0000-000000000000}';

procedure TForm1.ClearSubstClick(Sender: TObject);
var
  pPro: ITfInputProcessorProfiles;
  pMgr: ITfInputProcessorProfileMgr;
  pSubLayout: ITfInputProcessorProfileSubstituteLayout;
  v: HKL;
  rclsid: GUID;
  guidProfile: GUID;
  langid: Word;
  profile: TF_INPUTPROCESSORPROFILE;
  hklUS: HKL;
begin
  OleCheck(CoCreateInstance(CLSID_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER, IID_ITfInputProcessorProfiles, pPro));

  if Supports(pPro, IID_ITfInputProcessorProfileMgr, pMgr) then
  begin
    OleCheck(pMgr.GetActiveProfile(GUID_TFCAT_TIP_KEYBOARD, profile));
    if not IsEqualGuid(profile.clsid, EmptyGuid) then
    begin
      //hklUS := LoadKeyboardLayout('00000409', KLF_NOTELLSHELL);
      OleCheck(pPro.SubstituteKeyboardLayout(profile.clsid, profile.langid, profile.guidProfile, 0));
      memo1.Lines.Add('Substitution removed');
    end
    else
      memo1.Lines.Add('Subst not supported for keyboard layouts');
  end;
end;

procedure TForm1.DoSubstClick(Sender: TObject);
var
  pPro: ITfInputProcessorProfiles;
  pMgr: ITfInputProcessorProfileMgr;
  pSubLayout: ITfInputProcessorProfileSubstituteLayout;
  v: HKL;
  rclsid: GUID;
  guidProfile: GUID;
  langid: Word;
  profile: TF_INPUTPROCESSORPROFILE;
  hklUS: HKL;
begin
  OleCheck(CoCreateInstance(CLSID_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER, IID_ITfInputProcessorProfiles, pPro));

  if Supports(pPro, IID_ITfInputProcessorProfileMgr, pMgr) then
  begin
    OleCheck(pMgr.GetActiveProfile(GUID_TFCAT_TIP_KEYBOARD, profile));
    if not IsEqualGuid(profile.clsid, EmptyGuid) then
    begin
      hklUS := $409; // LoadKeyboardLayout('00000409', KLF_NOTELLSHELL);
      OleCheck(pPro.SubstituteKeyboardLayout(profile.clsid, profile.langid, profile.guidProfile, hklUS));
      memo1.Lines.Add('US English substitution applied');
    end
    else
      memo1.Lines.Add('Subst not supported for keyboard layouts');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CoUninitialize;
end;

procedure TForm1.GetCurrentClick(Sender: TObject);
var
  pPro: ITfInputProcessorProfiles;
  pMgr: ITfInputProcessorProfileMgr;
  pSubLayout: ITfInputProcessorProfileSubstituteLayout;
  v: HKL;
  rclsid: GUID;
  guidProfile: GUID;
  langid: Word;
  profile: TF_INPUTPROCESSORPROFILE;
begin
  OleCheck(CoCreateInstance(CLSID_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER, IID_ITfInputProcessorProfiles, pPro));

  if Supports(pPro, IID_ITfInputProcessorProfileMgr, pMgr) then
  begin
    OleCheck(pMgr.GetActiveProfile(GUID_TFCAT_TIP_KEYBOARD, profile));
    if not IsEqualGuid(profile.clsid, EmptyGuid) then
    begin
      if Supports(pPro, IID_ITfInputProcessorProfileSubstituteLayout, pSubLayout) then
      begin
        //OleCheck(pPro.GetActiveLanguageProfile(rclsid, langid, guidProfile));
        OleCheck(pSubLayout.GetSubstituteKeyboardLayout(profile.clsid, profile.langid, profile.guidProfile, v));
        memo1.Lines.Add('Substitute for '+IntToHex(profile.langid,8)+' is: '+IntToHex(v, 8));
      end;
    end
    else
      memo1.Lines.Add('Subst not supported for keyboard layouts');
  end;
end;

end.
