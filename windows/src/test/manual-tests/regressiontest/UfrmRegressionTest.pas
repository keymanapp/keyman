unit UfrmRegressionTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, kmdebug, Menus, Plusmemo, ErrorControlledRegistry, 
  RegistryKeys, PlusmemoU, Grids;

type
  TXMLTag = class
  private
    FData: string;
  public
    function GetTag(n: string): TXMLTag;
    function GetFirstSubTag: TXMLTag;
    function GetNextTag: TXMLTag;
    function AddTag(n: string): TXMLTag;
    function DeleteTag(t: TXMLTag): Boolean;
    property Data: string read FData write FData;
  end;

  TXML = class(TXMLTag)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    function SaveToFile: Boolean;
    function LoadFromFile: Boolean;
  end;

type
  TfrmRegressionTest = class(TForm)
    dlgFont: TFontDialog;
    mnuMain: TMainMenu;
    mnuEdit: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    N3: TMenuItem;
    mnuEditFont: TMenuItem;
    mnuEditResetFont: TMenuItem;
    pages: TPageControl;
    tabSelectKeyboards: TTabSheet;
    tabCreateTests: TTabSheet;
    lblKeyboard: TLabel;
    cbKeyboard: TComboBox;
    lblNewTest: TLabel;
    editNewTest: TEdit;
    cmdAddTest: TButton;
    lblSelect: TLabel;
    lbKeyboards: TListBox;
    cmdAdd: TButton;
    cmdRemove: TButton;
    gridTests: TStringGrid;
    cmdRuleDelete: TButton;
    cmdRuleUp: TButton;
    tabRunTests: TTabSheet;
    cmdRuleDown: TButton;
    cmdRulesClear: TButton;
    cmdRunTests: TButton;
    chkShowChangesOnly: TCheckBox;
    gridResults: TStringGrid;
    cmdColumns: TButton;
    editTestWindow: TEdit;
    lblTestWindow: TLabel;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure editTestGotFocus(Sender: TObject);
    procedure editTestLostFocus(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdAddClick(Sender: TObject);
    procedure cmdRemoveClick(Sender: TObject);
  private
    FInGotFocus: Boolean;
    FTestFileName: string;
    procedure SetEditKeyboardState;
    procedure SetTestFileName(const Value: string);
    procedure AddFile(const filename: string);
    procedure RemoveFile(const filename: string);
    procedure LoadFile;
    procedure AddResult(const FID: string; timestamp: Integer;
      chars: WideString);
    procedure AddTest(const filename: string; keystrokes: TStringList;
      timestamp: Integer; chars: WideString);
    function GetMaxTestID: Integer;
    procedure RefreshData;
  public
    xml: TXML;
    function SaveFile: Boolean;
    function SaveFileAs: Boolean;
    property TestFileName: string read FTestFileName write SetTestFileName;
  end;

implementation

uses util, kmxfile, keyman32_int, UfrmMain;

{$R *.DFM}

procedure TfrmRegressionTest.SetEditKeyboardState;
begin
{  if editNewTest.Focused then
    Keyman_StopForcingKeyboard
  if not FTesting then
  else
  begin
    if not Keyman_ForceKeyboard(FKeyboardName) then
      if not FShownError then
      begin
        FShownError := True;
        ShowMessage('Could not test keyboard.');
        tmrShowError.Enabled := True;
      end;
  end;}
end;

procedure TfrmRegressionTest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmRegressionTest.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TfrmRegressionTest.editTestGotFocus(Sender: TObject);
begin
  if FInGotFocus then Exit;
  FInGotFocus := True;
  SetEditKeyboardState;
  FInGotFocus := False;
end;

procedure TfrmRegressionTest.editTestLostFocus(Sender: TObject);
begin
  Keyman_StopForcingKeyboard;
end;


procedure TfrmRegressionTest.FormCreate(Sender: TObject);
begin
  pages.ActivePage := tabSelectKeyboards;
end;

procedure TfrmRegressionTest.SetTestFileName(const Value: string);
begin
  FTestFileName := Value;
  LoadFile;
end;

procedure TfrmRegressionTest.cmdAddClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    AddFile(dlgOpen.FileName);
end;

procedure TfrmRegressionTest.cmdRemoveClick(Sender: TObject);
begin
  RemoveFile(lbKeyboards.Items[lbKeyboards.ItemIndex]);
end;

procedure TfrmRegressionTest.LoadFile;
begin
  if Assigned(xml) then xml.Free;

  xml := TXML.Create(FTestFileName);
  xml.LoadFromFile;

  RefreshData;
  
  {<xml>
    <keyboards>
      <keyboard>
        <filename>blahblah.kmx</filename>
        <name>Blah Blah</name>
      </keyboard>
    </keyboards>
    <tests>
      <test>
        <keyboard>blahblah.kmx</keyboard>
        <keystrokes>
          <keystroke>
            <vkkey>102</vkkey>
            <shift>4</shift>
            <state>1=down|0=up</state>
          </keystroke>
        </keystrokes>
        <outputs>
          <output>
            <timestamp>11/01/2000 11:15am</timestamp>
            <chars>
              <char>U+1234</char>
              <char>U+2468</char>
            </chars>
          </output>
        </outputs>
      </test>
    </tests>
  </xml>}

  //Use an XML tree to view the file -- do not uncode and recode
end;

const
  XT_XML = 'xml';
  XT_KEYBOARDS = 'keyboards';
  XT_KEYBOARD = 'keyboard';
  XT_FILENAME = 'filename';
  XT_NAME = 'name';

  XT_TESTS = 'tests';
  XT_TEST = 'test';
  XT_ID = 'ID';
  XT_KEYSTROKES = 'keystrokes';
  XT_KEYSTROKE = 'keystroke';
  XT_VKKEY = 'vkkey';
  XT_SHIFT = 'shift';
  XT_STATE = 'state';
  XT_OUTPUTS = 'outputs';
  XT_OUTPUT = 'output';
  XT_TIMESTAMP = 'timestamp';
  XT_CHARS = 'chars';
  XT_CHAR = 'char';

{ XML stuff! }

function TfrmRegressionTest.GetMaxTestID: Integer;
var
  xt: TXMLTag;
begin
  Result := 0;
  xt := xml.GetTag(XT_XML).GetTag(XT_TESTS).GetFirstSubTag;
  while Assigned(xt) do
  begin
    if StrToInt(xt.GetTag(XT_ID).Data) > Result then Result := StrToInt(xt.GetTag(XT_ID).Data);
    xt := xt.GetNextTag;
  end;
end;

procedure TfrmRegressionTest.AddTest(const filename: string; keystrokes: TStringList;
  timestamp: Integer; chars: WideString);
var
  FID: string;
  xt, xt2, xt3: TXMLTag;
  i: Integer;
begin
  FID := IntToStr(GetMaxTestID + 1);
  xt := xml.GetTag(XT_XML).GetTag(XT_TESTS).AddTag(XT_TEST);
  xt.AddTag(XT_KEYBOARD).Data := filename;
  xt.AddTag(XT_ID).Data := FID;
  xt2 := xt.AddTag(XT_KEYSTROKES);
  for i := 0 to (keystrokes.Count div 3) - 1 do
  begin
    xt3 := xt2.AddTag(XT_KEYSTROKE);

    xt3.AddTag(XT_VKKEY).Data := keystrokes[i*3];
    xt3.AddTag(XT_SHIFT).Data := keystrokes[i*3+1];
    xt3.AddTag(XT_STATE).Data := keystrokes[i*3+2];
  end;

  xt.AddTag(XT_OUTPUTS);

  AddResult(FID, 0, chars);
end;

procedure TfrmRegressionTest.AddResult(const FID: string; timestamp: Integer; chars: WideString);
var
  i: Integer;
  xt, xt2: TXMLTag;
begin
  xt := xml.GetTag(XT_XML).GetTag(XT_TESTS).GetFirstSubTag;
  while Assigned(xt) do
  begin
    if xt.GetTag(XT_ID).Data = FID then
    begin
      xt2 := xt.GetTag(XT_OUTPUTS).AddTag(XT_OUTPUT);
      xt2.AddTag(XT_TIMESTAMP).Data := IntToStr(timestamp);
      xt2 := xt2.AddTag(XT_CHARS);
      for i := 1 to Length(chars) do
        xt2.AddTag(XT_CHAR).Data := 'U+' + IntToHex(Ord(chars[i]), 4);
    end;
    xt := xt.GetNextTag;
  end;
end;

procedure TfrmRegressionTest.AddFile(const filename: string);
var
  xt: TXMLTag;
begin
  xt := xml.GetTag(XT_XML).GetTag(XT_KEYBOARDS).AddTag(XT_KEYBOARD);
  xt.AddTag(XT_FILENAME).Data := filename;
  xt.AddTag(XT_NAME).Data := name;
  RefreshData;
end;

procedure TfrmRegressionTest.RemoveFile(const filename: string);
var
  xt, xt2: TXMLTag;
begin
  xt := xml.GetTag(XT_XML).GetTag(XT_KEYBOARDS);
  xt2 := xt.GetFirstSubTag;
  while Assigned(xt2) do
  begin
    if xt2.GetTag(XT_FILENAME).Data = filename then
    begin
      xt.DeleteTag(xt2); // This will delete it and all subtags from the xml tree
      Exit;
    end;
    xt2 := xt2.GetNextTag;
  end;

  RefreshData;
end;

{ Load/save file }

function TfrmRegressionTest.SaveFile: Boolean;
begin
{  if Untitled then
    Result := SaveFileAs
  else
  begin
    xml.SaveToFile(FTestFileName);
    Result := True;
  end;}
end;

function TfrmRegressionTest.SaveFileAs: Boolean;
begin
  if FTestFileName <> '' then
    dlgSave.FileName := FTestFileName;
  if dlgSave.Execute then
  begin
    FTestFileName := dlgSave.FileName;
    Result := SaveFile;
    if Result then frmTike.AddMRU(FTestFileName);
  end
  else
    Result := False;
end;

{ TXMLTag }

function TXMLTag.AddTag(n: string): TXMLTag;
begin

end;

function TXMLTag.DeleteTag(t: TXMLTag): Boolean;
begin

end;

function TXMLTag.GetFirstSubTag: TXMLTag;
begin

end;

function TXMLTag.GetNextTag: TXMLTag;
begin

end;

function TXMLTag.GetTag(n: string): TXMLTag;
begin

end;

{ TXML }

constructor TXML.Create(const AFileName: string);
begin

end;

function TXML.LoadFromFile: Boolean;
begin

end;

function TXML.SaveToFile: Boolean;
begin

end;

procedure TfrmRegressionTest.RefreshData;
begin

end;

end.

