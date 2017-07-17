unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, RichEdit, AxCtrls, OleCtrls, DbOleCtl,
  RichTextLib_TLB;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure RichEdit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

type
  tstyle = record
    Name: string;
    Value: Cardinal;
  end;

const
  WinStyle: array[0..40] of tstyle = (
    (Name: 'WS_OVERLAPPED   '; Value: $00000000),
    (Name: 'WS_POPUP        '; Value: $80000000),
    (Name: 'WS_CHILD        '; Value: $40000000),
    (Name: 'WS_MINIMIZE     '; Value: $20000000),
    (Name: 'WS_VISIBLE      '; Value: $10000000),
    (Name: 'WS_DISABLED     '; Value: $08000000),
    (Name: 'WS_CLIPSIBLINGS '; Value: $04000000),
    (Name: 'WS_CLIPCHILDREN '; Value: $02000000),
    (Name: 'WS_MAXIMIZE     '; Value: $01000000),
    (Name: 'WS_BORDER       '; Value: $00800000),
    (Name: 'WS_DLGFRAME     '; Value: $00400000),
    (Name: 'WS_VSCROLL      '; Value: $00200000),
    (Name: 'WS_HSCROLL      '; Value: $00100000),
    (Name: 'WS_SYSMENU      '; Value: $00080000),
    (Name: 'WS_THICKFRAME   '; Value: $00040000),
    (Name: 'WS_GROUP        '; Value: $00020000),
    (Name: 'WS_TABSTOP      '; Value: $00010000),
    (Name: 'WS_MINIMIZEBOX  '; Value: $00020000),
    (Name: 'WS_MAXIMIZEBOX  '; Value: $00010000),    //* Edit styles
    (Name: 'ES_LEFT        '; Value: $0000),
    (Name: 'ES_CENTER      '; Value: $0001),
    (Name: 'ES_RIGHT       '; Value: $0002),
    (Name: 'ES_MULTILINE   '; Value: $0004),
    (Name: 'ES_UPPERCASE   '; Value: $0008),
    (Name: 'ES_LOWERCASE   '; Value: $0010),
    (Name: 'ES_PASSWORD    '; Value: $0020),
    (Name: 'ES_AUTOVSCROLL '; Value: $0040),
    (Name: 'ES_AUTOHSCROLL '; Value: $0080),
    (Name: 'ES_NOHIDESEL   '; Value: $0100),
    (Name: 'ES_OEMCONVERT  '; Value: $0400),
    (Name: 'ES_READONLY    '; Value: $0800),
    (Name: 'ES_WANTRETURN  '; Value: $1000),
    (Name: 'ES_NUMBER      '; Value: $2000),    //* New edit control styles */
    (Name: 'ES_SAVESEL'; Value:				  $00008000),
    (Name: 'ES_SUNKEN'; Value:				    $00004000),
    (Name: 'ES_DISABLENOSCROLL'; Value:	$00002000),    //* same as WS_MAXIMIZE; Value: but that doesn't make sense so we re-use the value */
    (Name: 'ES_SELECTIONBAR'; Value:			$01000000),      //* same as ES_UPPERCASE; Value: but re-used to completely disable OLE drag'n'drop */
    (Name: 'ES_NOOLEDRAGDROP'; Value:		$00000008),    //* These flags are used in FE Windows */
    (Name: 'ES_VERTICAL'; Value:				  $00400000),		//* Not supported in RE 2.0/3.0 */
    (Name: 'ES_NOIME'; Value:				    $00080000),
    (Name: 'ES_SELFIME'; Value:				  $00040000)
  );


  WinExStyle: array[0..24] of tstyle = (
    //* Edit control extended style */
    (Name: 'ES_EX_NOCALLOLEINIT'; Value:	$01000000),
    (Name: 'WS_EX_DLGMODALFRAME'   ; Value: $00000001),
    (Name: 'WS_EX_NOPARENTNOTIFY'  ; Value: $00000004),
    (Name: 'WS_EX_TOPMOST'         ; Value: $00000008),
    (Name: 'WS_EX_ACCEPTFILES'     ; Value: $00000010),
    (Name: 'WS_EX_TRANSPARENT'     ; Value: $00000020),
    (Name: 'WS_EX_MDICHILD'        ; Value: $00000040),
    (Name: 'WS_EX_TOOLWINDOW'      ; Value: $00000080),
    (Name: 'WS_EX_WINDOWEDGE'      ; Value: $00000100),
    (Name: 'WS_EX_CLIENTEDGE'      ; Value: $00000200),
    (Name: 'WS_EX_CONTEXTHELP'     ; Value: $00000400),
    (Name: 'WS_EX_RIGHT'           ; Value: $00001000),
    (Name: 'WS_EX_LEFT'            ; Value: $00000000),
    (Name: 'WS_EX_RTLREADING'      ; Value: $00002000),
    (Name: 'WS_EX_LTRREADING'      ; Value: $00000000),
    (Name: 'WS_EX_LEFTSCROLLBAR'   ; Value: $00004000),
    (Name: 'WS_EX_RIGHTSCROLLBAR'  ; Value: $00000000),
    (Name: 'WS_EX_CONTROLPARENT'   ; Value: $00010000),
    (Name: 'WS_EX_STATICEDGE'      ; Value: $00020000),
    (Name: 'WS_EX_APPWINDOW'       ; Value: $00040000),
    (Name: 'WS_EX_LAYERED'         ; Value: $00080000),
    (Name: 'WS_EX_NOINHERITLAYOUT' ; Value: $00100000), // Disable inheritence of mirroring by children
    (Name: 'WS_EX_LAYOUTRTL'       ; Value: $00400000), // Right to left mirroring
    (Name: 'WS_EX_COMPOSITED'      ; Value: $02000000),
    (Name: 'WS_EX_NOACTIVATE'      ; Value: $08000000)
  );

  LangOptions: array[0..6] of TStyle = (
    (Name: 'IMF_AUTOFONT';            Value: $2 ),
    (Name: 'IMF_AUTOFONTSIZEADJUST';  Value: $10 ),
    (Name: 'IMF_AUTOKEYBOARD';        Value: $1 ),
    (Name: 'IMF_DUALFONT';            Value: $80 ),
    (Name: 'IMF_IMEALWAYSSENDNOTIFY'; Value: $8 ),
    (Name: 'IMF_IMECANCELCOMPLETE';   Value: $4 ),
    (Name: 'IMF_UIFONTS';             Value: $20 ));

  TextModes: array[0..5] of TStyle = (
    (Name: 'TM_PLAINTEXT'; Value: $0001 ),
    (Name: 'TM_RICHTEXT'; Value: $0002),
    (Name: 'TM_SINGLELEVELUNDO'; Value: $0004),
    (Name: 'TM_MULTILEVELUNDO'; Value: $0008),
    (Name: 'TM_SINGLECODEPAGE'; Value: $00010 ),
    (Name: 'TM_MULTICODEPAGE'; Value: $00020 ));

function st(sy: array of tstyle; v: Integer): string;
var
  i: Integer;
begin
  Result := IntToHex(v, 8) + #13#10;
  for i := low(sy) to high(sy) do
  begin
    if (v and sy[i].Value) = sy[i].Value then
    begin
      Result := Result + '  '+sy[i].Name + stringOfChar(' ',32-Length(sy[i].Name))+' ('+IntToHex(sy[i].Value, 8)+')'+#13#10;
    end;
  end;
  result := Result + #13#10;
end;

procedure Fill(memo: TMemo; h: Integer);
begin
  Memo.Lines.Add('Style = '      +st(winstyle,    GetWindowLong(h, GWL_STYLE)));
  Memo.Lines.Add('ExStyle = '    +st(winexstyle,  GetWindowLong(h, GWL_EXSTYLE)));
  Memo.Lines.Add('LangOptions = '+st(langoptions, SendMessage(h, EM_GETLANGOPTIONS, 0,0)));
  Memo.Lines.Add('TextModes =   '+st(textmodes, SendMessage(h, EM_GETTEXTMODE, 0,0)));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  Fill(Memo1, RichEdit1.Handle);
//  Fill(Memo1, rt.Handle);
  Fill(Memo2, $230242);
end;

procedure TForm1.RichEdit1Change(Sender: TObject);
begin
  ShowMessage('1');
end;

end.
