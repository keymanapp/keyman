unit finddefaultlang_latin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function VerifyUsefulBaseLayout(LangID: Integer): Integer;
var
  buf: array[0..240] of char;
  i: Integer;
const
  LOCALE_SSCRIPTS = $6C;
  //CLatinLocales: array[0..n] of Integer = (
begin
  Result := LangID;

  begin
    if GetLocaleInfo(LangID, LOCALE_SSCRIPTS, buf, 240) > 0 then
    begin
      if Pos('Latn', buf) = 0 then
        // The base keyboard is a non-Latin script layout.  Use US as base layout so mnemonic layouts are useful
        Result := $0409;
    end;
  end;
end;

function LocaleEnumProc(lpLocaleString: PChar): DWORD; stdcall;
const
  LOCALE_SSCRIPTS=$6C;
var
  buf:array[0..240] of char;
  s: string;
begin
  s := lpLocaleString;
  if GetLocaleInfo(StrToIntDef('$'+s, 0), LOCALE_SSCRIPTS, buf, 240) > 0 then
    s := s + ' = '+buf;
  Form1.Memo1.Lines.Add(s);
  Result := 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EnumSystemLocales(@LocaleEnumProc, LCID_SUPPORTED);
end;

end.
