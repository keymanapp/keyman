unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation
uses Registry, JwsclSID;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var Reg: TRegistry; SID: TJwSecurityId; SidStrings: TStringlist; i: integer;
begin
Listbox1.Items.Clear;
Reg:=TRegistry.Create(KEY_QUERY_VALUE);
try
 Reg.RootKey:=HKEY_LOCAL_MACHINE;
 if Reg.OpenKey('Software\XPElevation\AllowedUsers\', false) then
  try
   SidStrings:=TStringlist.Create;
   try
    Reg.GetValueNames(SidStrings);
    for i:=0 to SidStrings.Count-1 do
     begin
      try
       SID:=TJwSecurityId.Create(SidStrings[i]);
       try
        Listbox1.Items.Add(SID.AccountName['']);
       finally
        SID.Free;
       end;
      except
      end;
     end;
   finally
    SidStrings.Free;
   end;
  finally
   Reg.CloseKey;
  end;
finally
 Reg.Free;
end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var Reg: TRegistry; s: string; SID: TJwSecurityId;
begin
if not InputQuery('New elevated user', 'Enter the user which shall be allowed to be elevated!', s) then
 exit;
try
 SID:=TJwSecurityId.Create('', s);
except
 Showmessage('The user '+s+' does not exist!');
 exit;
end;
try
 Reg:=TRegistry.Create;
 try
  Reg.RootKey:=HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('Software\XPElevation\AllowedUsers\', true) then
   try
    if Reg.ValueExists(SID.StringSID) then
     begin
      Showmessage('The user '+s+' is already allowed to be elevated!');
      abort;
     end;
    Reg.WriteString(SID.StringSID, '');
   finally
    Reg.CloseKey;
   end;
 finally
  Reg.Free;
 end;
finally
 SID.Free;
end;
  Button1.Click;
end;

procedure TForm1.Button3Click(Sender: TObject);
var SID: TJwSecurityId; Reg: TRegistry;
begin
try
 SID:=TJwSecurityId.Create('', Listbox1.Items[Listbox1.ItemIndex]);
except
end;
try
 Reg:=TRegistry.Create(KEY_SET_VALUE);
 try
  Reg.RootKey:=HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('Software\XPElevation\AllowedUsers\', false) then
   try
    Reg.DeleteValue(SID.StringSID);
    Listbox1.Items.Delete(Listbox1.ItemIndex);
   finally
    Reg.CloseKey;
   end;
 finally
  Reg.Free;
 end;
finally
 SID.Free;
end;
  Button1.Click;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Click;
end;

end.
