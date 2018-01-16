// Tests issue #508
// See https://quality.embarcadero.com/browse/RSP-19669: The Winapi.msctf.TF_INPUTPROCESSORPROFILES
// record has a 4 byte alignment but should have an 8 byte alignment on Win64. This makes the record 
// 80 bytes instead of 88 bytes.
//
// A redefinition of the TF_INPUTPROCESSORPROFILES structure (and the interfaces that we need that 
// reference it) is in keyman_msctf.pas. Once the root issue has been addressed by Embarcadero, we 
// should remove the redefinitions and all references to them.
//
program tf_ippsize;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Win.ComObj,
  System.SysUtils,
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.msctf;

begin
  try
    writeln(Format('%s = %d', ['TF_HALTCOND', sizeof(TF_HALTCOND)]));
    writeln(Format('%s = %d', ['TF_SELECTIONSTYLE', sizeof(TF_SELECTIONSTYLE)]));
    writeln(Format('%s = %d', ['TF_SELECTION', sizeof(TF_SELECTION)]));
    writeln(Format('%s = %d', ['TS_STATUS', sizeof(TS_STATUS)]));
    writeln(Format('%s = %d', ['TF_PERSISTENT_PROPERTY_HEADER_ACP', sizeof(TF_PERSISTENT_PROPERTY_HEADER_ACP)]));
    writeln(Format('%s = %d', ['TF_LANGUAGEPROFILE', sizeof(TF_LANGUAGEPROFILE)]));
    writeln(Format('%s = %d', ['TF_INPUTPROCESSORPROFILE', sizeof(TF_INPUTPROCESSORPROFILE)]));
    writeln(Format('%s = %d', ['TF_PROPERTYVAL', sizeof(TF_PROPERTYVAL)]));
    writeln(Format('%s = %d', ['TF_PRESERVEDKEY', sizeof(TF_PRESERVEDKEY)]));
    writeln(Format('%s = %d', ['TF_DA_COLOR', sizeof(TF_DA_COLOR)]));
    writeln(Format('%s = %d', ['TF_DISPLAYATTRIBUTE', sizeof(TF_DISPLAYATTRIBUTE)]));    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
