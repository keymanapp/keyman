program delphishareddata;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Keyman.System.SharedBuffers in '..\..\..\..\engine\keyman\Keyman.System.SharedBuffers.pas',
  Keyman.System.Security in '..\..\..\..\global\delphi\general\Keyman.System.Security.pas',
  Keyman.Winapi.VersionHelpers in '..\..\..\..\global\delphi\winapi\Keyman.Winapi.VersionHelpers.pas';

var
  skb: TSelectKeyboardBuffer;
begin
  writeln('sizeof(SharedBuffer)='+IntToStr(SizeOf(TSharedBuffer)));
  writeln('sizeof(SelectKeyboardBuffer)='+IntToStr(SizeOf(TSelectKeyboardBuffer)));
  writeln('offsetof(SelectKeyboardBuffer.CLSID)='+IntToStr(Integer(@skb.CLSID)-Integer(@skb)));
  writeln('offsetof(SelectKeyboardBuffer.GUIDProfile)='+IntToStr(Integer(@skb.GUIDProfile)-Integer(@skb)));
end.
