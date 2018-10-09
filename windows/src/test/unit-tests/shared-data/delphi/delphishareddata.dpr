program delphishareddata;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Keyman.System.SharedBuffers in '..\..\..\..\engine\keyman\Keyman.System.SharedBuffers.pas';

var
  skb: TSelectKeyboardBuffer;
begin
  writeln('sizeof(SharedBuffer)='+IntToStr(SizeOf(TSharedBuffer)));
  writeln('sizeof(SelectKeyboardBuffer)='+IntToStr(SizeOf(TSelectKeyboardBuffer)));
  writeln('offsetof(SelectKeyboardBuffer.CLSID)='+IntToStr(Integer(@skb.CLSID)-Integer(@skb)));
  writeln('offsetof(SelectKeyboardBuffer.GUIDProfile)='+IntToStr(Integer(@skb.GUIDProfile)-Integer(@skb)));
end.
