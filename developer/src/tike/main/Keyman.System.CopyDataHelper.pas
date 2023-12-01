unit Keyman.System.CopyDataHelper;

interface

uses
  Winapi.Messages,
  Winapi.Windows;

type
  TCopyDataCommand = (CD_OPENFILE = 0);

type
  TCopyDataHelper = class sealed
  private
    // This rudimentary validation stops other apps accidentally
    // sending us WM_COPYDATA messages with bogus content.
    const CopyDataGuid = '{2F2BB073-CAD3-4AAA-8BEF-B8DB5F06D329}';
  public
    class function SendData(SenderHandle, ReceiverHandle: THandle; id: TCopyDataCommand; const data: string): Boolean;
    class function ReceiveData(var Message: TWMCopyData; var id: TCopyDataCommand; var data: string): Boolean;
  end;

implementation

uses
  System.SysUtils;

class function TCopyDataHelper.SendData(SenderHandle, ReceiverHandle: THandle; id: TCopyDataCommand; const data: string): Boolean;
var
  cds: TCopyDataStruct;
  dwResult: DWord;
  buffer: string;
begin
  buffer := CopyDataGuid + data;
  cds.dwData := DWord(id);
  cds.cbData := (buffer.Length+1) * sizeof(Char);
  cds.lpData := PChar(buffer);

  Result := SendMessageTimeout(
    ReceiverHandle, WM_COPYDATA, SenderHandle, NativeInt(@cds),
    SMTO_NORMAL, 1000, @dwResult) <> 0;
end;

class function TCopyDataHelper.ReceiveData(var Message: TWMCopyData; var id: TCopyDataCommand; var data: string): Boolean;
var
  buffer: PChar;
  bufferData: string;
begin
  Message.Result := ERROR_INVALID_DATA;
  buffer := PChar(Message.CopyDataStruct.lpData);
  bufferData := Copy(buffer, 1, Message.CopyDataStruct.cbData div sizeof(Char) - 1);
  if not bufferData.StartsWith(CopyDataGuid) then
    Exit(False);

  id := TCopyDataCommand(Message.CopyDataStruct.dwData);
  data := bufferData.Substring(CopyDataGuid.Length);
  Message.Result := ERROR_SUCCESS;
  Result := True;
end;

end.
