unit Keyman.System.Debug.DebugUtils;

interface

uses
  debugdeadkeys,
  Keyman.System.KeymanCore,
  KeymanDeveloperDebuggerMemo;

function GetContextFromMemo(Memo: TKeymanDeveloperDebuggerMemo; DeadKeys: TDebugDeadkeyInfoList; IncludeMarkers: Boolean): TArray<km_core_context_item>;

implementation

uses
  Unicode;

function GetContextFromMemo(Memo: TKeymanDeveloperDebuggerMemo; DeadKeys: TDebugDeadkeyInfoList; IncludeMarkers: Boolean): TArray<km_core_context_item>;
var
  n, i: Integer;
  ch: Char;
  dk: TDeadKeyInfo;
begin
  n := 0;
  SetLength(Result, Length(Memo.Text)+1);
  i := 1;
  while i <= Memo.SelStart + Memo.SelLength do
  begin
    ch := Memo.Text[i];
    if Uni_IsSurrogate1(ch) and (i < Length(Memo.Text)) and
      Uni_IsSurrogate2(Memo.Text[i+1]) then
    begin
      Result[n]._type := KM_CORE_CT_CHAR;
      Result[n].character := Uni_SurrogateToUTF32(ch, Memo.Text[i+1]);
      Inc(i);
    end
    else if Ord(ch) = $FFFC then
    begin
      if IncludeMarkers then
      begin
        Result[n]._type := KM_CORE_CT_MARKER;
        dk := DeadKeys.GetFromPosition(i-1);
        Assert(Assigned(dk));
        Result[n].marker := dk.Deadkey.Value;
      end
      else
      begin
        Inc(i);
        Continue;
      end;
    end
    else
    begin
      Result[n]._type := KM_CORE_CT_CHAR;
      Result[n].character := Ord(ch);
    end;
    Inc(i);
    Inc(n);
  end;

  Result[n]._type := KM_CORE_CT_END;
end;

end.
