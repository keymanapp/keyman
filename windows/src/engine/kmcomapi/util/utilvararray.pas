unit utilvararray;

interface

function FormatVariant(const FormatString: WideString; a: OleVariant): WideString;

implementation

uses Variants, SysUtils, Windows;

function FormatVariant(const FormatString: WideString; a: OleVariant): WideString;
type
  PVarRecArray = ^TVarRecArray;
  TVarRecArray  = array [1 .. MaxInt div SizeOf (TVarRec)] of TVarRec;
var
  pp: array of TVarRec;
  ap    : PVariant;
  ep, ehp : Pointer;
  i, ESize, Count  : Integer;
begin
  Count := VarArrayHighBound (a, 1) - VarArrayLowBound (a, 1) + 1;
  ESize := Count * SizeOf (Extended);
  SetLength(pp, Count);
  GetMem (ep, ESize);
  try
    if VarType (a) <> (varVariant or varArray) then
      raise EVariantError.Create ('type mismatch');
    ehp  := ep;
    ap  := VarArrayLock (a);
    try
      for i := 0 to Count-1 do
      begin
        case VarType (ap^) of
          varInteger, varSmallint :
             begin
               pp[i].VType := vtInteger;
               pp[i].VInteger := ap^;
             end;
          varSingle, varDouble, varDate :
             begin
               Extended (ehp^) := ap^;
               pp[i].VType := vtExtended;
               pp[i].VExtended := PExtended (ehp);
            end;
         varCurrency :
            begin
              Currency (ehp^) := ap^;
              pp[i].VType := vtCurrency;
              pp[i].VCurrency := PCurrency (ehp);
            end;
        else
          pp[i].VType   := vtVariant;
          pp[i].VVariant := ap;
        end;
        Inc (ap);
        ehp := Pointer (Cardinal (ehp) + SizeOf (Extended));
      end;
      Result := SysUtils.Format (FormatString, pp);
    finally
      VarArrayUnlock (a);
    end;
  finally
    FreeMem (ep, ESize);
  end;
end;

end.
