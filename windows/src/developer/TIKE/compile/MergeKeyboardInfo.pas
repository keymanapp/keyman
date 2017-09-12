unit MergeKeyboardInfo;

interface

uses
  compile;

type
  TMergeKeyboardInfo = class
    class function Execute(JsKmpFiles, JsonFile: string; FSilent: Boolean; FCallback: TCompilerCallback): Boolean;
  end;

implementation

{ TMergeKeyboardInfo }

class function TMergeKeyboardInfo.Execute(JsKmpFiles, JsonFile: string;
  FSilent: Boolean; FCallback: TCompilerCallback): Boolean;
begin
  Result := False;
end;

end.
