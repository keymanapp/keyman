(*
  Name:             kpbase
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      12 Mar 2010

  Modified Date:    12 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
*)
unit kpbase;

interface

uses keymancontext;

type
  TKPBase = class
  private
    FContext: TKeymanContext;
  protected
    procedure Error(ErrorCode: Cardinal);
    procedure ErrorFmt(ErrorCode: Cardinal; Args: OleVariant);
    procedure Warn(WarnCode: Cardinal);
    procedure WarnFmt(WarnCode: Cardinal; Args: OleVariant);
  public
    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
    property Context: TKeymanContext read FContext;
  end;

implementation

uses keymanapi_TLB;

{ TKPBase }

constructor TKPBase.Create(AContext: TKeymanContext);
begin
  inherited Create;
  FContext := AContext;
  { Enter Critical Section here }
end;

destructor TKPBase.Destroy;
begin
  inherited;
  { Exit Critical Section Here }
end;

procedure TKPBase.Error(ErrorCode: Cardinal);
begin
  (FContext as TKeymanContext).Errors.Add(ErrorCode, kesError);
end;

procedure TKPBase.ErrorFmt(ErrorCode: Cardinal; Args: OleVariant);
begin
  (FContext as TKeymanContext).Errors.AddFmt(ErrorCode, Args, kesError);
end;

procedure TKPBase.Warn(WarnCode: Cardinal);
begin
  (FContext as TKeymanContext).Errors.Add(WarnCode, kesWarning);
end;

procedure TKPBase.WarnFmt(WarnCode: Cardinal; Args: OleVariant);
begin
  (FContext as TKeymanContext).Errors.AddFmt(WarnCode, Args, kesWarning);
end;

end.
